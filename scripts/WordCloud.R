###########################################################################
#
# WORD CLOUD TRIP ADVISOR
#
# Inspired by:
# 
# Author: Ana Valdivia
# Date: June 2016
###########################################################################

# Packages & Libraries
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(data.table)
# library(devtools)
# devtools::install_github("statsmaths/coreNLP")
# library(coreNLP)
# initCoreNLP("./stanford-corenlp-full-2015-01-29")

# Read data
# load("./data/TripAdvisorAlhambra.Rdata")
# 
# TripAdvisor_NEG <- TripAdvisorAlhambra[TripAdvisorAlhambra$rating > 3,]

# Merge all opinions
# Load data
TripAdvisor <- read.csv("./data/TripAdvisorSentiment_20160630.csv")


# Function for bag of words

# FREQUENCY
word.freq <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  # construct word frequency df
  freq.df <- colSums(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}

word.freq.pos <- word.freq(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "positive"])
word.freq.neg <- word.freq(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "negative"])
word.freq.neutral <- word.freq(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "neutral"])

word.freq.pos <- as.data.table(word.freq.pos)
word.freq.pos <- word.freq.pos[order(freq, decreasing = TRUE),]
word.freq.neg <- as.data.table(word.freq.neg)
word.freq.neg <- word.freq.neg[order(freq, decreasing = TRUE),]
# word.freq.neutral <- as.data.table(word.freq.neutral)
# word.freq.neutral <- word.freq.neutral[order(freq, decreasing = TRUE),]

# NSDI
# merge by word
freq.all <- merge(word.freq.neg, word.freq.pos, by = "word", all = T)
# clean up
freq.all$freq.x[is.na(freq.all$freq.x)] <- 0
freq.all$freq.y[is.na(freq.all$freq.y)] <- 0
# compute difference
freq.all$diff <- abs(freq.all$freq.x - freq.all$freq.y)

#smoothing term
alpha <- 150
freq.all$ndsi <- abs(freq.all$freq.x - freq.all$freq.y)/(freq.all$freq.x + freq.all$freq.y + 2*alpha)

# TF*IDF
word.tfidf <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  docTerm.df <- as.data.frame(temp.tf)
  # construct word frequency df
  freq.df <- colMeans(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  list(Freq = freq.df, Temp = docTerm.df)
}

word.tfidf.pos <- word.tfidf(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "positive"])$Freq
word.tfidf.neg <- word.tfidf(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "negative"])$Freq
# word.tfidf.neutral <- word.tfidf(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "neutral"])$Freq

word.tfidf.pos <- as.data.table(word.tfidf.pos)
word.tfidf.pos <- word.tfidf.pos[order(freq, decreasing = TRUE),]
word.tfidf.neg <- as.data.table(word.tfidf.neg)
word.tfidf.neg <- word.tfidf.neg[order(freq, decreasing = TRUE),]
# word.tfidf.neutral <- as.data.table(word.tfidf.neutral)
# word.tfidf.neutral <- word.tfidf.neutral[order(freq, decreasing = TRUE),]

# NSDI
# merge by word
freq.all <- merge(word.tfidf.neg, word.tfidf.pos, by = "word", all = TRUE)
# clean up
freq.all$freq.x[is.na(freq.all$freq.x)] <- 0
freq.all$freq.y[is.na(freq.all$freq.y)] <- 0
# compute difference
freq.all$diff <- abs(freq.all$freq.x - freq.all$freq.y)

#smoothing term
alpha <- 150
freq.all$ndsi <- abs(freq.all$freq.x - freq.all$freq.y)/(freq.all$freq.x + freq.all$freq.y + 2*alpha)

# Select tf*idf pos and neg
wordNeg <- merge(word.tfidf.neg, word.freq.neg, by="word", all = TRUE)
wordPos <- merge(word.tfidf.pos, word.freq.pos, by="word", all = TRUE)

setnames(wordNeg, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))
setnames(wordPos, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))

# Delete STOPWORDS
wordNeg <- wordNeg[!(wordNeg$word %in% stopwords("SMART"))]
wordPos <- wordPos[!(wordPos$word %in% stopwords("SMART"))]

# Order and select most 500 popular words
wordNeg <- wordNeg[order(tfidf, decreasing = TRUE),]
wordNeg500 <- wordNeg[1:500,]
wordPos <- wordPos[order(tfidf, decreasing = TRUE),]
wordPos500 <- wordPos[1:500,]

wordNegPos500 <- merge(wordNeg500, wordPos500, by = "word", all = TRUE)
setnames(wordNegPos500, old=c("tfidf.x", "freq.x", "tfidf.y", "freq.y"), 
         new=c("tfidfNeg", "freqNeg", "tfidfPos", "freqPos"))
wordNegPos500 <- wordNegPos500[order(tfidfNeg, decreasing = TRUE),]
wordNegPos500Vector <- wordNegPos500$word                

wordNegSelect <- wordNeg[!(wordNeg$word %in% wordPos$word),]
wordPosSelect <- wordPos[!(wordPos$word %in% wordNeg$word),]
wordCommonPosNeg <- wordNeg[(wordNeg$word %in% wordPos$word),]


# plot wordcloud
#NEG
wordNeg[49]$word <- "€"

wordcloud(wordNeg$word, wordNeg$tfidf, max.words=100, scale=c(2.5,0.1), random.order=FALSE, 
          rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(6, "Set1"))

#POS

wordcloud(wordPos$word, wordPos$tfidf, max.words=100, scale=c(2.5,0.1), random.order=FALSE, 
          rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"))
