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
require(textreuse)
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

# # Function for bag of words
# 
# # FREQUENCY
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
                                      use.names = FALSE)

word.freq <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(tokenize = BigramTokenizer, stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
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
# word.freq.neutral <- word.freq(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "neutral"])

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
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
                                      use.names = FALSE)
word.tfidf <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(tokenize = BigramTokenizer, stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
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
#word.tfidf.neutral <- word.tfidf(TripAdvisor$titleopinion[TripAdvisor$SentimentValue == "neutral"])$Freq

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

# Merge FREQ and TFIDF
wordNeg <- merge(word.tfidf.neg, word.freq.neg, by="word", all = TRUE)
wordPos <- merge(word.tfidf.pos, word.freq.pos, by="word", all = TRUE)

setnames(wordNeg, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))
setnames(wordPos, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))

# Delete STOPWORDS bi-grams
deleteSTOPWords <- function(wordSent){
  numRows <- nrow(wordSent)
  delRows <- c()
  for(i in 1:numRows){
    if(sapply(strsplit(as.character(wordSent$word[i]), " "), length) == 2){
      word1 <- strsplit(as.character(wordSent$word[i]), " ")[[1]][1]
      word2 <- strsplit(as.character(wordSent$word[i]), " ")[[1]][2]
      if(word1 == "" | word2 == ""){
        delRows <- c(delRows, i)
      } else if(sum(grepl(word1, stopwords("SMART"))) > 0  & sum(grepl(word2, stopwords("SMART"))) > 0){
        delRows <- c(delRows, i)
      }
    }
    else{
      delRows <- c(delRows, i)
    }
    
  }
  return(delRows)
}


wordNeg <- wordNeg[-deleteSTOPWords(wordNeg),]
wordPos <- wordPos[-deleteSTOPWords(wordPos),]

# Delte some sets
rm(word.freq.neg)
rm(word.tfidf.neg) 
rm(word.freq.pos) 
rm(word.tfidf.pos)
rm(freq.all)

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

# Delete more stopWords
wordNegPos500 <- wordNegPos500[!(wordNegPos500$word=="a lot"),]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="lot of",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="lots of",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="a bit",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="you dont",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="we didnt",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="due to",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="i didnt",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="lot to",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="we couldnt",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="i wouldnt",]
wordNegPos500 <- wordNegPos500[!wordNegPos500$word=="you wont",]
wordNegPos500Vector <- wordNegPos500$word 

# plot wordcloud
#NEG


wordcloud(wordNeg$word, wordNeg$tfidf, max.words=100, scale=c(2.5, 0.5), random.order=FALSE, 
          rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(5, "Set1"))

#POS

wordcloud(wordPos$word, wordPos$tfidf, max.words=100, scale=c(2.5,0.5), random.order=FALSE, 
          rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"))
