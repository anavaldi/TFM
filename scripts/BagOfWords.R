# ###########################################################################
#
# BAG OF WORDS TRIP ADVISOR OPINIONS
#
# Description: Create a bag of words for pos, neg and neutral opinions
# Author: Ana Valdivia
# Date: July 2016
###########################################################################

# Load packages
library(tm)
library(SnowballC)
library(data.table)

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

rm(freq.all)
rm(word.freq.neg)
rm(word.freq.pos)
rm(word.tfidf.neg)
rm(word.tfidf.pos)

# DocumentTermMatrix for PositiveNegative
TripAdvisorPosNeg <- TripAdvisor[TripAdvisor$SentimentValue != "neutral",]
TripAdvisorFeatures <- word.tfidf(TripAdvisorPosNeg$titleopinion)$Temp
TripAdvisorFeatures <- ifelse(TripAdvisorFeatures > 0, 1, 0)
TripAdvisorFeatures <- TripAdvisorFeatures[, colnames(TripAdvisorFeatures) %in% wordNegPos500Vector]

sink("./sinks/rawColNames.txt")
colnames(TripAdvisorFeatures)
sink()

# Changing some column names
colnames(TripAdvisorFeatures)[1] <-  "euroX" #€
colnames(TripAdvisorFeatures)[69] <-  "breakX" #break
TripAdvisorFeatures <- as.data.frame(TripAdvisorFeatures)

TripAdvisorFeatures$advanceX <- TripAdvisorFeatures$advanc + TripAdvisorFeatures$advance
TripAdvisorFeatures$advanceX <- ifelse(TripAdvisorFeatures$advanceX > 1, 1, TripAdvisorFeatures$advanceX)
TripAdvisorFeatures$advanc <- NULL
TripAdvisorFeatures$advance <- NULL

TripAdvisorFeatures$amazingX <- TripAdvisorFeatures$amaz + TripAdvisorFeatures$amazing
TripAdvisorFeatures$amazingX <- ifelse(TripAdvisorFeatures$amazingX > 1, 1, TripAdvisorFeatures$amazingX)
TripAdvisorFeatures$amaz <- NULL
TripAdvisorFeatures$amazing <- NULL

TripAdvisorFeatures$andalusiaX <- TripAdvisorFeatures$andalusia + TripAdvisorFeatures$andalucia
TripAdvisorFeatures$andalusiaX <- ifelse(TripAdvisorFeatures$andalusiaX > 1, 1, TripAdvisorFeatures$andalusiaX)
TripAdvisorFeatures$andalusia <- NULL
TripAdvisorFeatures$andalucia <- NULL

TripAdvisorFeatures$architectureX <- TripAdvisorFeatures$architectur + TripAdvisorFeatures$architecture
TripAdvisorFeatures$architectureX <- ifelse(TripAdvisorFeatures$architectureX > 1, 1, TripAdvisorFeatures$architectureX)
TripAdvisorFeatures$architectur <- NULL
TripAdvisorFeatures$architecture <- NULL

TripAdvisorFeatures$areaX <- TripAdvisorFeatures$area + TripAdvisorFeatures$areas
TripAdvisorFeatures$areaX <- ifelse(TripAdvisorFeatures$areaX > 1, 1, TripAdvisorFeatures$areaX)
TripAdvisorFeatures$area <- NULL
TripAdvisorFeatures$areas <- NULL

TripAdvisorFeatures$awesomeX <- TripAdvisorFeatures$awesom + TripAdvisorFeatures$awesome
TripAdvisorFeatures$awesomeX <- ifelse(TripAdvisorFeatures$awesomeX > 1, 1, TripAdvisorFeatures$awesomeX)
TripAdvisorFeatures$awesom <- NULL
TripAdvisorFeatures$awesome <- NULL

TripAdvisorFeatures$beautifulX <- TripAdvisorFeatures$beauti + TripAdvisorFeatures$beautiful + TripAdvisorFeatures$beauty
TripAdvisorFeatures$beautifulX <- ifelse(TripAdvisorFeatures$beautifulX > 1, 1, TripAdvisorFeatures$beautifulX)
TripAdvisorFeatures$beauti <- NULL
TripAdvisorFeatures$beautiful <- NULL
TripAdvisorFeatures$beauty <- NULL

TripAdvisorFeatures$breathtakeX <- TripAdvisorFeatures$breathtak + TripAdvisorFeatures$breathtaking
TripAdvisorFeatures$breathtakeX <- ifelse(TripAdvisorFeatures$breathtakeX > 1, 1, TripAdvisorFeatures$breathtakeX)
TripAdvisorFeatures$breathtak <- NULL
TripAdvisorFeatures$breathtaking <- NULL

TripAdvisorFeatures$buildX <- TripAdvisorFeatures$build + TripAdvisorFeatures$buildings + TripAdvisorFeatures$built
TripAdvisorFeatures$buildX <- ifelse(TripAdvisorFeatures$buildX > 1, 1, TripAdvisorFeatures$buildX)
TripAdvisorFeatures$build <- NULL
TripAdvisorFeatures$buildings <- NULL
TripAdvisorFeatures$built <- NULL

TripAdvisorFeatures$cityX <- TripAdvisorFeatures$citi + TripAdvisorFeatures$city
TripAdvisorFeatures$cityX <- ifelse(TripAdvisorFeatures$cityX > 1, 1, TripAdvisorFeatures$cityX)
TripAdvisorFeatures$citi <- NULL
TripAdvisorFeatures$city <- NULL

TripAdvisorFeatures$commentX <- TripAdvisorFeatures$comment + TripAdvisorFeatures$commentari
TripAdvisorFeatures$commentX <- ifelse(TripAdvisorFeatures$commentX > 1, 1, TripAdvisorFeatures$commentX)
TripAdvisorFeatures$comment <- NULL
TripAdvisorFeatures$commentari <- NULL

TripAdvisorFeatures$complaintX <- TripAdvisorFeatures$complain + TripAdvisorFeatures$complaint
TripAdvisorFeatures$complaintX <- ifelse(TripAdvisorFeatures$complaintX > 1, 1, TripAdvisorFeatures$complaintX)
TripAdvisorFeatures$complain <- NULL
TripAdvisorFeatures$complaint <- NULL

TripAdvisorFeatures$crowdX <- TripAdvisorFeatures$crowd + TripAdvisorFeatures$crowded + TripAdvisorFeatures$crowds
TripAdvisorFeatures$crowdX <- ifelse(TripAdvisorFeatures$crowdX > 1, 1, TripAdvisorFeatures$crowdX)
TripAdvisorFeatures$crowd <- NULL
TripAdvisorFeatures$crowded <- NULL
TripAdvisorFeatures$crowds <- NULL

TripAdvisorFeatures$didnt <- NULL

TripAdvisorFeatures$disappointX <- TripAdvisorFeatures$disappoint + TripAdvisorFeatures$disappointed + TripAdvisorFeatures$disappointing + TripAdvisorFeatures$disappointment
TripAdvisorFeatures$disappointX <- ifelse(TripAdvisorFeatures$disappointX > 1, 1, TripAdvisorFeatures$disappointX)
TripAdvisorFeatures$disappoint <- NULL
TripAdvisorFeatures$disappointed <- NULL
TripAdvisorFeatures$disappointing <- NULL

TripAdvisorFeatures$dont <- NULL

TripAdvisorFeatures$due <- NULL

TripAdvisorFeatures$earlyX <- TripAdvisorFeatures$earli + TripAdvisorFeatures$earlier + TripAdvisorFeatures$early
TripAdvisorFeatures$earlyX <- ifelse(TripAdvisorFeatures$earlyX > 1, 1, TripAdvisorFeatures$earlyX)
TripAdvisorFeatures$earli <- NULL
TripAdvisorFeatures$earlier <- NULL
TripAdvisorFeatures$early <- NULL

TripAdvisorFeatures$easyX <- TripAdvisorFeatures$easi + TripAdvisorFeatures$easili
TripAdvisorFeatures$easyX <- ifelse(TripAdvisorFeatures$easyX > 1, 1, TripAdvisorFeatures$easyX)
TripAdvisorFeatures$easi <- NULL
TripAdvisorFeatures$easili <- NULL

TripAdvisorFeatures$emptyX <- TripAdvisorFeatures$empti + TripAdvisorFeatures$empty
TripAdvisorFeatures$emptyX <- ifelse(TripAdvisorFeatures$emptyX > 1, 1, TripAdvisorFeatures$emptyX)
TripAdvisorFeatures$empti <- NULL
TripAdvisorFeatures$empty <- NULL

TripAdvisorFeatures$entranceX <- TripAdvisorFeatures$entranc + TripAdvisorFeatures$entrance + TripAdvisorFeatures$entri + TripAdvisorFeatures$entry
TripAdvisorFeatures$entranceX <- ifelse(TripAdvisorFeatures$entranceX > 1, 1, TripAdvisorFeatures$entranceX)
TripAdvisorFeatures$entranc <- NULL
TripAdvisorFeatures$entrance <- NULL
TripAdvisorFeatures$entri <- NULL

TripAdvisorFeatures$experienceX <- TripAdvisorFeatures$experi + TripAdvisorFeatures$experienc + TripAdvisorFeatures$experience
TripAdvisorFeatures$experienceX <- ifelse(TripAdvisorFeatures$experienceX > 1, 1, TripAdvisorFeatures$experienceX)
TripAdvisorFeatures$experi <- NULL
TripAdvisorFeatures$experienc <- NULL
TripAdvisorFeatures$experience <- NULL

TripAdvisorFeatures$fantasticX <- TripAdvisorFeatures$fantast + TripAdvisorFeatures$fantastic
TripAdvisorFeatures$fantasticX <- ifelse(TripAdvisorFeatures$fantasticX > 1, 1, TripAdvisorFeatures$fantasticX)
TripAdvisorFeatures$fantast <- NULL
TripAdvisorFeatures$fantastic <- NULL

TripAdvisorFeatures$feelX <- TripAdvisorFeatures$feel + TripAdvisorFeatures$felt
TripAdvisorFeatures$feelX <- ifelse(TripAdvisorFeatures$feelX > 1, 1, TripAdvisorFeatures$feelX)
TripAdvisorFeatures$feel <- NULL
TripAdvisorFeatures$felt <- NULL

TripAdvisorFeatures$fortX <- TripAdvisorFeatures$fort + TripAdvisorFeatures$fortress
TripAdvisorFeatures$fortX <- ifelse(TripAdvisorFeatures$fortX > 1, 1, TripAdvisorFeatures$fortX)
TripAdvisorFeatures$fort <- NULL
TripAdvisorFeatures$fortress <- NULL

TripAdvisorFeatures$fountainX <- TripAdvisorFeatures$fountain + TripAdvisorFeatures$fountains
TripAdvisorFeatures$fountainX <- ifelse(TripAdvisorFeatures$fountainX > 1, 1, TripAdvisorFeatures$fountainX)
TripAdvisorFeatures$fountain <- NULL
TripAdvisorFeatures$fountains <- NULL

TripAdvisorFeatures$fullX <- TripAdvisorFeatures$full + TripAdvisorFeatures$fulli
TripAdvisorFeatures$fullX <- ifelse(TripAdvisorFeatures$fullX > 1, 1, TripAdvisorFeatures$fullX)
TripAdvisorFeatures$full <- NULL
TripAdvisorFeatures$fulli <- NULL

TripAdvisorFeatures$gardenX <- TripAdvisorFeatures$garden + TripAdvisorFeatures$gardens
TripAdvisorFeatures$gardenX <- ifelse(TripAdvisorFeatures$gardenX > 1, 1, TripAdvisorFeatures$gardenX)
TripAdvisorFeatures$garden <- NULL
TripAdvisorFeatures$gardens <- NULL

TripAdvisorFeatures$generalifeX <- TripAdvisorFeatures$generalif + TripAdvisorFeatures$generalife
TripAdvisorFeatures$generalifeX <- ifelse(TripAdvisorFeatures$generalifeX > 1, 1, TripAdvisorFeatures$generalifeX)
TripAdvisorFeatures$generalif <- NULL
TripAdvisorFeatures$generalife <- NULL

TripAdvisorFeatures$granadaX <- TripAdvisorFeatures$granada + TripAdvisorFeatures$grenada
TripAdvisorFeatures$granadaX <- ifelse(TripAdvisorFeatures$granadaX > 1, 1, TripAdvisorFeatures$granadaX)
TripAdvisorFeatures$granada <- NULL
TripAdvisorFeatures$grenada <- NULL

TripAdvisorFeatures$groundX <- TripAdvisorFeatures$ground + TripAdvisorFeatures$grounds
TripAdvisorFeatures$groundX <- ifelse(TripAdvisorFeatures$groundX > 1, 1, TripAdvisorFeatures$groundX)
TripAdvisorFeatures$ground <- NULL
TripAdvisorFeatures$grounds <- NULL

TripAdvisorFeatures$guideX <- TripAdvisorFeatures$guid + TripAdvisorFeatures$guide + TripAdvisorFeatures$guides
TripAdvisorFeatures$guideX <- ifelse(TripAdvisorFeatures$guideX > 1, 1, TripAdvisorFeatures$guideX)
TripAdvisorFeatures$guid <- NULL
TripAdvisorFeatures$guide <- NULL
TripAdvisorFeatures$guides <- NULL

TripAdvisorFeatures$hearX <- TripAdvisorFeatures$hear + TripAdvisorFeatures$heard
TripAdvisorFeatures$hearX <- ifelse(TripAdvisorFeatures$hearX > 1, 1, TripAdvisorFeatures$hearX)
TripAdvisorFeatures$hear <- NULL
TripAdvisorFeatures$heard <- NULL

TripAdvisorFeatures$historyX <- TripAdvisorFeatures$histor + TripAdvisorFeatures$histori + TripAdvisorFeatures$history
TripAdvisorFeatures$historyX <- ifelse(TripAdvisorFeatures$historyX > 1, 1, TripAdvisorFeatures$historyX)
TripAdvisorFeatures$histor <- NULL
TripAdvisorFeatures$histori <- NULL
TripAdvisorFeatures$history <- NULL

TripAdvisorFeatures$hourX <- TripAdvisorFeatures$hour + TripAdvisorFeatures$hours
TripAdvisorFeatures$hourX <- ifelse(TripAdvisorFeatures$hourX > 1, 1, TripAdvisorFeatures$hourX)
TripAdvisorFeatures$hour <- NULL
TripAdvisorFeatures$hours <- NULL

TripAdvisorFeatures$impressX <- TripAdvisorFeatures$impress + TripAdvisorFeatures$impressive
TripAdvisorFeatures$impressX <- ifelse(TripAdvisorFeatures$impressX > 1, 1, TripAdvisorFeatures$impressX)
TripAdvisorFeatures$impress <- NULL
TripAdvisorFeatures$impressive <- NULL

TripAdvisorFeatures$incredibleX <- TripAdvisorFeatures$incred + TripAdvisorFeatures$incredible
TripAdvisorFeatures$incredibleX <- ifelse(TripAdvisorFeatures$incredibleX > 1, 1, TripAdvisorFeatures$incredibleX)
TripAdvisorFeatures$incred <- NULL
TripAdvisorFeatures$incredible <- NULL

TripAdvisorFeatures$infoX <- TripAdvisorFeatures$info + TripAdvisorFeatures$inform
TripAdvisorFeatures$infoX <- ifelse(TripAdvisorFeatures$infoX > 1, 1, TripAdvisorFeatures$infoX)
TripAdvisorFeatures$info <- NULL
TripAdvisorFeatures$inform <- NULL

TripAdvisorFeatures$insideX <- TripAdvisorFeatures$insid + TripAdvisorFeatures$inside
TripAdvisorFeatures$insideX <- ifelse(TripAdvisorFeatures$insideX > 1, 1, TripAdvisorFeatures$insideX)
TripAdvisorFeatures$insid <- NULL
TripAdvisorFeatures$inside <- NULL

TripAdvisorFeatures$interestX <- TripAdvisorFeatures$interest + TripAdvisorFeatures$interesting
TripAdvisorFeatures$interestX <- ifelse(TripAdvisorFeatures$interestX > 1, 1, TripAdvisorFeatures$interestX)
TripAdvisorFeatures$interest <- NULL
TripAdvisorFeatures$interesting <- NULL

TripAdvisorFeatures$knowX <- TripAdvisorFeatures$knew + TripAdvisorFeatures$knowledg
TripAdvisorFeatures$knowX <- ifelse(TripAdvisorFeatures$knowX > 1, 1, TripAdvisorFeatures$knowX)
TripAdvisorFeatures$knew <- NULL
TripAdvisorFeatures$knowledg <- NULL

TripAdvisorFeatures$longX <- TripAdvisorFeatures$long + TripAdvisorFeatures$longer
TripAdvisorFeatures$longX <- ifelse(TripAdvisorFeatures$longX > 1, 1, TripAdvisorFeatures$longX)
TripAdvisorFeatures$long <- NULL
TripAdvisorFeatures$longer <- NULL

TripAdvisorFeatures$loseX <- TripAdvisorFeatures$lose + TripAdvisorFeatures$lost
TripAdvisorFeatures$loseX <- ifelse(TripAdvisorFeatures$loseX > 1, 1, TripAdvisorFeatures$loseX)
TripAdvisorFeatures$lose <- NULL
TripAdvisorFeatures$lost <- NULL

TripAdvisorFeatures$machineX <- TripAdvisorFeatures$machin + TripAdvisorFeatures$machines
TripAdvisorFeatures$machineX <- ifelse(TripAdvisorFeatures$machineX > 1, 1, TripAdvisorFeatures$machineX)
TripAdvisorFeatures$machin <- NULL
TripAdvisorFeatures$machines <- NULL

TripAdvisorFeatures$magicX <- TripAdvisorFeatures$magic + TripAdvisorFeatures$magical
TripAdvisorFeatures$magicX <- ifelse(TripAdvisorFeatures$magicX > 1, 1, TripAdvisorFeatures$magicX)
TripAdvisorFeatures$magic <- NULL
TripAdvisorFeatures$magical <- NULL

TripAdvisorFeatures$magnificX <- TripAdvisorFeatures$magnific + TripAdvisorFeatures$magnificent
TripAdvisorFeatures$magnificX <- ifelse(TripAdvisorFeatures$magnificX > 1, 1, TripAdvisorFeatures$magnificX)
TripAdvisorFeatures$magnific <- NULL
TripAdvisorFeatures$magnificent <- NULL

TripAdvisorFeatures$makeX <- TripAdvisorFeatures$make + TripAdvisorFeatures$made
TripAdvisorFeatures$makeX <- ifelse(TripAdvisorFeatures$makeX > 1, 1, TripAdvisorFeatures$makeX)
TripAdvisorFeatures$make <- NULL
TripAdvisorFeatures$made <- NULL

TripAdvisorFeatures$minutX <- TripAdvisorFeatures$minute + TripAdvisorFeatures$minutes
TripAdvisorFeatures$minutX <- ifelse(TripAdvisorFeatures$minutX > 1, 1, TripAdvisorFeatures$minutX)
TripAdvisorFeatures$minute <- NULL
TripAdvisorFeatures$minutes <- NULL

TripAdvisorFeatures$missX <- TripAdvisorFeatures$miss + TripAdvisorFeatures$missed
TripAdvisorFeatures$missX <- ifelse(TripAdvisorFeatures$missX > 1, 1, TripAdvisorFeatures$missX)
TripAdvisorFeatures$miss <- NULL
TripAdvisorFeatures$missed <- NULL

TripAdvisorFeatures$moorX <- TripAdvisorFeatures$moor + TripAdvisorFeatures$moorish + TripAdvisorFeatures$muslim
TripAdvisorFeatures$moorX <- ifelse(TripAdvisorFeatures$moorX > 1, 1, TripAdvisorFeatures$moorX)
TripAdvisorFeatures$moor <- NULL
TripAdvisorFeatures$moorish <- NULL
TripAdvisorFeatures$muslim <- NULL

TripAdvisorFeatures$morningX <- TripAdvisorFeatures$morn + TripAdvisorFeatures$morning
TripAdvisorFeatures$morningX <- ifelse(TripAdvisorFeatures$morningX > 1, 1, TripAdvisorFeatures$morningX)
TripAdvisorFeatures$morn <- NULL
TripAdvisorFeatures$morning <- NULL

TripAdvisorFeatures$nasridX <- TripAdvisorFeatures$nasrid + TripAdvisorFeatures$nazari + TripAdvisorFeatures$nazrid
TripAdvisorFeatures$nasridX <- ifelse(TripAdvisorFeatures$nasridX > 1, 1, TripAdvisorFeatures$nasridX)
TripAdvisorFeatures$nasrid <- NULL
TripAdvisorFeatures$nazari <- NULL
TripAdvisorFeatures$nazrid <- NULL

TripAdvisorFeatures$officeX <- TripAdvisorFeatures$office + TripAdvisorFeatures$offic
TripAdvisorFeatures$officeX <- ifelse(TripAdvisorFeatures$officeX > 1, 1, TripAdvisorFeatures$officeX)
TripAdvisorFeatures$office <- NULL
TripAdvisorFeatures$offic <- NULL

TripAdvisorFeatures$onlineX <- TripAdvisorFeatures$onlin + TripAdvisorFeatures$online
TripAdvisorFeatures$onlineX <- ifelse(TripAdvisorFeatures$onlineX > 1, 1, TripAdvisorFeatures$onlineX)
TripAdvisorFeatures$onlin <- NULL
TripAdvisorFeatures$online <- NULL

TripAdvisorFeatures$organX <- TripAdvisorFeatures$organ + TripAdvisorFeatures$organis
TripAdvisorFeatures$organX <- ifelse(TripAdvisorFeatures$organX > 1, 1, TripAdvisorFeatures$organX)
TripAdvisorFeatures$organ <- NULL
TripAdvisorFeatures$organis <- NULL

TripAdvisorFeatures$palaceX <- TripAdvisorFeatures$palac + TripAdvisorFeatures$palace + TripAdvisorFeatures$palaces + TripAdvisorFeatures$palacio
TripAdvisorFeatures$palaceX <- ifelse(TripAdvisorFeatures$palaceX > 1, 1, TripAdvisorFeatures$palaceX)
TripAdvisorFeatures$palac <- NULL
TripAdvisorFeatures$palace <- NULL
TripAdvisorFeatures$palaces <- NULL

TripAdvisorFeatures$parkX <- TripAdvisorFeatures$park + TripAdvisorFeatures$parking
TripAdvisorFeatures$parkX <- ifelse(TripAdvisorFeatures$parkX > 1, 1, TripAdvisorFeatures$parkX)
TripAdvisorFeatures$park <- NULL
TripAdvisorFeatures$parking <- NULL

TripAdvisorFeatures$payX <- TripAdvisorFeatures$paid + TripAdvisorFeatures$pay + TripAdvisorFeatures$payment
TripAdvisorFeatures$payX <- ifelse(TripAdvisorFeatures$payX > 1, 1, TripAdvisorFeatures$payX)
TripAdvisorFeatures$paid <- NULL
TripAdvisorFeatures$pay <- NULL
TripAdvisorFeatures$payment <- NULL

TripAdvisorFeatures$peopleX <- TripAdvisorFeatures$peopl + TripAdvisorFeatures$people 
TripAdvisorFeatures$peopleX <- ifelse(TripAdvisorFeatures$peopleX > 1, 1, TripAdvisorFeatures$peopleX)
TripAdvisorFeatures$peopl <- NULL
TripAdvisorFeatures$people <- NULL

TripAdvisorFeatures$pre <- NULL

TripAdvisorFeatures$purchaseX <- TripAdvisorFeatures$purchas + TripAdvisorFeatures$purchase 
TripAdvisorFeatures$purchaseX <- ifelse(TripAdvisorFeatures$purchaseX > 1, 1, TripAdvisorFeatures$purchaseX)
TripAdvisorFeatures$purchas <- NULL
TripAdvisorFeatures$purchase <- NULL

TripAdvisorFeatures$queueX <- TripAdvisorFeatures$queu + TripAdvisorFeatures$queue + TripAdvisorFeatures$queues
TripAdvisorFeatures$queueX <- ifelse(TripAdvisorFeatures$queueX > 1, 1, TripAdvisorFeatures$queueX)
TripAdvisorFeatures$queu <- NULL
TripAdvisorFeatures$queue <- NULL
TripAdvisorFeatures$queues <- NULL

TripAdvisorFeatures$serviceX <- TripAdvisorFeatures$servic + TripAdvisorFeatures$service
TripAdvisorFeatures$serviceX <- ifelse(TripAdvisorFeatures$serviceX > 1, 1, TripAdvisorFeatures$serviceX)
TripAdvisorFeatures$servic <- NULL
TripAdvisorFeatures$service <- NULL

TripAdvisorFeatures$sevillaX <- TripAdvisorFeatures$sevill + TripAdvisorFeatures$seville + TripAdvisorFeatures$sevilla
TripAdvisorFeatures$sevillaX <- ifelse(TripAdvisorFeatures$sevillaX > 1, 1, TripAdvisorFeatures$sevillaX)
TripAdvisorFeatures$sevill <- NULL
TripAdvisorFeatures$seville <- NULL
TripAdvisorFeatures$sevilla <- NULL

TripAdvisorFeatures$shoeX <- TripAdvisorFeatures$shoe + TripAdvisorFeatures$shoes
TripAdvisorFeatures$shoeX <- ifelse(TripAdvisorFeatures$shoeX > 1, 1, TripAdvisorFeatures$shoeX)
TripAdvisorFeatures$shoe <- NULL
TripAdvisorFeatures$shoes <- NULL

TripAdvisorFeatures$signX <- TripAdvisorFeatures$sign + TripAdvisorFeatures$signag
TripAdvisorFeatures$signX <- ifelse(TripAdvisorFeatures$signX > 1, 1, TripAdvisorFeatures$signX)
TripAdvisorFeatures$sign <- NULL
TripAdvisorFeatures$signag <- NULL

TripAdvisorFeatures$simplX <- TripAdvisorFeatures$simpl + TripAdvisorFeatures$simpli
TripAdvisorFeatures$simplX <- ifelse(TripAdvisorFeatures$simplX > 1, 1, TripAdvisorFeatures$simplX)
TripAdvisorFeatures$simpl <- NULL
TripAdvisorFeatures$simpli <- NULL

TripAdvisorFeatures$situatX <- TripAdvisorFeatures$situat + TripAdvisorFeatures$situation
TripAdvisorFeatures$situatX <- ifelse(TripAdvisorFeatures$situatX > 1, 1, TripAdvisorFeatures$situatX)
TripAdvisorFeatures$situat <- NULL
TripAdvisorFeatures$situation <- NULL

TripAdvisorFeatures$spainX <- TripAdvisorFeatures$spain + TripAdvisorFeatures$spanish
TripAdvisorFeatures$spainX <- ifelse(TripAdvisorFeatures$spainX > 1, 1, TripAdvisorFeatures$spainX)
TripAdvisorFeatures$spain <- NULL
TripAdvisorFeatures$spanish <- NULL

TripAdvisorFeatures$spendX <- TripAdvisorFeatures$spent + TripAdvisorFeatures$spend
TripAdvisorFeatures$spendX <- ifelse(TripAdvisorFeatures$spendX > 1, 1, TripAdvisorFeatures$spendX)
TripAdvisorFeatures$spent <- NULL
TripAdvisorFeatures$spend <- NULL

TripAdvisorFeatures$speakX <- TripAdvisorFeatures$speak + TripAdvisorFeatures$spoke
TripAdvisorFeatures$speakX <- ifelse(TripAdvisorFeatures$speakX > 1, 1, TripAdvisorFeatures$speakX)
TripAdvisorFeatures$speak <- NULL
TripAdvisorFeatures$spoke <- NULL

TripAdvisorFeatures$strollX <- TripAdvisorFeatures$stroll + TripAdvisorFeatures$stroller
TripAdvisorFeatures$strollX <- ifelse(TripAdvisorFeatures$strollX > 1, 1, TripAdvisorFeatures$strollX)
TripAdvisorFeatures$stroll <- NULL
TripAdvisorFeatures$stroller <- NULL

TripAdvisorFeatures$ticketX <- TripAdvisorFeatures$ticket + TripAdvisorFeatures$tickets
TripAdvisorFeatures$ticketX <- ifelse(TripAdvisorFeatures$ticketX > 1, 1, TripAdvisorFeatures$ticketX)
TripAdvisorFeatures$ticket <- NULL
TripAdvisorFeatures$tickets <- NULL

TripAdvisorFeatures$touristX <- TripAdvisorFeatures$tourist + TripAdvisorFeatures$tourists
TripAdvisorFeatures$touristX <- ifelse(TripAdvisorFeatures$touristX > 1, 1, TripAdvisorFeatures$touristX)
TripAdvisorFeatures$tourist <- NULL
TripAdvisorFeatures$tourists <- NULL

TripAdvisorFeatures$viewX <- TripAdvisorFeatures$view + TripAdvisorFeatures$views
TripAdvisorFeatures$viewX <- ifelse(TripAdvisorFeatures$viewX > 1, 1, TripAdvisorFeatures$viewX)
TripAdvisorFeatures$view <- NULL
TripAdvisorFeatures$views <- NULL

TripAdvisorFeatures$visitX <- TripAdvisorFeatures$visit + TripAdvisorFeatures$visited + TripAdvisorFeatures$visitor
TripAdvisorFeatures$visitX <- ifelse(TripAdvisorFeatures$visitX > 1, 1, TripAdvisorFeatures$visitX)
TripAdvisorFeatures$visit <- NULL
TripAdvisorFeatures$visited <- NULL
TripAdvisorFeatures$visitors <- NULL

TripAdvisorFeatures$walkX <- TripAdvisorFeatures$walking + TripAdvisorFeatures$walk 
TripAdvisorFeatures$walkX <- ifelse(TripAdvisorFeatures$walkX > 1, 1, TripAdvisorFeatures$walkX)
TripAdvisorFeatures$walking <- NULL
TripAdvisorFeatures$walk <- NULL

TripAdvisorFeatures$wallX <- TripAdvisorFeatures$wall + TripAdvisorFeatures$walls 
TripAdvisorFeatures$wallX <- ifelse(TripAdvisorFeatures$wallX > 1, 1, TripAdvisorFeatures$wallX)
TripAdvisorFeatures$wall <- NULL
TripAdvisorFeatures$walls <- NULL

TripAdvisorFeatures <- TripAdvisorFeatures[ ,order(names(TripAdvisorFeatures))]

sink("./sinks/newColNames.txt")
colnames(TripAdvisorFeatures)
sink()


# Preparing the final set
TripAdvisorAndFeatures <- cbind(TripAdvisorPosNeg, TripAdvisorFeatures)
TripAdvisorAndFeatures$pos <- NULL
TripAdvisorAndFeatures$neg <- NULL

# Delete Neutral opinions
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentValue!="neutral",]
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[TripAdvisorAndFeatures$SentimentCoreNLP!="neutral",]

# Complete set
save(TripAdvisorAndFeatures, file="./data/TripAdvisorAndFeatures.Rdata")
write.csv(TripAdvisorAndFeatures, file="./data/TripAdvisorAndFeatures.csv", row.names = FALSE)

# Not matching
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures[!(TripAdvisorAndFeatures$SentimentValue=="positive" & TripAdvisorAndFeatures$SentimentCoreNLP=="negative"),]
TripAdvisorAndFeatures_NOTMATCHING <- TripAdvisorAndFeatures_NOTMATCHING[!(TripAdvisorAndFeatures_NOTMATCHING$SentimentValue=="negative" & TripAdvisorAndFeatures_NOTMATCHING$SentimentCoreNLP=="positive"),]

save(TripAdvisorAndFeatures_NOTMATCHING, file="./data/TripAdvisorAndFeatures_NOTMATCHING.Rdata")
write.csv(TripAdvisorAndFeatures_NOTMATCHING, file="./data/TripAdvisorAndFeatures_NOTMATCHING.csv", row.names = FALSE)
