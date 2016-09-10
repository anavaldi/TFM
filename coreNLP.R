###########################################################################
#
# SENTIMENT ANALYSIS TRIP ADVISOR
#
# Inspired by:
# https://github.com/statsmaths/coreNLP
# https://github.com/ashukumar27/TripadvisorReviewMining
# Author: Ana Valdivia
# Date: June 2016
###########################################################################

# Download CoreNLP java library 
library(data.table)
library(devtools)
devtools::install_github("statsmaths/coreNLP")
coreNLP::downloadCoreNLP()
library(coreNLP)
# initCoreNLP("./stanford-corenlp-full-2015-01-29")
initCoreNLP("./stanford-corenlp-full-2015-12-09")

# Read the data
load(file="./data/TripAdvisorAlhambra.Rdata")

# Change name
TripAdvisor <- TripAdvisorAlhambra
rm(TripAdvisorAlhambra)

# Creating sentiment label
TripAdvisor$SentimentValue <- NA
TripAdvisor$SentimentValue <- ifelse(TripAdvisor$rating <= 2, "negative", 
                                     ifelse(TripAdvisor$rating == 3, "neutral",
                                            ifelse(TripAdvisor$rating >= 4, "positive", TripAdvisor$SentimentValue)))

# Predict sentiment with coreNLP
TripAdvisor$SentimentCoreNLP <- NA
for(i in 1:nrow(TripAdvisor)){
  print(i)
  pos <- 0
  neg <- 0
  
  opinion <- TripAdvisor$titleop[i]
  opinion.df <- getSentiment(annotateString(opinion))

  for(j in 1:nrow(opinion.df)){
    if(opinion.df$sentiment[j]=="Verypositive"){
      pos = pos + 2
    } else if(opinion.df$sentiment[j]=="Positive"){
      pos = pos + 1
    } else if(opinion.df$sentiment[j]=="Negative"){
      neg = neg + 1
    } else if(opinion.df$sentiment[j]=="Verynegative"){
      neg = neg + 2
    }
  }
  
  TripAdvisor$pos[i] <- pos
  TripAdvisor$neg[i] <- neg

}

TripAdvisor$SentimentCoreNLP <- ifelse(TripAdvisor$pos > TripAdvisor$neg, "positive", 
                                    ifelse(TripAdvisor$pos < TripAdvisor$neg, "negative", "neutral"))



write.csv(TripAdvisor, file="./data/TripAdvisorCoreNLP.csv")
save(TripAdvisor, file="./data/TripAdvisorCoreNLP.rdata")
  
# Analysing SentimentValue vs. SentimentCoreNLP

# Table
table(TripAdvisor$SentimentCoreNLP, TripAdvisor$SentimentValue)
100*(table(TripAdvisor$SentimentCoreNLP, TripAdvisor$SentimentValue)/(nrow(TripAdvisor)))

