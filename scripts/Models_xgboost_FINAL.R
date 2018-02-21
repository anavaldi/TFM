# ###########################################################################
#
# MODELS FOR SENTIMENT ANALYSIS TRIP ADVISOR OPINIONS
#
# Description: Create a bag of words for pos, neg and neutral opinions
# Author: Ana Valdivia
# Date: August 2016
###########################################################################

# Load packages
library(caret)
library(randomForest)
library(plyr)
library(data.table)
library(xgboost)
library(unbalanced)
library(doParallel)

set.seed(15)

#registerDoParallel(detectCores())
options(mc.cores = detectCores())

# Load data

# complete
# TripAdvisorAndFeatures <- read.csv("./data/TripAdvisorAndFeatures_20160630.csv")

# not matching
# TripAdvisorAndFeatures <- read.csv("./data/TripAdvisorAndFeatures_20160630_!NOTMATCH.csv")

# complete bigrams
# TripAdvisorAndFeatures <- read.csv("./data/TripAdvisorAndFeaturesBigrams.csv")
# TripAdvisorAndFeatures$SentimentCoreNLP <- NULL
# TripAdvisorAndFeatures$pos <- NULL
# TripAdvisorAndFeatures$neg <- NULL

# not matching bigrams
TripAdvisorAndFeatures <- read.csv("./data/TripAdvisorAndFeaturesBigrams.csv")
TripAdvisorAndFeatures$pos <- NULL
TripAdvisorAndFeatures$neg <- NULL
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[!(TripAdvisorAndFeatures$SentimentValue=="positive" & TripAdvisorAndFeatures$SentimentCoreNLP=="negative"),]
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[!(TripAdvisorAndFeatures$SentimentValue=="negative" & TripAdvisorAndFeatures$SentimentCoreNLP=="positive"),]
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[!(TripAdvisorAndFeatures$SentimentCoreNLP=="neutral"),]
TripAdvisorAndFeatures$SentimentCoreNLP <- NULL

# delete cols
TripAdvisorAndFeatures <- TripAdvisorAndFeatures[,!(colnames(TripAdvisorAndFeatures)%in% c("username", "location", "userop", "quote", "rating", "date", "reviewnospace", "page", "titleopinion"))] 

TripAdvisorAndFeatures$SentimentValue <- as.factor(TripAdvisorAndFeatures$SentimentValue)

# Split data set
## 75% of the sample size
sampleSize <- floor(0.75 * nrow(TripAdvisorAndFeatures))

## set the seed to make your partition reproductible
set.seed(15)
trainIndex <- sample(seq_len(nrow(TripAdvisorAndFeatures)), size = sampleSize)

TripAdvisorFeaturesTRAIN <- TripAdvisorAndFeatures[trainIndex, ]
IDTripAdvisorFeaturesTRAIN <- TripAdvisorFeaturesTRAIN$id
TripAdvisorFeaturesTRAIN$id <- NULL
TripAdvisorFeaturesTEST <- TripAdvisorAndFeatures[-trainIndex, ]
IDTripAdvisorFeaturesTEST <- TripAdvisorFeaturesTEST$id
TripAdvisorFeaturesTEST$id <- NULL
LABELTripAdvisorFeaturesTEST <- TripAdvisorFeaturesTEST$SentimentValue
TripAdvisorFeaturesTEST$SentimentValue <- NULL

# Balancing TRAIN data set
print("IR:")
print(table(TripAdvisorFeaturesTRAIN$SentimentValue)[2]/table(TripAdvisorFeaturesTRAIN$SentimentValue)[1])
minorityIndices <- (1:dim(TripAdvisorFeaturesTRAIN)[1])[TripAdvisorFeaturesTRAIN$SentimentValue == "negative"]
startLength <- dim(TripAdvisorFeaturesTRAIN)[1]
IR <- 10
toAdd <- round((1/IR)*(startLength - length(minorityIndices))-length(minorityIndices))
duplicate <- sample(minorityIndices, toAdd, replace = T)

for( j in 1:length(duplicate)){
  TripAdvisorFeaturesTRAIN <- rbind(TripAdvisorFeaturesTRAIN, TripAdvisorFeaturesTRAIN[duplicate[j],])
  TripAdvisorFeaturesTRAIN$SentimentValue[startLength+j] <- "negative"
}
print("IR:")
print(table(TripAdvisorFeaturesTRAIN$SentimentValue)[2]/table(TripAdvisorFeaturesTRAIN$SentimentValue)[1])


# Learn a Model
control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                        summaryFunction = twoClassSummary, allowParallel = TRUE)
set.seed(15)
xgbGrid <- expand.grid(
  nrounds = 1,
  eta = 0.3,
  max_depth = 5,
  gamma = 0,
  colsample_bytree=1, 
  min_child_weight=1)

predictors <- TripAdvisorFeaturesTRAIN[,-1]
for(i in 1:ncol(predictors)){
  predictors[,i] <- as.numeric(as.character(predictors[,i]))
}

label <- TripAdvisorFeaturesTRAIN$SentimentValue
set.seed(15)
time1 <- Sys.time()
xgbResults <- caret::train(x=predictors,
                           y=label,
                           method="xgbTree",
                           trControl=control,
                           tuneGrid=xgbGrid,
                           metric="ROC")
time2 <- Sys.time()
print(xgbResults)
confMatrix <- confusionMatrix(xgbResults)
print(confMatrix)

confTable <- (5847/100)*(confMatrix$table)
# Precision: tp/(tp+fp):
precision <- confTable[2,2]/sum(confTable[2,1:2])

# Recall: tp/(tp + fn):
recall <- confTable[2,2]/sum(confTable[1:2,2])

# F-Score: 2 * precision * recall /(precision + recall):
fscore <- 2 * precision * recall /(precision + recall)

# G-measure: sqrt(precision*recall)
gmeasure <- sqrt(precision * recall)

print(paste0("Fscore: ", fscore))
print(paste0("Gmeasure: ", gmeasure))

print(time2-time1)

# Prediction
for(i in 1:ncol(TripAdvisorFeaturesTEST)){
  TripAdvisorFeaturesTEST[,i] <- as.numeric(as.character(TripAdvisorFeaturesTEST[,i]))
}


xgbResults_pred = predict(xgbResults, TripAdvisorFeaturesTEST, type="prob")
# print(confusionMatrix(xfbResults_pred[,2], LABELTripAdvisorFeaturesTEST))
TripAdvisorPrediction <- data.frame(IDTripAdvisorFeaturesTEST , LABELTripAdvisorFeaturesTEST, xgbResults_pred[,2]) 
setnames(TripAdvisorPrediction, old=c("IDTripAdvisorFeaturesTEST", "LABELTripAdvisorFeaturesTEST", "xgbResults_pred...2."),
         new=c("id", "SentimentValue", "ProbSentiment"))
TripAdvisorPrediction$SentimentPred <- ifelse(TripAdvisorPrediction$ProbSentiment > 0.5, "positive", "negative")

# Print confusion matrix test
confTableTEST <- table(TripAdvisorPrediction$SentimentPred, TripAdvisorPrediction$SentimentValue)
print(table(TripAdvisorPrediction$SentimentPred, TripAdvisorPrediction$SentimentValue))
print(postResample(TripAdvisorPrediction$SentimentPred, TripAdvisorPrediction$SentimentValue))

# Precision: tp/(tp+fp):
precision <- confTableTEST[2,2]/sum(confTableTEST[2,1:2])

# Recall: tp/(tp + fn):
recall <- confTableTEST[2,2]/sum(confTableTEST[1:2,2])

# F-Score: 2 * precision * recall /(precision + recall):
fscore <- 2 * precision * recall /(precision + recall)

# G-measure: sqrt(precision*recall)
gmeasure <- sqrt(precision * recall)

print(paste0("FscoreTEST: ", fscore))
print(paste0("GmeasureTEST: ", gmeasure))

# write.csv(TripAdvisorPrediction,"results/TripAdvisorPrediction.csv", row.names=F)
