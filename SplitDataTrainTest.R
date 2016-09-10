# ###########################################################################
#
# SPLIT DATA 
#
# Description: Split data into train and test and balanced data sets
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


#registerDoParallel(detectCores())
options(mc.cores = detectCores())

# select parameters
# percentage split train/test:
percentage <- 0.75

# set to analyse:
TripAdvisordatasets <- c("TripAdvisorAndFeatures", "TripAdvisorAndFeatures_NOTMATCHING", 
                         "TripAdvisorAndFeaturesBigram", "TripAdvisorAndFeaturesBigram_NOTMATCHING", 
                         "TripAdvisorAndFeatures_CoreNLP", "TripAdvisorAndFeaturesBigram_CoreNLP")
# unbalanced:
unbalancedV <- c("y:30", "y:20",  "y:10", "y:5",  "y:1", "n")

# function
# function to split
splitTrainTest <- function(namedataset, perc, colsdel){
  # In this function SentimentValue refers to class label
  if(namedataset == "TripAdvisorAndFeatures_CoreNLP" | namedataset == "TripAdvisorAndFeaturesBigram_CoreNLP"){
    namedataset <- strsplit(namedataset, split='_', fixed=TRUE)[[1]][1]
    dataset <- read.csv(paste0("./data/", namedataset,".csv"))
    setnames(dataset, old = c("SentimentValue", "SentimentCoreNLP"), new = c("SentimentCoreNLP", "SentimentValue"))
  }else{
    dataset <- read.csv(paste0("./data/", namedataset,".csv"))
    
  }
  dataset$SentimentCoreNLP <- NULL
  dataset$SentimentValue <- as.factor(dataset$SentimentValue)
  # delete cols
  dataset <- dataset[,!(colnames(dataset) %in% colsdel)] 
  
  sampleSize <- floor(percentage * nrow(dataset))
  
  ## set the seed to make your partition reproductible
  set.seed(15)
  trainIndex <- sample(seq_len(nrow(dataset)), size = sampleSize)
  
  datasetTRAIN <- dataset[trainIndex, ]
  IDdatasetTRAIN <- dataset$id
  datasetTRAIN$id <- NULL
  datasetTEST <- dataset[-trainIndex, ]
  IDdatasetTEST <- datasetTEST$id
  datasetTEST$id <- NULL
  LABELdatasetTEST <- datasetTEST$SentimentValue
  datasetTEST$SentimentValue <- NULL
  list(train = datasetTRAIN, test = datasetTEST, 
       labelTest = LABELdatasetTEST, idTest = IDdatasetTEST)
}

# Learn Model & PrintResults

for(n in 1:length(TripAdvisordatasets)){
  for(m in 1:length(unbalancedV)){
        set <- TripAdvisordatasets[n]
        unbalanced <- strsplit(unbalancedV[m], ":")[[1]][1]
        if( unbalanced == "y"){
          ir <- as.numeric(strsplit(unbalancedV[m], ":")[[1]][2])
        }else{
          ir <- 0
        }
        
        print(set)
        print(unbalanced)
        print(ir)
        
        colsToDel <- c("username", "location", "userop", "quote", "rating", "date", "reviewnospace", "page", "titleopinion")
        
        TripAdvisorComplete <- splitTrainTest(set, percentage, colsToDel)
        TripAdvisorFeaturesTRAIN <- TripAdvisorComplete$train
        TripAdvisorFeaturesTEST <- TripAdvisorComplete$test
        IDTripAdvisorFeaturesTEST <- TripAdvisorComplete$idTest
        LABELTripAdvisorFeaturesTEST <- TripAdvisorComplete$labelTest
        
        
        # BALANCE DATA SET
        if ( (unbalanced == "y" & ir < 30) | (unbalanced == "y" & ir == 30 & (set == "TripAdvisorAndFeatures" | set == "TripAdvisorAndFeaturesBigram")) ){
          rawIR <- table(TripAdvisorFeaturesTRAIN$SentimentValue)[2]/table(TripAdvisorFeaturesTRAIN$SentimentValue)[1]
          minorityIndices <- (1:dim(TripAdvisorFeaturesTRAIN)[1])[TripAdvisorFeaturesTRAIN$SentimentValue == "negative"]
          startLength <- dim(TripAdvisorFeaturesTRAIN)[1]
          IR <- ir
          toAdd <- round((1/IR)*(startLength - length(minorityIndices))-length(minorityIndices))
          duplicate <- sample(minorityIndices, toAdd, replace = T)
          
          for( j in 1:length(duplicate)){
            TripAdvisorFeaturesTRAIN <- rbind(TripAdvisorFeaturesTRAIN, TripAdvisorFeaturesTRAIN[duplicate[j],])
            TripAdvisorFeaturesTRAIN$SentimentValue[startLength+j] <- "negative"
          }
          newIR <- table(TripAdvisorFeaturesTRAIN$SentimentValue)[2]/table(TripAdvisorFeaturesTRAIN$SentimentValue)[1]
        }
        
        if(unbalancedV[m] == "n"){
          write.csv(TripAdvisorFeaturesTRAIN, file = paste0("./data/", "TRAIN", "_", TripAdvisordatasets[n], "_", unbalanced, ".csv"), row.names = FALSE)
          write.csv(TripAdvisorFeaturesTEST, file = paste0("./data/", "TEST", "_", TripAdvisordatasets[n], ".csv"), row.names = FALSE)
          write.csv(IDTripAdvisorFeaturesTEST, file = paste0("./data/", "IDTEST", "_", TripAdvisordatasets[n], ".csv"), row.names = FALSE)
          write.csv(LABELTripAdvisorFeaturesTEST, file = paste0("./data/", "LABELTEST", "_", TripAdvisordatasets[n], ".csv"), row.names = FALSE)
        }else{
          write.csv(TripAdvisorFeaturesTRAIN, file = paste0("./data/", "TRAIN", "_", TripAdvisordatasets[n], "_", unbalanced, round(ir), ".csv"), row.names = FALSE)
        }
  }
}


  
  