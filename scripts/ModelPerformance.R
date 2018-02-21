# ###########################################################################
#
# SPLIT DATA 
#
# Description: Split data into train and test and balanced data sets
# Author: Ana Valdivia
# Date: August 2016
###########################################################################

# Load packages
library(data.table)
library(caret)
library(randomForest)
library(plyr)
library(xgboost)
library(RWeka)
library(kernlab)
library(partykit)
library(unbalanced)
library(doParallel)
library(e1071) #nb


#registerDoParallel(detectCores())
options(mc.cores = detectCores())


# set to analyse:
TripAdvisordatasets <- c("TripAdvisorAndFeatures", "TripAdvisorAndFeatures_NOTMATCHING", 
                         "TripAdvisorAndFeaturesBigram", "TripAdvisorAndFeaturesBigram_NOTMATCHING", 
                         "TripAdvisorAndFeatures_CoreNLP", "TripAdvisorAndFeaturesBigram_CoreNLP")

TripAdvisordatasets <- c("TripAdvisorAndFeatures_CoreNLP")
# unbalanced:
unbalancedV <- c("y:30", "y:20", "y:10", "y:5", "y:1", "n")
unbalancedV <- c("n")
# model:
modelV <- c("xgboost", "svm", "c45")
# modelV <- c("xgboost", "svm", "nb", "rf", "c45")


for(n in 1:length(TripAdvisordatasets)){
  for(m in 1:length(unbalancedV)){
    for(o in 1:length(modelV)){
        set <- TripAdvisordatasets[n]
        unbalanced <- strsplit(unbalancedV[m], ":")[[1]][1]
        if( unbalanced == "y"){
          ir <- as.numeric(strsplit(unbalancedV[m], ":")[[1]][2])
        }else{
          ir <- 0
        }

        model <- modelV[o]
        
        print(set)
        set
        print(unbalanced)
        unbalanced
        print(ir)
        print(model)
        model
              
        TripAdvisorFeaturesTEST <- read.csv(paste0("./data/", "TEST", "_", TripAdvisordatasets[n], ".csv"))
        IDTripAdvisorFeaturesTEST <- read.csv(paste0("./data/", "IDTEST", "_", TripAdvisordatasets[n], ".csv"))
        setnames(IDTripAdvisorFeaturesTEST, old="x", new="IDTripAdvisorFeaturesTEST")
        LABELTripAdvisorFeaturesTEST <- read.csv(paste0("./data/", "LABELTEST", "_", TripAdvisordatasets[n], ".csv"))
        LABELTripAdvisorFeaturesTEST$x <- as.factor(LABELTripAdvisorFeaturesTEST$x)
        setnames(LABELTripAdvisorFeaturesTEST, old="x", new="LABELTripAdvisorFeaturesTEST")
        
        
        if(unbalancedV[m] == "n"){
          TripAdvisorFeaturesTRAIN <- read.csv(paste0("./data/", "TRAIN", "_", TripAdvisordatasets[n], "_", unbalanced, ".csv"))
        }else{
          TripAdvisorFeaturesTRAIN <- read.csv(paste0("./data/", "TRAIN", "_", TripAdvisordatasets[n], "_", unbalanced, round(ir), ".csv"))
        }
        
        TripAdvisorFeaturesTRAIN$SentimentValue <- as.factor(TripAdvisorFeaturesTRAIN$SentimentValue)
        
        # MODELS
        
        # Learn XGBOOST
        if(model=="xgboost"){
          control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                                  summaryFunction = twoClassSummary, allowParallel = TRUE)
          set.seed(15)
          time1 <- Sys.time()
          xgbGrid <- expand.grid(
            nrounds = 1,
            eta = 0.3,
            max_depth = 5,
            gamma = 0,
            colsample_bytree=1, 
            min_child_weight=1)
          
          xgbGrid2 <- expand.grid(
            nrounds = 2,
            eta = 0.3,
            max_depth = 6,
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
          ModelResults <- caret::train(x=predictors,
                                       y=label,
                                       method="xgbTree",
                                       trControl=control,
                                       tuneGrid=xgbGrid2,
                                       metric="ROC")
          time2 <- Sys.time()
        }else if(model=="svm"){
          control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                                  summaryFunction = twoClassSummary, allowParallel = TRUE)
          
          grid <- expand.grid(C = c(0.75))
          
          set.seed(15)
          time1 <- Sys.time()
          ModelResults <- caret::train(SentimentValue ~ .,
                                       data=TripAdvisorFeaturesTRAIN,
                                       method="svmLinear",
                                       trControl=control,
                                       tuneGrid = grid,
                                       metric="ROC")
          time2 <- Sys.time()
        }else if(model=="nb"){
          nearZeroVarTrain <- nearZeroVar(TripAdvisorFeaturesTRAIN, saveMetrics = TRUE)
          nearZeroVarTrain <- nearZeroVarTrain[-1,]
          colnamesToDelete <- rownames(nearZeroVarTrain[nearZeroVarTrain$nzv==TRUE,])
          TripAdvisorFeaturesTRAIN <- TripAdvisorFeaturesTRAIN[,!(colnames(TripAdvisorFeaturesTRAIN) %in% colnamesToDelete)]
          
          control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                                  summaryFunction = twoClassSummary, allowParallel = TRUE)
          set.seed(15)
          time1 <- Sys.time()
          ModelResults <- caret::train(SentimentValue ~ ., 
                                       data=TripAdvisorFeaturesTRAIN,
                                       method = "nb",
                                       trControl = control,
                                       metric = "ROC")
          time2 <- Sys.time()
        }else if(model=="rf"){
          control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                                  summaryFunction = twoClassSummary, allowParallel = TRUE)
          
          set.seed(15)
          time1 <- Sys.time()
          ModelResults<- caret::train(SentimentValue ~ ., data=TripAdvisorFeaturesTRAIN, 
                            method="rf",
                            trControl=control,
                            metric="ROC")
          time2 <- Sys.time()
          
        }
        else if(model=="c45"){
          control <- trainControl(method="cv", number=5, classProbs = TRUE, 
                                  summaryFunction = twoClassSummary, allowParallel = TRUE)
          
          grid <- expand.grid(C = c(0.25))
          
          set.seed(15)
          time1 <- Sys.time()
          ModelResults<- caret::train(SentimentValue ~ ., data=TripAdvisorFeaturesTRAIN, 
                                      method="J48",
                                      trControl=control,
                                      tuneGrid = grid,
                                      metric="ROC")
          time2 <- Sys.time()
        }
        
        
        # Print Results
        if(unbalanced=="n"){
          pathResults <- paste0("./results/", model, "_",set ,".txt")
        }else{
          pathResults <- paste0("./results/", model, "_",set ,"_ir",ir,".txt")
        }
        
        sink(pathResults)
        print(paste0("Model: ", model))
        print(paste0("DataSet: ", set))
        if(unbalanced=="n"){
          "Unbalanced: No"
        }else{
          "Unbalanced: Yes"
          # print(paste0("oldIR: ", rawIR))
          print(paste0("newIR: ", ir))
        }
        print(paste0("Number of train instances: ", nrow(TripAdvisorFeaturesTRAIN)))
        print("Model summary: ")
        print(ModelResults)
        
        confMatrix <- confusionMatrix(ModelResults)
        
        print("ConfusionMatrix: ")
        print(confMatrix)
        
        confTable <- (nrow(TripAdvisorFeaturesTRAIN)/100)*(confMatrix$table)
        # Precision: tp/(tp+fp):
        precision <- confTable[2,2]/sum(confTable[2,1:2])
        
        # Recall: tp/(tp + fn):
        recall <- confTable[2,2]/sum(confTable[1:2,2])
        
        # F-Score: 2 * precision * recall /(precision + recall):
        fscore <- 2 * precision * recall /(precision + recall)
        
        # G-measure: sqrt(precision*recall)
        gmeasure <- sqrt(precision * recall)
        
        print(paste0("FscoreTRAIN: ", fscore))
        print(paste0("GmeasureTRAIN: ", gmeasure))
        
        print(paste0("CompTime:", time2-time1))
        
        
        # Prediction
        for(i in 1:ncol(TripAdvisorFeaturesTEST)){
          TripAdvisorFeaturesTEST[,i] <- as.numeric(as.character(TripAdvisorFeaturesTEST[,i]))
        }
        
        
        ModelResults_pred <- predict(ModelResults, TripAdvisorFeaturesTEST, type="prob")
        # print(confusionMatrix(xfbResults_pred[,2], LABELTripAdvisorFeaturesTEST))
        TripAdvisorPrediction <- data.frame(IDTripAdvisorFeaturesTEST , LABELTripAdvisorFeaturesTEST, ModelResults_pred[,2]) 
        setnames(TripAdvisorPrediction, old=c("IDTripAdvisorFeaturesTEST", "LABELTripAdvisorFeaturesTEST", "ModelResults_pred...2."),
                 new=c("id", "SentimentValue", "ProbSentiment"))
        TripAdvisorPrediction$SentimentPred <- ifelse(TripAdvisorPrediction$ProbSentiment > 0.5, "positive", "negative")
        
        # Print confusion matrix test
        confTableTEST <- table(TripAdvisorPrediction$SentimentPred, TripAdvisorPrediction$SentimentValue)
        print("ConfMatrix TEST: ")
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
        
        sink()
    }
  }
}


  
  