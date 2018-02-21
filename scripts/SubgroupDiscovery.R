# ###########################################################################
#
# Subgroup Discovery
#
# Description: Apply subgroup discovery in data in order to find patterns
# with a fixed target value.
# Author: Ana Valdivia
# Date: August 2016
###########################################################################

# Load packages
library(rsubgroup)
library(caret)
library(rpart)

# read data:
namedataset <- "TripAdvisorAndFeatures_NOTMATCHING" 
dataset <- read.csv(paste0("./data/", namedataset,".csv"))
dataset$SentimentCoreNLP <- NULL
dataset$SentimentValue <- as.factor(dataset$SentimentValue)
# delete cols
colsToDel <- c("id", "location", "rating", "date", "username","userop", "quote", "reviewnospace", "page", "titleopinion")

dataset <- dataset[,!(colnames(dataset) %in% colsToDel)] 
dataset <- as.data.frame(sapply(dataset, function(x) as.factor(as.character(x))))

tree <- ctree(SentimentValue ~ ., data = dataset,
      controls = ctree_control(maxsurrogate = 3))
plot(tree)  #default plot, some crowding with N hidden on leafs

# feature Importance
fit <- rpart(SentimentValue ~., dataset, maxdepth=30)
plot(fit)
text(fit)
# 
# RocImp <- VarImp(x = dataset[, -c(1, 2, 3, 4, 5)], y = dataset$SentimentValue)
# 
# RocImp <- filterVarImp(x = dataset[, -1], y = dataset$SentimentValue)
# sink("./sinks/RocImp.txt")
# RocImp[order(RocImp$negative, decreasing = T),]
# sink()


# discover subgroups
task <- CreateSDTask( dataset, as.target("SentimentValue", "negative"), 
                      new("SDTaskConfig", attributes=c("announc", "assum", "attitud", 
                                                       "bad", "commentX",
                                                       "complaintX", "custom",
                                                       "deni", "desk", "disappointX",
                                                       "email", "employe", "emptyX",
                                                       "guard", "ignor", "indic", "poor",
                                                       "recepit", "refund", "respons", 
                                                       "rip", "rude", "secur", "speakX",
                                                       "staff", "strang", "terribl", "upset",
                                                       "valid", "woman", "worst")))
discoveredSubgroups <- DiscoverSubgroupsByTask(task, as.df=TRUE)    

task <- CreateSDTask( dataset, as.target("SentimentValue", "negative"), 
                      new("SDTaskConfig", attributes = colnames(dataset)[6:589], minqual=70))
discoveredSubgroups <- DiscoverSubgroupsByTask(task, as.df=TRUE)  

