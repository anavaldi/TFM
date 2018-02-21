# ###########################################################################
#
# TRIP ADVISOR OPINIONS
#
# Description: Analysing data about Trip Advisor Alhambra Opinions
# Author: Ana Valdivia
# Date: August 2016
###########################################################################

# Library and Packages
library(data.table)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

# Languages
lang <- as.factor(c("English", "Spanish", "Italian", "French", "Portuguese", "German", "Japanese", 
          "Russian", "Dutch", "Others"))
count <- c(8414, 6119, 1710, 1598, 900, 517, 449, 355, 308, 527)

language <- data.frame(lang, count)

ggplot(language, aes(x=reorder(lang, -count), y=count)) + geom_bar(stat="identity", fill="orangered1", position="dodge") + 
  labs(title="Alhambra's Language Reviews", x="", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold"))


# Traveller Rating
rate <- as.factor(c("Excellent", "Very good", "Average", "Poor", "Terrible"))
count <- c(6574, 1312, 354, 86, 88)

travelRating <- data.frame(rate, count)

ggplot(travelRating, aes(x=reorder(rate, -count), y=count)) + geom_bar(stat="identity", fill="chartreuse3", position="dodge") + 
  labs(title="Alhambra's Traveller Rating", x="", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold"))

# Traveller Type
rate <- as.factor(c("Families", "Couples", "Solo", "Business", "Friends"))
count <- c(1346, 3857, 493, 63, 1476)

travelType <- data.frame(rate, count)

ggplot(travelType, aes(x=reorder(rate, -count), y=count)) + geom_bar(stat="identity", fill="magenta3", position="dodge") + 
  labs(title="Alhambra's Traveller Type", x="", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold"))





################################################################
###############################################################



TripAdvisor <- read.csv("./data/TripAdvisorSentiment_20160630.csv")

colnames(TripAdvisor)

TripAdvisor <- as.data.table(TripAdvisor)
RateDate <- TripAdvisor[,list(avgRatings=mean(rating)), by=list(date)]
RateDate$date <- as.Date(as.character(RateDate$date), format="%Y-%m-%d")
RateDate$dateMonth <- as.factor(format(RateDate$date, "%m"))
RateDate$dateYear <- as.factor(format(RateDate$date, "%Y"))

RateDate$monthDay <- as.Date(paste0(as.character(RateDate$dateYear), 
               "-", as.character(RateDate$dateMonth), "-01"), "%Y-%m-%d")


# avgRatingsMonthly <- as.data.table(aggregate( avgRatings ~ dateMonth + dateYear , RateDate , mean))
# setnames(avgRatingsMonthly, old = "avgRatings", new = "avgRatingsMonthly")
# RateDate <- merge(RateDate, avgRatingsMonthly, by=c("dateMonth", "dateYear"))
# 
# ggplot(RateDate[date >= as.Date("2013-01-01")], aes(date, avgRatings)) + geom_line() + xlab("Month") + 
#   ylab("Monthly AVG Ratings") + scale_x_date(date_breaks = "3 month", labels=date_format("%b-%Y")) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 

ggplot(RateDate[date >= as.Date("2012-01-01")], aes(monthDay, avgRatingsMonthly)) + 
  geom_point(color="black") + geom_line(color="grey44", linetype="longdash") + 
  scale_x_date(date_breaks = "3 month", labels=date_format("%b-%Y")) + 
  labs(title="Monthly average rating Jan 2012 - Jun 2016", x="Month", y = "Rating") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold"))



ggplot(RateDate[date >= as.Date("2012-01-01")], aes(date, avgRatingsMonthly)) + geom_line() + xlab("Month") + 
  ylab("Monthly AVG Ratings") + scale_x_date(date_breaks = "6 month", labels=date_format("%b-%Y")) + 
  labs(title="Alhambra's Traveller Type", x="", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold"))


# table reviews by year
TripAdvisor$count <- 1
ReviewDay <- TripAdvisor[,list(reviewCount=sum(count)), by=list(date)]
TripAdvisor$count <- NULL
ReviewDay$date <- as.Date(as.character(ReviewDay$date), format="%Y-%m-%d")
ReviewDay$dateMonth <- as.factor(format(ReviewDay$date, "%m"))
ReviewDay$dateYear <- as.factor(format(ReviewDay$date, "%Y"))

ReviewYear <- ReviewDay[,list(reviewCount=sum(reviewCount)), by=list(dateYear)]
setnames(ReviewYear, old=c("dateYear", "reviewCount"), new=c("Year", "Total Opinions"))
xReviewYear <- xtable(ReviewYear)

# nchar
mean(nchar(as.character(TripAdvisor[SentimentValue=="positive"]$titleopinion)))
mean(nchar(as.character(TripAdvisor[SentimentValue=="neutral"]$titleopinion)))
mean(nchar(as.character(TripAdvisor[SentimentValue=="negative"]$titleopinion)))



###################################################################################

colnames(TripAdvisor)
setnames(TripAdvisor, old="SentimentPred", new="SentimentCoreNLP")

table(TripAdvisor$SentimentValue, TripAdvisor$SentimentCoreNLP)



# Barplot of positive SentimentValue rate 4
SentimentCoreNLP <- as.factor(c("positive", "neutral", "negative"))
count <- c(475, 176, 612)
SentimentCoreNLP <- factor(SentimentCoreNLP, levels=c("positive", "neutral", "negative"), ordered = T)


Sentiment <- data.frame(SentimentCoreNLP, count)

ggplot(Sentiment, aes(x=SentimentCoreNLP, y=count)) + geom_bar(stat="identity", fill="orangered1", position="dodge") + 
  labs(title="Positive SentimentValue with rate 4", x="SentimentCoreNLP", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold"))


# Barplot of positive SentimentValue rate 5
SentimentCoreNLP <- as.factor(c("positive", "neutral", "negative"))
SentimentCoreNLP <- factor(SentimentCoreNLP, levels=c("positive", "neutral", "negative"), ordered = T)
count <- c(3574, 895, 1896)

Sentiment <- data.frame(SentimentCoreNLP, count)

ggplot(Sentiment, aes(x=SentimentCoreNLP, y=count)) + geom_bar(stat="identity", fill="chartreuse3", position="dodge") + 
  labs(title="Positive SentimentValue with rate 5", x="SentimentCoreNLP", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold"))


#########################################

AucSpecTest <- read.delim("./sinks/AUCSPEC.txt")
ggplot(AucSpecTest, aes(x=AUC, y=SpecTEST)) + geom_point(aes(size = 0.5*(AUC +SpecTEST), colour=0.5*(AUC +SpecTEST))) + geom_text(aes(label=ifelse(SpecTEST>0.5,as.character(ExpNum),'')),, hjust=-2, vjust=0.5) +
  labs(title="Best Model Performance (AUC vs SpecTest)", x="AUC", y = "Specificity Test") + 
  theme(axis.text.x = element_text(hjust = 1, size=12), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold")) +
        scale_colour_gradient(low="darkorange4", high="darkorange1")+
        guides(color=guide_legend(), size = guide_legend())

Algor <- read.delim("./sinks/AllExp.txt")
layout(matrix(2, 2,2, 2, byrow = TRUE))
a <- ggplot(Algor[Algor$Features == "unigram",], aes(factor(Algorithm), AUC)) + 
  geom_boxplot(aes(fill = factor(Algorithm))) +
  scale_fill_manual(values=c("chartreuse3", "chocolate1", "magenta4")) +
  labs(title="AUC of Unigram Experiments", x="Algorithm", y = "AUC") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=12,face="bold")) 
b <- ggplot(Algor[Algor$Features == "unigram",], aes(factor(Algorithm), SpecTEST)) + 
  geom_boxplot(aes(fill = factor(Algorithm))) +
  scale_fill_manual(values=c("chartreuse3", "chocolate1", "magenta4")) +
  labs(title="SpecTEST of Unigram Experiments", x="Algorithm", y = "Specificity Test") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=12,face="bold")) 
c <- ggplot(Algor[Algor$Features == "bigram",], aes(factor(Algorithm), AUC)) + 
  geom_boxplot(aes(fill = factor(Algorithm))) +
  scale_fill_manual(values=c("chartreuse3", "chocolate1", "magenta4")) +
  labs(title="AUC of Bigram Experiments", x="Algorithm", y = "AUC") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=12,face="bold")) 
d <- ggplot(Algor[Algor$Features == "bigram",], aes(factor(Algorithm), SpecTEST)) + 
  geom_boxplot(aes(fill = factor(Algorithm))) +
  scale_fill_manual(values=c("chartreuse3", "chocolate1", "magenta4")) +
  labs(title="SpecTEST of Bigram Experiments", x="Algorithm", y = "Specificity Test") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=12,face="bold")) 
grid.arrange(a, b, c, d, nrow=2, ncol=2)
