#  Sentiment Analysis For Touristic Attractions: A Case Study On The Alhambra
###### Master Thesis Project of Ana Valdivia

![alt text](https://github.com/anavaldi/TFM/blob/master/DSC00677_2.jpg "Alhambra")

# ABSTRACT #

The development of Web 2.0 has led to an important amount of content in webpage. Users are free to express their opinions about products, places and events. This project research is aimed at introducing sentiment analysis into touristic attractions. To begin with, we scrap TripAdvisor reviews from the most touristic attraction in Spain, the Alhambra. We then create two sentiment labels: the expert sentiment which is the rate of the reviewer; and the machine sentiment which is extracted from a Natural Language Processing toolkit developed in Stanford University. After that, we build classification models so as to predict polarity sentiments. Finally, we develop a subgroup discovery method so as to extract valuable information about negative reviews.


## CODE ##

There is a brief description for each script of the code:

1. [scrapTripAdvisorLoop_anony_ENG_complete.R](https://github.com/anavaldi/TFM/blob/master/scripts/scrapTripAdvisorLoop_anony_ENG_complete.R): Scrapper for Alhambra's English reviews of TripAdvisor.

2. [scrapTripAdvisorLoop_anony_SPA_complete.R](https://github.com/anavaldi/TFM/blob/master/scripts/scrapTripAdvisorLoop_anony_SPA_complete.R): Scrapper for Alhambra's Spanish reviews of TripAdvisor.

3. [coreNLP.R](https://github.com/anavaldi/TFM/blob/master/scripts/coreNLP.R): Applying CoreNLP Sentiment Analysis method to reviews.

4. [UFSM.R](https://github.com/anavaldi/TFM/blob/master/scripts/UFSM.R): Unigram Feature Selection Method (UFSM) script.

5. [BFSM.R](https://github.com/anavaldi/TFM/blob/master/scripts/BSFM.R): Bigram Feature Selection Method (BFSM) script.

6. [WordCloud.R](https://github.com/anavaldi/TFM/blob/master/scripts/WordCloud.R): Print a wordcloud of reviews.

7. [WordCloudBigram.R](https://github.com/anavaldi/TFM/blob/master/scripts/WordCloudBigram.R): Print a wordcloud of reviews with bigrams.

8. [ReviewGraphics.R](https://github.com/anavaldi/TFM/blob/master/scripts/ReviewGraphics.R): Plots for analyzing Alhambra's TripAdvisor data (number of reviews by language, reviews per month, average ratings...).

9. [SplitDataTrainTest.R](https://github.com/anavaldi/TFM/blob/master/scripts/SplitDataTrainTest.R): Script that splits reviews for training and testing.

10. [ModelPerformance.R](https://github.com/anavaldi/TFM/blob/master/scripts/ModelPerformance.R): Different models for sentiment classification.

11. [Models_xgboost_FINAL.R](https://github.com/anavaldi/TFM/blob/master/scripts/Models_xgboost_FINAL.R): XGBoost models for sentiment classification.



License: [CC BY-NC-SA](https://creativecommons.org/licenses/by-nc-sa/4.0/)
