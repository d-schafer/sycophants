# sycophants

Replication materials for "Sycophants in 280 Characters", Party Politics.
------------------------------------------------------------------------------------------------
Manuscript can be found here: https://www.deanschafer.com/publications

<br>
Software

Analyses were carried out using R version 4.2.1 for Windows (x86_64-w64-mingw32/x64 (64-bit))


<br>


Data

handcoded_tweets.RData  -  Tweet IDs for set of Tweets used to train the supervised model

training.RData  -  For replicating training/testing process described in appendix. Includes a model and training/testing set from the original analysis

cvfit.lass_finalmodel.RData  -  this is the sentiment analysis model used for the analysis in the manuscript.

tweetIDs_media.RData  -  Tweet IDS for all Tweets directed at the media by political elites in this study.

tweets_scored.RData  -  scores for all Tweets directed at the media. Includes party and political position information. Necessary to replicate figures 2 through 4.

[folder] Data  -  contains two datasets: Chapel Hill Expert Survey and the V-Party Dataset.

[folder] dict  -  contains word lists for English translations for democratic-authoritarian words in Figure 1.

[folder] cleaning  -  contains word lists for manually created set of stopwords.



<br>


R Code files

01a-hydrateTweets_training.R  -  Code to rehydrate Tweets used to train the model. If you don't TwitterAPI access, this step can be skipped.

01b-testTrainmodel  -  Code to train model and evaluate its accuracy and Area Under the Curve (AUC)

01c-examineModel_figure1  -  Code to replicate Figure 1. This helps us interpret the model.

02a-hydrateTweets_analysis  -  Code to rehydrate all Tweets directed against the media. If you are replicating the sentiment analysis model training process, this step also applies to final model to the full set of Tweets to score them on the democratic-authoritarian dimension.

02b-analysis - Code to conduct the main analysis in Party Politics. Replicates Figures 2 through 4.
