##########################################################
#
#   Sycophants in 280 Characters - Replication Code
#   
#   02a - Analysis
#   
#   This code does two main things:
#   1) Hydrates Tweets - All Tweets directed at the media
#   2) Uses the sentiment analysis model to score those Tweets on a democratic-
#      authoritarian dimension. 
#
#   If you do not have Twitter API access and cannot hydrate Tweets you can skip
#   ahead to "02b-analysis." I've provided the data necessary to reproduce Figures 2 - 4.


#Load Libraries
library(quanteda)


#Set Working Directory (only works in R Studio - otherwise replace manually)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#load LASSO model
load("cvfit.lasso_finalmodel.RData")

#load IDs for all Tweets directed at the media (these were identified using key word searches)
load("tweetIDs_media.RData")

#Hydrating Tweets using the academictwitteR package
tweets_media <- academictwitteR::hydrate_tweets(c(tweetIDs_media$tweet_id)) #NOTE: possibly some Tweets have been deleted.





######################################
# Turning Tweets into a Quanteda object######
#
# Note: Text has to be processed in EXACTLY THE SAME WAY that the text was for the model.
# Otherwise, it won't work.
#############
# 
# Declaring the corpus - each individual tweet as a document
tweetcorp <- corpus(tweets_media, 
                    docid_field = "id",
                    text_field = "text")

# Tokenizing - removes punctuation, separators, and urls
tweet_tokens <- tokens(tolower(tweetcorp),
                       remove_numbers = F, remove_punct = T, remove_separators = T, remove_url = T)

# Removing stop words - with the exception of the Turkish negation "degil" - "is not", for now
stopwords <- stopwords("tr", source = "stopwords-iso")
notkeep <- brio::readLines("./cleaning/notkeep.txt")
stopwords <- stopwords[!(stopwords %in% notkeep)]

tweet_tokens <- tokens_remove(tweet_tokens, stopwords, verbose = TRUE)
tweet_tokens <- tokens_remove(tweet_tokens, stopwords("tr", source = "nltk"), verbose = TRUE)

#And some nonsense words
tweet_tokens <- tokens_remove(tweet_tokens, "https", verbose = TRUE)
tweet_tokens <- tokens_remove(tweet_tokens, "htt", verbose = TRUE)
tweet_tokens <- tokens_remove(tweet_tokens, "rt", verbose = TRUE)

#ngrams
tweet_tokens <- tokens_ngrams(tweet_tokens, n = 1:4)

#manual stopwords
stopwords_manual <- brio::readLines("./cleaning/stopwords_manual.txt")
tweet_tokens <- tokens_remove(tweet_tokens, stopwords_manual, verbose = TRUE)

#Document term matrix
dtm <- dfm(tweet_tokens)

# ...and look at number of features:
dtm
summary(rowSums(dtm))



###
#getting variables from model to match with new dataframe
vars <- coef(cvfit.lasso, s = "lambda.min")
coefList <- vars@Dimnames[[1]]
coefList <- coefList[coefList != "(Intercept)"] #... getting rid of intercept term

#Matching the new dataset using the pattern of the original
match_dtm <- dfm_match(dtm, coefList)

#Removing empty docs so model fitting can work
match_dtm <- dfm_subset(match_dtm, ntoken(match_dtm) > 0)

match_dtm
summary(rowSums(match_dtm))

##################################################################
####    Applying the Model                                ########

# This step gives probability scores for each Tweet text on the democratic-authoritarian dimension

################
# LASSO.
#NOTE: This may take a minute

#making predictions on the test set
set.seed(1969)
pred <- predict(cvfit.lasso,
                newx = match_dtm,
                s = "lambda.min",
                type = "response")


#converting to a dataframe
data.pred <- data.frame(pred)

#converting rows and renaming to merge
data.pred <- tibble::rownames_to_column(data.pred, var = "tweet_id")

#renaming columns
colnames(data.pred) <- c("tweet_id", "dem_auth_press")



#Loading Tweets scored by model that also includes information about political elites (esp. party, position in party)
load("tweets_scored.RData")

#dropping democratic-authoritarian score from prepared set. Keeping just party and institutional information
tweets_scored <- tweets_scored[,c("tweet_id", "author_id", "party", "position", "confirmed_advisor")]

#merging information about political elites, while maintaining scores given to hydrated Tweets
tweets_scored <- dplyr::left_join(data.pred, tweets_scored,
                                  by = "tweet_id")


# Overwriting old file using hydrated Tweets. 
# Note: It is possible there are few Tweets now because some have been deleted. 
save(tweets_scored, file = "tweets_scored.RData")


