################################################################
#
#   Sycophants in 280 Characters - by Dean Schafer
#
#   
#   Replication Code
#   01a - Hydrating Tweets and Training the Model
#
#
# Note: This step provides Tweet IDs that will need to be hydrated in order to replicate
#       the model training step from scratch. Twitter API access is necessary to run
#       this code. As of writing, it was not clear if academic research access to Twitter
#       will continue. 
#
#       I have also included the trained sentiment analysis model with this 
#       replication code. The trained model can be used to recreate all the analysis and figures
#       from the paper (using replication code 01b and 02b).
#
#
#       The dataframe below "handcoded_tweets" includes the Tweet ID and handcoded score 
#       used to train the sentiment analysis model. Rehydrate the Tweets in order 
#       to get the text of the Tweets.
#
#       If you don't have Twitter API access you can Skip to "01b-theModel_figure1".




#Load Libraries
library(quanteda)
library(caret)
library(glmnet)
library(ROCR)



#Set Working Directory (only works in R Studio - otherwise replace manually)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



#importing hand coded tweets (567 of them)
load("handcoded_tweets.RData")


#Hydrate Tweets using the acaddemictwitteR package
tweets <- academictwitteR::hydrate_tweets(c(handcoded_tweets$tweet_id)) #possibly some Tweets have been deleted.

#Merging text back in
text <- tweets[,c("id", "text")]
text_code <- dplyr::left_join(handcoded_tweets, text, by = c("tweet_id" = "id"))

#dropping any missing tweets
text_code <- text_code[!is.na(text_code$text),]


######################################
# Turning into a quanteda object######
#############
# Processing
# Declaring the corpus - each individual tweet as a document
tweetcorp <- corpus(text_code, 
                    docid_field = "tweet_id",
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

#Removing a few more tokens
tweet_tokens <- tokens_remove(tweet_tokens, "https", verbose = TRUE)
tweet_tokens <- tokens_remove(tweet_tokens, "htt", verbose = TRUE)
tweet_tokens <- tokens_remove(tweet_tokens, "rt", verbose = TRUE)

#making ngrams
tweet_tokens <- tokens_ngrams(tweet_tokens, n = 1:4)

#Removing a list of stopwords that I put together manually
stopwords_manual <- brio::readLines("./cleaning/stopwords_manual.txt")
tweet_tokens <- tokens_remove(tweet_tokens, stopwords_manual, verbose = TRUE)


#Making into a Document term matrix
dtm <- dfm(tweet_tokens)


# ...and look at number of features:
dtm
summary(rowSums(dtm))



# pull out the document level covariates:
tweet_features <- docvars(dtm)





###########################################
####    Training the Model        #########


#Pulling out outcome variable
outcomes <- tweet_features$dichotomous


#Partitioning data
set.seed(42)
trainIndex <- createDataPartition(outcomes, # Vector of outcomes
                                  p = 0.75, # % of data going into training set
                                  list = FALSE, # Results be in a list
                                  times = 1) # Number of partitions


# Pull out the first column as a vector.
trainIndex <- trainIndex[,1]
class(trainIndex)

# Splitting data into testing and training set
train <- dtm[trainIndex, ]
test <- dtm[-trainIndex, ]

# Creating vectors for the outcome variable for both testing and training sets.
train.label  <- outcomes[trainIndex]
test.label   <- outcomes[-trainIndex]




################
# time to LASSO.
#NOTE: This may take a minute
set.seed(1999)
cvfit.lasso.T <- cv.glmnet(x = train,
                         y = train.label,
                         family = "binomial",
                         type.measure = "class",
                         alpha = 0.7) 

# save the model when it's maximized
save(cvfit.lasso.T, test, test.label, train.label, file = "training.RData")












