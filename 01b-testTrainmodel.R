################################################################
#
#   Sycophants in 280 Characters - by Dean Schafer
#
#   
#   Replication Code
#   01b - Testing the Model
#
#   Includes replication code for figures in Appendix A.4
#
#
# Note: There are two options for this step:
#       1) Hydrate Tweets using 01a and save the training model and testing data
#       OR
#       2) Use the original training model and data provided with this code


#Load Libraries
library(caret)
library(glmnet)
library(ROCR)

#Set Working Directory (only works in R Studio - otherwise replace manually)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#################################################################
### Evaluations - examining the accuracy of the training model

### Load training/testing model ###
# This step either loads provided training/testing data or loads data from 01a
load("training.RData") 
#
# NOTE: original training/testing set can also be used to compare 
# with hydrated Tweets, particularly if many missing tweets
# significantly affect accuracy.


# make predictions on the test set
pred <- predict(cvfit.lasso.T,
                newx = test,
                s = "lambda.min",
                type = "response")


######

#looking at the model
plot(cvfit.lasso.T)


# select a threshold and generate predicted labels:
pred_vals <- ifelse(pred >= 0.5, 2, 1)



# Confusion matrix gives model sensitivity, specificity, precision (Pos Pred Value),
# and recall (Neg Pred Value)
confusionMatrix(table(pred_vals, test.label),positive="1")






###########################
### Area Under the Curve
### 

# Use ROCR package to plot ROC Curve
lasso.pred <- prediction(pred, test.label)
lasso.perf <- performance(lasso.pred, "tpr", "tnr")

plot(lasso.perf,
     avg = "threshold",
     colorize = TRUE,
     lwd = 1,
     main = "ROC Curve w/ Thresholds",
     print.cutoffs.at = c(.9,.8,.7,.6,.5,.4,.3,.2,.1),
     text.adj = c(-0.5, 0.5),
     text.cex = 0.5)
grid(col = "lightgray")
axis(1, at = seq(0, 1, by = 0.1))
axis(2, at = seq(0, 1, by = 0.1))
abline(v = c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x = c(0, 1), y = c(1, 0), col="black", lty="dotted")



# getting the AUC for this predictor...
auc.perf <- performance(lasso.pred,
                        measure = "auc")
auc.perf@y.values[[1]]


# and for looking at accuracy by threshold.
acc.perf <- performance(lasso.pred, measure = "acc")
plot(acc.perf)


# We can also calculate the optimal accuracy and its associated threshold:
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))








#########################################################################
##############   THE MODEL   ############################################
# Now that we've tested the accuracy of the model using a testing/training set
# we train the model using the all the data available (all handcoded scores).

#NOTE: Only run this step if you have run 01a and rehydrated the Tweets.

################
# LASSO.
#NOTE: This may take a minute
set.seed(1999)
cvfit.lasso <- cv.glmnet(x = dtm,
                         y = tweet_features$dichotomous,
                         family = "binomial",
                         type.measure = "class",
                         alpha = 0.7) 





# save the model when it's maximized
save(cvfit.lasso, file = "cvfit.lasso_finalmodel.RData")


