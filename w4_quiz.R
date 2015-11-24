# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 4 | Quiz
# Joe Nguyen | 23 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))

## Question 1
# Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833.
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

# Fit (1) a random forest predictor relating the factor variable y to the remaining variables and (2) a boosted predictor using the "gbm" method. Fit these both with the train() command in the caret package.
modRf <- train(y ~ ., data = vowel.train, method = "rf")
modGbm <- train(y ~ ., data = vowel.train, method = "gbm")

# What are the accuracies for the two approaches on the test data set? What is the accuracy among the test set samples where the two methods agree?
predRf <- predict(modRf, vowel.test)
predGbm <- predict(modGbm, vowel.test)

confusionMatrix(predRf, vowel.test$y)
confusionMatrix(predGbm, vowel.test$y)

predAgree <- predRf
predAgree[which(predRf != predGbm)] <- NA
confusionMatrix(predAgree, vowel.test$y)

# Answers:
# RF accuracy = 0.6061
# GBM accuracy = 0.5238
# Agreement accuracy = 0.6355

# Choose answer from quiz:
# RF Accuracy = 0.6082
# GBM Accuracy = 0.5152
# Agreement Accuracy = 0.6361


## Question 2
# Load the Alzheimer's data using the following commands
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model. Stack the predictions together using random forests ("rf"). What is the resulting accuracy on the test set? Is it better or worse than each of the individual predictions? 
set.seed(62433)
modRf <- train(diagnosis ~ ., data = training, method = "rf")
modGbm <- train(diagnosis ~ ., data = training, method = "gbm")
modLda <- train(diagnosis ~ ., data = training, method = "lda")

predRf <- predict(modRf, testing)
predGbm <- predict(modGbm, testing)
predLda <- predict(modLda, testing)

# Stack/combine
predDf <- data.frame(predRf, predGbm, predLda, diagnosis = testing$diagnosis)
modComb <- train(diagnosis ~ ., data = predDf, method = "rf")

# Prediction on prediction data
predComb <- predict(modComb, predDf)

# Accuracies
confusionMatrix(predRf, testing$diagnosis)
confusionMatrix(predGbm, testing$diagnosis)
confusionMatrix(predLda, testing$diagnosis)
confusionMatrix(predComb, testing$diagnosis)

# RF    0.7805
# GBM   0.8049
# LDA   0.7683
# Comb  0.8171

# Choose quiz answer:
# Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.


## Question 3
# Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
# Set the seed to 233 and fit a lasso model to predict Compressive Strength. Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet). 
library(elasticnet); set.seed(233)
modLasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")

plot.enet(modLasso$finalModel, xvar = "penalty", use.color = TRUE, lwd = 5)
covNames <- names(training)
legend("topright", inset = c(0.0,-0.6), covNames,
       pch = 8, lty = 1:length(covNames), col = 1:length(covNames))

# ANSWER: Cement (black line)


## Question 4
# Load the data on the number of visitors to the instructors blog from here:
if (!dir.exists("./data")) { dir.create("./data") }
if (!file.exists("./data/gaData.csv")) {
    url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
    download.file(url, "./data/gaData.csv", method = "curl")
}
# Using the commands:
library(lubridate)  # For year() function below
dat <- read.csv("./data/gaData.csv")
training <- dat[year(dat$date) < 2012,]
testing <- dat[(year(dat$date)) > 2011,]
tstrain <- ts(training$visitsTumblr)
# Fit a model using the bats() function in the forecast package to the training time series. Then forecast this model for the remaining time points. 
library(forecast)
modBats <- bats(tstrain)
fcast <- forecast(modBats, h = length(tstest), level = 95)
tstest <- ts(testing$visitsTumblr)

plot(fcast)
lines(testing$date, tstest, col = "red")

# For how many of the testing points is the true value within the 95% prediction interval bounds?
sum(tstest < fcast$upper & tstest > fcast$lower) / length(tstest) * 100
sum(tstest <= fcast$upper & tstest >= fcast$lower) / length(tstest) * 100
# 96%


## Question 5
# Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling); library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
# Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive Strength using the default settings. Predict on the testing set. What is the RMSE? 
set.seed(325); library(e1071)
modSvm <- svm(CompressiveStrength ~ ., data = training)
predSvm <- predict(modSvm, testing)

# RMSE
# INCORRECT
sqrt( sum((predSvm - testing$CompressiveStrength)^2) )

# CORRECT (ATTEMPT 2)
sqrt( mean((predSvm - testing$CompressiveStrength)^2) )

