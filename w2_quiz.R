# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Quiz
#
# Joe Nguyen | 15 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
par(mfrow = c(1,2))


## Question 1
# Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
# Which of the following commands will create training and test sets with about 50% of the observations assigned to each? (a)
adData = data.frame(diagnosis, predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50, list = FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

dim(training); dim(testing)


## Question 2
# Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform to try to make the data more symmetric. Why would that be a poor choice for this variable?
hist(training$Superplasticizer)
hist(log(training$Superplasticizer + 1))
hist(log(training$Superplasticizer))
# Poor choice because most values are close to zero; log(x+1) = 0 anyway
# Also, log(x) turns values negative 
#
# ANSWER:
# There are values of zero so when you take the log() transform those values will be -Inf.
#
# This happens but is not true that classifiers can't handle negative values:
# The log transform produces negative values which can not be used by some classifiers.


## Question 3
# Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Find all the predictor variables in the training set that begin with IL. Perform principal components on these variables with the preProcess() function from the caret package. Calculate the number of principal components needed to capture 80% of the variance. How many are there? 
# head(training)

# Get all predictors (col variables) beginning with "IL"
require(dplyr)
name <- "IL"
features <- names(training)
features <- features[substring(features, 1, nchar(name)) == name]

trainingIL <- training[, features]
names(trainingIL)

# PCA -> 7 PCs required to capture 80% of variance
preProcess(trainingIL, method = "pca", thresh = 0.8)

# extra PCA
prComp <- prcomp(trainingIL)
prComp$rotation


## Question 4
# Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" in the train function. What is the accuracy of each method in the test set? Which is more accurate? 
trainingIL <- training[, c("diagnosis", features)] 
names(trainingIL)

# Model A -> use all predictors
modA <- train(diagnosis ~ ., method = "glm", data = trainingIL)
# predA <- predict(modA, testing)

# training "Accuracy" = 0.6861681
modA

# test set accuracy = 0.6463 = 0.65
confusionMatrix(testing$diagnosis, predict(modA, testing))


# Model B -> PCA with PCs explaining 80% of variance
preProc <- preProcess(trainingIL[,-1], method = "pca", thresh = 0.8)
trainPc <- predict(preProc, trainingIL)
modB <- train(diagnosis ~ ., method = "glm", data = trainPc)
# modB <- train(diagnosis ~ ., method = "glm", preProcess = "pca", thres = 0.8, data = trainingIL)

# training "Accuracy" = 0.7084427
modB

# test set accuracy = 0.7195 = 0.72
testPc <- predict(preProc, testing[,-1])
confusionMatrix(testing$diagnosis, predict(modB, testPc))


