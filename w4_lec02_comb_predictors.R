# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 4 | Lecture 2 - Combining Predictors
# Joe Nguyen | 18 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))

library(ISLR); data(Wage); library(ggplot2); library(caret)
Wage <- subset(Wage, select = -c(logwage))

# Create a building data set and validation set
idxBuild <- createDataPartition(Wage$wage, p = 0.7, list = FALSE)
validation <- Wage[-idxBuild,]; build <- Wage[idxBuild,]
idxTrain <- createDataPartition(build$wage, p = 0.7, list = FALSE)
training <- build[idxTrain,]; testing <- build[-idxTrain,]
dim(training)
dim(testing)
dim(validation)

# Build 2 models / predictors
mod_1 <- train(wage ~ ., method = "glm", data = training)
mod_2 <- train(wage ~ ., method = "rf", data = training,
               trControl = trainControl(method = "cv", number = 3))
# ^ 3-fold cross-validation (as resampling method)

# Predict on the testing set
pred_1 <- predict(mod_1, testing)
pred_2 <- predict(mod_2, testing)
qplot(pred_1, pred_2, col = wage, data = testing)
# ^ plot shows some correlation between predictions and wage, but not complete correlation. Thus, let's combine the predictors.

# Fit a model that combines predictors (Generalised Additive Model (GAM))
predDf <- data.frame(pred_1, pred_2, wage = testing$wage)
modComb <- train(wage ~ ., method = "gam", data = predDf)

# Prediction on prediction data
predComb <- predict(modComb, predDf)

# Prediction on validation data
pred_1V <- predict(mod_1, validation)
pred_2V <- predict(mod_2, validation)
predDfV <- data.frame(pred_1 = pred_1V, pred_2 = pred_2V)
# ^don't have to include "wage" because only predicting (not model-building)
predCombV <- predict(modComb, predDfV)
qplot(predCombV, wage, data = validation)

