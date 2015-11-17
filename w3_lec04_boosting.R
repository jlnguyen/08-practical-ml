# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 3 | Lecture 4 - Boosting
# Joe Nguyen | 16 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))

# BOOSTING
# Have to define number of classes k

# R boosting libraries
# gbm       boosting with trees
# mboost    model based boosting
# ada       additive logistic regression
# gamBoost  generalised additive models

library(ISLR); data("Wage"); library(ggplot2); library(caret)
Wage <- subset(Wage, select = -c(logwage))
idxTrain <- createDataPartition(Wage$wage, p = 0.7, list = FALSE)
training <- Wage[idxTrain,]; testing <- Wage[-idxTrain,]

# Boosting with trees
modFit <- train(wage ~ ., method = "gbm", data = training, verbose = FALSE)
modFit

# Predicted wage (x) vs true wage (y)
qplot(predict(modFit, testing), wage, data = testing)

