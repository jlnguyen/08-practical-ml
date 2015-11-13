# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Lecture 1 - Caret package
#
# Joe Nguyen | 13 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## SPAM Example: Data splitting
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# Fit a model
set.seed(32343)
modelFit <- train(type ~ ., data=training, method="glm")
modelFit

# Final model
modelFit$finalModel

# Prediction
predictions <- predict(modelFit, newdata=testing)
# predictions

# Confusion matrix
confusionMatrix(predictions,testing$type)

