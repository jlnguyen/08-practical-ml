# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 3 | Lecture 5 - Model Based Prediction
# Joe Nguyen | 17 Nov, 2015

# Supervised learning methods: Linear Discriminant Analysis (LDA) and Naive Bayes (NB). Training set used and number of classes known.


# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))

data("iris"); library(ggplot2); library(caret)
names(iris)
table(iris$Species)

idxTrain <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
training <- iris[idxTrain,]; testing <- iris[-idxTrain,]
dim(training); dim(testing)

# Build Predictors / Features
# Linear Discriminant Analysis (LDA)
modlda <- train(Species ~ ., data = training, method = "lda")

# Naive Bayes (NB)
modnb <- train(Species ~ ., data = training, method = "nb")

plda <- predict(modlda, testing)
pnb <- predict(modnb, testing)
table(plda, pnb)

# Comparison of results
equalPred <- plda == pnb
qplot(Petal.Width, Sepal.Width, col = equalPred, data = testing)

