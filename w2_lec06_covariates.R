# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Lecture 6 - Covariate Creation
#
# Joe Nguyen | 13 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## COVARIATES, aka FEATURES, PREDICTORS
require(kernlab); data("spam")

# Create covariate for later usage
spam$capitalAveSq <- spam$capitalAve^2


## Load example data
library(ISLR); library(caret); data("Wage")
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]


# Convert factor vars into indicator vars
table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

# Removing zero covariates
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

# Spline basis - fit non-linear regression
library(splines)
bsBasis <- bs(training$age, df = 3)
bsBasis

# Fitting curves with splines
lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lm1, newdata = training), col = "red", pch = 19, cex = 0.5)
plot(training$age, predict(lm1, newdata = training), col = "blue")

# Splines on the test set
predict(bsBasis, age = testing$age)



