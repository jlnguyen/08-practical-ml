# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Lecture 2 - Data Slicing
#
# Joe Nguyen | 13 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)


## Cross validation
# K-folds
set.seed(32323)
folds <- createFolds(y=spam$type, k=10,
                     list=TRUE, returnTrain=TRUE)
sapply(folds,length)

# Return test subset
foldsTs <- createFolds(y=spam$type, k=10,
                     list=TRUE, returnTrain=FALSE)
sapply(foldsTs,length)
foldsTs[[1]][1:10]


## Resampling
set.seed(32323)
folds <- createResample(y=spam$type, times=10,
                        list=TRUE)
sapply(folds,length)
folds[[1]][1:10]


## Time slices
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]

folds$train[[2]]
folds$test[[2]]


###############################
## Lecture 3 - Train Options ##
###############################
args(train.default)
args(trainControl)
