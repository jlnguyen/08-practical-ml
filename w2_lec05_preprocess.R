# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Lecture 5 - Basic preprocessing
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

hist(training$capitalAve, main = "", xlab = "ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

# Standardising
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

# Standardising test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(testCapAve)) / sd(testCapAve)
mean(testCapAveS)
sd(testCapAveS)

## Standardising using 'preProcess' (col 58 is label col: (nonspam, spam))
preObj <- preProcess(training[,-58], method = c("center", "scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)


# preProcess argument
set.seed(32343)
modelFit <- train(type ~ ., data = training,
                  preProcess = c("center", "scale"),
                  method = "glm")
modelFit
hist(trainCapAveS)

## Standardising - Box-Cox transforms
preObj <- preProcess(training[,-58], method = c("BoxCox"))
trainCapAveSTf <- predict(preObj, training[,-58])$capitalAve

par(mfrow = c(1,2));
hist(trainCapAveSTf)
qqnorm(trainCapAveSTf)


## Imputing data
set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
training$capAve[selectNA] <- NA

# Impute and standardise
preObj <- preProcess(training[,-58], method = "knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

# Standardise true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])




