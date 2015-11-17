# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 3 | Quiz
# Joe Nguyen | 17 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))


## Question 1
# Load the cell segmentation data from the AppliedPredictiveModeling package using the commands:
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
# 1. Subset the data to a training set and testing set based on the Case variable in the data set.
training <- subset(segmentationOriginal, segmentationOriginal$Case == "Train")
testing <- subset(segmentationOriginal, segmentationOriginal$Case == "Test")
dim(training); dim(testing)
# str(training)

# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings.
set.seed(125)
q1fit <- train(Class ~ ., method = "rpart", data = training)

# 3. In the final model what would be the final model prediction for cases with the following variable values:
q1fit$finalModel

library(rattle)
fancyRpartPlot(q1fit$finalModel)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2

# ANSWERS using dendrogram: [PS, WS, PS, Not possible to predict]

# Using "predict"
quizCases <- testing[1:4,]
quizCases[, sapply(quizCases, is.numeric)] <- 0

quizCases$TotalIntenCh2 <- c(23e3, 50e3, 57e3, 0)
quizCases$FiberWidthCh1 <- c(10, 10, 8, 8)
quizCases$PerimStatusCh1 <- c(2, 0, 0, 2)
quizCases$VarIntenCh4 <- c(0, 100, 100, 100)

predict(q1fit, quizCases)


## Question 2
# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out cross validation?

# K is number of folds
# The bias is larger and the variance is smaller.
# REASON: smaller K means smaller number of training sets, so parameters tuned or biased to these training sets only. Less variance because the size of each training set is larger.
#
# INCORRECT:
# Under leave one out cross validation K is equal to one.
#
# CORRECT (ATTEMPT 2):
# The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.


## Question 3
# Load the olive oil data using the commands:
library(pgmm)
data(olive)
olive = olive[,-1]
# (NOTE: If you have trouble installing the pgmm package, you can download the olive dataset here: olive_data.zip. After unzipping the archive, you can load the file using the load() function in R.)

# These data contain information on 572 different Italian olive oils from multiple regions in Italy. Fit a classification tree where Area is the outcome variable. Then predict the value of area for the following data frame using the tree command with all defaults
testing <-  as.data.frame(t(colMeans(olive)))
# What is the resulting prediction? Is the resulting prediction strange? Why or why not?
q3fit <- train(Area ~ ., method = "rpart", data = olive)
predict(q3fit, testing)

# Since features are type double, regression tree is used.

# ANSWER: 2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata
q3_1fit <- train(as.factor(Area) ~ ., method = "rpart", data = olive)
predict(q3_1fit, testing)


## Question 4
# Load the South Africa Heart Disease Data and create training and test sets with the following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
# Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors.
set.seed(13234)
q4fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family = "binomial", data = trainSA)

# Calculate the misclassification rate for your model using this function and a prediction on the "response" scale:
missClass = function(values, prediction) {
    sum(((prediction > 0.5)*1) != values) / length(values)
}
# What is the misclassification rate on the training set? What is the misclassification rate on the test set?
predTest <- predict(q4fit, testSA)
predTrain <- predict(q4fit, trainSA)

missClass(testSA$chd, predTest)
missClass(trainSA$chd, predTrain)

# Alternative
q4_1fit <- train(as.factor(chd) ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family = "binomial", data = trainSA)

predTest <- predict(q4_1fit, testSA)
predTrain <- predict(q4_1fit, trainSA)

# Misclassification = 1 - Accuracy
cmatTest <- confusionMatrix(testSA$chd, predTest)
1 - cmatTest$overall[[1]]

cmatTrain <- confusionMatrix(trainSA$chd, predTrain)
1 - cmatTrain$overall[[1]]


## Question 5
# Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. Fit a random forest predictor relating the factor variable y to the remaining variables. Read about variable importance in random forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr The caret package uses by default the Gini importance. Calculate the variable importance using the varImp function in the caret package. What is the order of variable importance?
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

# q5fit <- train(y ~ ., method = "rf", importance = T, data = vowel.train)
# varImp(q5fit, useModel = F, scale = T, nonpara = T)
# varImp(q5fit, useModel = F)
# varImp(q5fit, useModel = T)

# ANSWER HERE:
# x.2 1 5 8 6 4 7 3 9 10

# ANSWER IN QUIZ: The order of the variables is:
# x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10

# The order of the variables is:
# x.2, x.1, x.5, x.8, x.6, x.4, x.3, x.9, x.7,x.10

q5fit <- train(y ~ ., method = "rf", importance = F, data = vowel.train)
varImp(q5fit)$importance
# ANSWER HERE:
# x.2 1 5 6 8 4 9 3 7 10
# ^ CORRECT (ATTEMPT 3)

