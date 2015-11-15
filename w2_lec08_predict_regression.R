# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Lecture 8 - Predicting with Regression
#
# Joe Nguyen | 13 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
par(mfrow = c(1,2))

library(caret); data("faithful"); set.seed(333)
inTrain <- createDataPartition(y = faithful$waiting, p = 0.5, list = FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)
plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = "blue",
     xlab = "Waiting", ylab = "Eruption Duration")

# Fit linear model
lm1 <- lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)

# Model fit
lines(trainFaith$waiting, lm1$fitted, lwd = 3)

## Predict a new value
coef(lm1)[1] + coef(lm1)[2] * 80

newdata <- data.frame(waiting = 80)
predict(lm1, newdata)


## Plot predictions - training and test
plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = "blue",
     xlab = "Waiting", ylab = "Eruption Duration")
lines(trainFaith$waiting, predict(lm1), lwd = 3)

plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = "blue",
     xlab = "Waiting", ylab = "Eruption Duration")
lines(testFaith$waiting, predict(lm1, newdata = testFaith), lty = 2, lwd = 3, col = "red")

# Get training / test errors (RMSE)
sqrt(sum((lm1$fitted - trainFaith$eruptions)^2))
# or
sqrt(sum(lm1$residuals^2))

# RMSE of test data
sqrt(sum((predict(lm1, newdata = testFaith) - testFaith$eruptions)^2))

# Prediction intervals
pred1 <- predict(lm1, newdata = testFaith, interval = "prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = "blue")
matlines(testFaith$waiting[ord], pred1[ord,], type = "l", col = c(1,2,2), lty = c(1,2,2), lwd = 3)


## Same process with caret
modFit <- train(eruptions ~ waiting, data = trainFaith, method = "lm")
summary(modFit$finalModel)

# Print RMSE
modFit

