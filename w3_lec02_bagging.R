# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 3 | Lecture 2 - Bagging (Bootstrap Aggregating)
# Joe Nguyen | 16 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
par(mfrow = c(1,1))

library(ElemStatLearn); data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Bagged loess (Local regression)
# Predict temperature from ozone
nPred <- 155
ll <- matrix(NA, nrow = 10, ncol = nPred)
for (i in 1:10) {
    ss <- sample(1 : dim(ozone)[1], replace = TRUE)
    ozone0 <- ozone[ss,]
    ozone0 <- ozone0[order(ozone$ozone),]
    loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
    ll[i,] <- predict(loess0, newdata = data.frame(ozone = 1:nPred))
}

plot(ozone$ozone, ozone$temp, pch = 19, cex = 0.5)
for (i in 1:10) {
    lines(1:nPred, ll[i,], col = "grey", lwd = 2)
}
lines(1:nPred, apply(ll, 2, mean), col = "red", lwd = 2)


## Bagging in caret
# Custom bagging method -> treebag
features <- data.frame(ozone = ozone$ozone)
outcome <- ozone$temperature
treebag <- bag(features, outcome, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone, ozone$temperature, col = "lightgrey", pch = 19)

# First ctree
points(ozone$ozone, predict(treebag$fits[[1]]$fit, features), col = "red", pch = 19)

# Bagged ctrees (average)
points(ozone$ozone, predict(treebag, features), col = "blue", pch = 19)

# Parts of bagging (conditional regression tree)
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate

