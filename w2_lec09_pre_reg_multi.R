# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Lecture 9 - Predicting with Regression, multiple covariates
#
# Joe Nguyen | 15 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
par(mfrow = c(1,2))


# logwage is outcome (to predict) -> take out of dataset
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage, select = -c(logwage))
summary(Wage)

# Get training / test sets
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

# Feature plot
featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage, plot = "pairs")

# Age vs wage by jobclass
require(ggplot2)
qplot(age, wage, data = training, colour = jobclass)

# by education
qplot(age, wage, data = training, colour = education)


## Fit linear model
modFit <- train(wage ~ age + jobclass + education,
                method = "lm", data = training)
finMod <- modFit$finalModel

# Diagnostics
# ideally want residuals to be zero for all fitted values
plot(finMod, 1, pch = 19, cex = 0.5, col = "#00000010")

# Colour by variables not used in model
qplot(finMod$fitted, finMod$residuals, colour = race, data = training)

# Plot by index
par(mfrow = c(1,1))
plot(finMod$residuals, pch = 19, cex = 0.5)
par(mfrow = c(1,2))

# Predicted vs truth in test set
pred <- predict(modFit, testing)
qplot(wage, pred, colour = year, data = testing)


## If want to use all covariates
modFitAll <- train(wage ~ ., data = training, method = "lm")
predAll <- predict(modFitAll, testing)
qplot(wage, predAll, data = testing)

