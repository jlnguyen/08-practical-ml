# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 3 | Lecture 3 - Random Forests
# Joe Nguyen | 16 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))


data("iris"); library(caret); library(ggplot2)
names(iris)
# ^ predict "Species" using features "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"
table(iris$Species)

idxTrain <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
training <- iris[idxTrain,]; testing <- iris[-idxTrain,]
dim(training); dim(testing)

# Random Forest
modFit <- train(Species ~ ., method = "rf", prox = TRUE, data = training)
modFit

# Get a single tree
getTree(modFit$finalModel, k = 1)

# Class centers
irisP <- classCenter(training[, c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)

p <- qplot(Petal.Width, Petal.Length, col = Species, data = training)
p + geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species),
               size = 10, shape = 4, data = irisP)

# Predicting new values
pred <- predict(modFit, testing)
testing$predRight <- pred == testing$Species
table(pred, testing$Species)

qplot(Petal.Width, Petal.Length, col = Species, shape = predRight,
      size = 5,
      data = testing,
      main = "newdata Predictions")

