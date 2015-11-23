# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 4 | Lecture 4 - Unsupervised Prediction
# Joe Nguyen | 23 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))


## Iris example igonring species labels
set.seed(123)
data("iris"); library(caret); library(ggplot2)
idxTrain <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
training <- iris[idxTrain,]; testing <- iris[-idxTrain,]
dim(training); dim(testing)

# Cluster with k-means
kMeans1 <- kmeans(subset(training, select = -Species), centers = 3)
training$cluster <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, data = training,
      col = cluster,
      shape = Species,
      size = 10)

# Compare to real labels
table(kMeans1$cluster, training$Species)

# Build predictor (classification tree)
modCt <- train(cluster ~ ., data = subset(training, select = -Species),
               method = "rpart")
table(predict(modCt, training), training$Species)

# Apply on test set
testClsPred <- predict(modCt, testing)
table(testClsPred, testing$Species)










