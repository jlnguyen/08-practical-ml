# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 3 | Lecture 1 - Predicting with trees
#
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

# Iris petal widths / sepal widths
qplot(Petal.Width, Sepal.Width, colour = Species, data = training)

# Iris petal lengths / sepal lengths
qplot(Petal.Length, Sepal.Length, colour = Species, data = training)
# ^ data better seperated (classified) according to petal width and length (vs sepal width and length)

## Classification tree
# "rpart" -> R package for classification and regression trees
# modFit <- train(Species ~ ., method = "rpart", data = training)
modFit <- train(Species ~ Sepal.Length + Sepal.Width, method = "rpart", data = training)
print(modFit$finalModel)

# Dendrogram
plot(modFit$finalModel, uniform = TRUE,
     main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)

# Prettier plot
library(rattle)
fancyRpartPlot(modFit$finalModel)



