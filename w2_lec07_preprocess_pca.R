# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Lecture 7 - Preprocessing with PCA
#
# Joe Nguyen | 13 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())

library(caret); library(kernlab); data("spam")
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]; testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8, arr.ind = TRUE)

# Correlated predictors
names(spam)[c(34,32)]
names(spam)[c(40,32)]

plot(spam[,34], spam[,32])
plot(spam[,40], spam[,32])

# Remove correlated predictors: idea of PCA
# Benefits:
# 1) reduced number of predictors
# 2) reduced noise (due to averaging)

# We could rotate the plot
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

# ^most of variability occurs along x-axis (summation of predictors), and data clustered at zero on y-axis (difference of predictors).
# Here, summation of predictors captures most of the information about both predictors, so use summation (for PCA). Also, taking difference results in less information.

# Principle components in R
smallSpam <- spam[, c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

# Rotation matrix
prComp$rotation

# PCA on spam data
typeColour <- ((spam$type == "spam") * 1 + 1)
prComp <- prcomp(spam[,-58])
plot(prComp$x[,1], prComp$x[,2], col = typeColour, xlab = "PC1", ylab = "PC2")

# Log data to make Gaussian
prComp <- prcomp(log10(spam[,-58] + 1))
plot(prComp$x[,1], prComp$x[,2], col = typeColour, xlab = "PC1", ylab = "PC2")


## PCA with caret (specify 2 PCs)
preProc <- preProcess(log(spam[,-58] + 1), method = "pca", pcaComp = 2)
spamPC <- predict(preProc, log(spam[,-58] + 1))
plot(spamPC[,1], spamPC[,2], col = typeColour)

# PCA preprocess on training set
preProc <- preProcess(log(training[,-58] + 1), method = "pca", pcaComp = 2)
trainPc <- predict(preProc, log(training[,-58] + 1))
modelFit <- train(training$type ~ ., method = "glm", data = trainPc)

# prediction
testPc <- predict(preProc, log(testing[,-58] + 1))
confusionMatrix(testing$type, predict(modelFit, testPc))


## Alternative -> this method sets the number of PCs in training phase
modelFit <- train(training$type ~ ., method = "glm", preProcess = "pca", data = training)
confusionMatrix(testing$type, predict(modelFit, testing))


