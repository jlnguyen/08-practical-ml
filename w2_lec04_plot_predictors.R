# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 2 | Lecture 4 - Plotting predictors
#
# Joe Nguyen | 13 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


# ISLR package from book: Introduction to statistical learning
library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

# Training and testing sets
inTrain <- createDataPartition(Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training)
dim(testing)

# Feature plot (from caret)
require(dplyr)
featurePlot(select(training, age, education, jobclass), training$wage,
            plot = "pairs")

# Wage vs age
qplot(age, wage, data = training)

qplot(age, wage, colour = jobclass, data = training)
qplot(age, wage, colour = race, data = training)

# regression smoothers
qplot(age, wage, colour = education, data = training) + 
    geom_smooth(method = "lm", formula = y ~ x)


## cut2, making factors (from a continuous variable)
require(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)

p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = "boxplot")
p1

# boxplot with points overlaid -> shows sufficient data in each boxplot to suggest trend is resent
p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot", "jitter"))
require(gridExtra)
grid.arrange(p1, p2, ncol = 2)

# tables
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

# density plots
qplot(wage, colour = education, data = training, geom = "density")




