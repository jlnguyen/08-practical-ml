# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 1 | Quiz
#
# Joe Nguyen | 13 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())

## Question 1
# Which of the following are steps in building a machine learning algorithm?
#
# Collecting data to answer the question.
# Data mining (INCORRECT ATTEMPT 2)
# Evaluating the prediction. (CORRECT ATTEMPT 3)


## Question 2
# Suppose we build a prediction algorithm on a data set and it is 100% accurate on that data set. Why might the algorithm not work well if we collect a new data set? 
#
# Our algorithm may be overfitting the training data, predicting both the signal and the noise. 


## Question 3
# What are typical sizes for the training and test sets? 
#
# 60% in the training set, 40% in the testing set. 


## Question 4
# What are some common error rates for predicting binary variables (i.e. variables with two possible values like yes/no, disease/normal, clicked/didn't click)? 
#
# INCORRECT
# Root mean squared error
#
# CORRECT (ATTEMPT 2)
# Specificity
# Predictive value of a positive, i.e. precision (ATTEMPT 3)


## Question 5
# Suppose that we have created a machine learning algorithm that predicts whether a link will be clicked with 99% sensitivity and 99% specificity. The rate the link is clicked is 1/1000 of visits to a website. If we predict the link will be clicked on a specific visit, what is the probability it will actually be clicked?
#
# C:    click
# nC:   not clicked
# Sensitivity (recall): p(+|C)
# Specificity: p(-|nC)
# p(C) = 0.001
#
# Find p(C|+) -> use Bayes
pC <- 0.001
pCnot <- 1 - pC
pyC <- 0.99
pnCnot <- 0.99
pyCnot <- 1 - pnCnot

# Bayes
pCy <- pyC * pC / (pyC * pC + pyCnot * pCnot)
pCy * 100

