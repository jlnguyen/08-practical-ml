# Coursera JHPH Data Science
# 08 - Pratical Machine Learning
# Week 4 | Lecture 3 - Forecasting
# Joe Nguyen | 23 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/08-practical-ml"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())
# par(mfrow = c(1,2))


## Apple Data
library(quantmod); library(ggplot2)
from.dat <- as.Date("01/01/08", format="%d/%m/%y")
to.dat <- as.Date("31/10/15", format="%d/%m/%y")
getSymbols("AAPL", src="google")
head(AAPL)

# Summarise monthly and store opening price as time series
mAapl <- to.monthly(AAPL)
aaplOpen <- Op(mAapl)
ts1 <- ts(aaplOpen, frequency = 12)
plot(ts1, xlab = "Years + 1", ylab = "AAPL")

# Time series decomposition (trend, seasonal, cyclic)
plot(decompose(ts1), xlab = "Years + 1")


# Training / testing sets - use time windows
ts1Train <- window(ts1, start = 1, end = 5)
ts1Test <- window(ts1, start = 5, end = (7-0.01))
ts1Train

# Simple moving average
plot(ts1Train)
addSMA(on = dev.cur())
# lines(ma(ts1Train, order = 3), col = "red")


# Exponential smoothing
# ets1 <- ets(ts1Train, model = "MMM")

