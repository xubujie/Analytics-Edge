# load library
library(dplyr)
library(caTools)
# load data
setwd("~/github/EYDataScience/data/")
train <- read.csv(file = "data_train.csv",sep = ";")
train$RiderID <- as.factor(train$RiderID)
# data preprocessing
set.seed(88)
split = sample.split(train$Speed, SplitRatio=0.7)
SpeedTrain = subset(train, split==TRUE)
SpeedTest = subset(train, split==FALSE)

LinReg = lm(Speed ~ .-Index, data = SpeedTrain)