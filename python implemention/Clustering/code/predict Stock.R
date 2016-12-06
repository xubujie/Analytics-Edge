setwd("Desktop/Analytics Edge/Unit 6 Clustering/data/")
stocks = read.csv("StocksCluster.csv")
str(stocks)
summary(stocks)
table(stocks$PositiveDec)
mean(stocks$PositiveDec)
cor(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

# logistic
StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
trainPred = predict(StocksModel,type = "response")
table(stocksTrain$PositiveDec, trainPred > 0.5)

testPred = predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, testPred > 0.5)

# Clustering
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

## normalize
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
str(clusterTest)
table(clusterTest)

# cluster-specific-predictions
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
table(stocksTest1$PositiveDec, PredictTest1 > 0.5)

PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)

PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)

# overall accuracy 
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)






