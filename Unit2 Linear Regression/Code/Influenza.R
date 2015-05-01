# Understand the data
FluTrain = read.csv("FluTrain.csv")
which(FluTrain$ILI == max(FluTrain$ILI))
FluTrain[303,]
which(FluTrain$Queries == max(FluTrain$Queries))

hist(FluTrain$ILI)
plot(FluTrain$Queries, log(FluTrain$ILI))

# linear Regression model
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
cor(FluTrain$Queries, log(FluTrain$ILI))

# performance of the test set 
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
str(PredTest1)
index = which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[index]
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))

# Training a time series model
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ log(ILILag2) + Queries, data=FluTrain)

# Evaluating the time series model in the test set
ILILag2_test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2_test)
FluTest$ILILag2[1:2] = FluTrain$ILI[416:417]

PreTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PreTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
