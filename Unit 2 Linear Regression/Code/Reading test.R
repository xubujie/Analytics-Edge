setwd("~/github/Analytics-Edge/Unit 2 Linear Regression/data/")
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# unordered factors in regression 

# building a model
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

LinReg = lm(readingScore ~ ., data=pisaTrain)
SSE = sum(LinReg$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))

# predicting on unseen data
predTest = predict(LinReg, newdata=pisaTest)
SSE = sum((predTest-pisaTest$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaTest))

baseline = mean(pisaTrain$readingScore)
SST = sum((baseline-pisaTest$readingScore)^2)
R = 1 - SSE/SST
