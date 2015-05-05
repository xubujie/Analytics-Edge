polling = read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

library("mice")
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
polling$SurveyUSA = imputed$SurveyUSA
polling$Rasmussen = imputed$Rasmussen

# split data into train and test
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
table(Train$Republican)

# simple baseline model
table(Train$Republican, sign(Train$Rasmussen))
table(Test$Republican, sign(Test$Rasmussen))

# logistic regression
cor(Train[c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])
model1 = glm(Republican ~ PropR, data = Train, family = binomial)
predTrain1 = predict(model1, type = "response")
predTest1 = predict(model1, newdata = Test, type = "response")
table(Train$Republican, predTrain1 >= 0.5)
table(Test$Republican, predTest1 >= 0.5)

model2 = glm(Republican ~ PropR + DiffCount, data = Train, family = binomial)
predTrain2 = predict(model2, type = "response")
table(Train$Republican, predTrain2 >= 0.5)
predTest2 = predict(model2, type = "response", newdata = Test)
table(Test$Republican, predTest2 >= 0.5)

#AUC 
library(ROCR)
ROCRpredTest = prediction(predTest1, Test$Republican)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)



