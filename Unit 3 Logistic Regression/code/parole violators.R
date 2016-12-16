# Understanding data
parole = read.csv("parole.csv")
str(parole)
summary(parole)

sum(parole$violator)

## Deal with unordered factor
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

## Splitting data 
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Buliding a Logistic Regression model
mod1 = glm(violator ~ ., data = train, family = binomial)

# Evaluationg the model
predTest = predict(mod1, newdata = test, type = "response")
table(test$violator, predTest >= 0.5)
sensitivity = 12/23
specificity = 167/(179)
accuracy = 179/202

## compare with baseline model
table(test$violator)

# AUC
library(ROCR)
ROCRpredTest = prediction(predTest, test$violator)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
