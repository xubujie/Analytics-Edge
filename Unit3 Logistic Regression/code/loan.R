loans = read.csv("loans.csv")
str(loans)
1 - mean(loans$not.fully.paid)
summary(loans)

dataMiss = subset(loans, is.na(log.annual.inc) == TRUE |
                    is.na(days.with.cr.line) == TRUE |
                    is.na(revol.util) == TRUE |
                    is.na(inq.last.6mths) == TRUE |
                    is.na(delinq.2yrs) == TRUE |
                    is.na(pub.rec) == TRUE)
mean(dataMiss$not.fully.paid)
# multiple imputation
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# Predict models 
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
Train = subset(loans, split == TRUE)
Test = subset(loans, split == FALSE)

mod1 = glm(not.fully.paid ~ ., data = Train, family = "binomial")

predicted.risk = predict(mod1, newdata = Test, type = "response")
Test$predicted.risk = predicted.risk
# AUC
library(ROCR)
ROCRpredTest = prediction(predicted.risk, Test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

# A "Smart baseline"
mod2 = glm(not.fully.paid ~ int.rate, data = Train, family = "binomial")
pred2 = predict(mod2, newdata = Test, type = "response")
ROCRpredTest2 = prediction(pred2, Test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest2, "auc")@y.values)

# A simple investment strategy
Test$profit = exp(Test$int.rate*3) - 1
Test$profit[Test$not.fully.paid == 1] = -1

highInterest = subset(Test, int.rate >= 0.15)
highInterest$profit = exp(highInterest$int.rate*3) - 1
highInterest$profit[highInterest$not.fully.paid == 1] = -1

cutoff = sort(highInterest$predicted.risk, decreasing = FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
