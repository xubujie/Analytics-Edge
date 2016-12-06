gerber = read.csv("gerber.csv")
str(gerber)
mean(gerber$voting)
mean(gerber$voting[gerber$civicduty == 1])
mean(gerber$voting[gerber$hawthorne == 1])
mean(gerber$voting[gerber$neighbors == 1])
mean(gerber$voting[gerber$self == 1])

# logistic regression
vote.log = glm(voting ~ hawthorne + civicduty + neighbors + self, data = gerber, family = binomial)
log.pred = predict(vote.log, type = "response")
table(gerber$voting, log.pred > 0.3)
table(gerber$voting, log.pred > 0.5)

controlLog = glm(voting ~ control + sex, data = gerber, family = binomial)
summary(controlLog)
possibilities = data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
predict(controlLog, newdata = possibilities, type = "response")
controlLog2 = glm(voting ~ control + sex + sex:control, data = gerber, family = "binomial")
predict(controlLog2, newdata = possibilities, type = "response")
## AUC 
library(ROCR)
predROCR = prediction(log.pred, gerber$voting)
auc = as.numeric(performance(predROCR, "auc")@y.values)

# Trees
library(rpart)
library(rpart.plot)
cart.model = rpart(voting ~ civicduty + hawthorne + neighbors + self, data = gerber)
prp(cart.model)
cart.model2 = rpart(voting ~ civicduty + hawthorne + neighbors + self, data = gerber,
                    cp = 0.0)
prp(cart.model2)
cart.model3 = rpart(voting ~ civicduty + hawthorne + neighbors + self + sex, data = gerber,
                    cp = 0.0)
prp(cart.model3)

control.cart.model = rpart(voting ~ control, data=gerber, cp=0.0)
control.cart.model2 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(control.cart.model, digits = 6)
prp(control.cart.model2, digits = 6)





