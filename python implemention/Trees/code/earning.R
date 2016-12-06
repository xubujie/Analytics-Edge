census = read.csv("census.csv")
str(census)
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split == TRUE)
test = subset(census, split == FALSE)
log.model = glm(over50k ~ ., data = train, family = binomial)
log.pred = predict(log.model, newdata = test, type = "response")
table(test$over50k, log.pred > 0.5)
table(test$over50k)
library(ROCR)
predROCR = prediction(log.pred, test$over50k)
auc = as.numeric(performance(predROCR, "auc")@y.values)

# tree
library(rpart)
library(rpart.plot)
cart = rpart(over50k ~ ., data = train, method = "class", cp = 0.002)
prp(cart)
cart.pred = predict(cart, newdata = test)
table(test$over50k, cart.pred[,2]>0.5)

# ROC curve
predROCR = prediction(log.pred, test$over50k)
auc = as.numeric(performance(predROCR, "auc")@y.values)
log.ROCRperf = performance(predROCR, "tpr", "fpr")
cart.ROCRpred = prediction(cart.pred[,2], test$over50k)
cart.ROCRperf = performance(cart.ROCRpred, "tpr", "fpr")
cart.auc = as.numeric(performance(cart.ROCRpred, "auc")@y.values)
par(mfrow=c(2,1))
plot(log.ROCRperf)
plot(cart.ROCRperf)
par(mfrow=c(1,1))
# randomForest
library(randomForest)
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
RF = randomForest(over50k ~ ., data = trainSmall)
RF.pred = predict(RF, newdata = test)
table(test$over50k, RF.pred)
## look inside randomForest
vu = varUsed(RF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(RF$forest$xlevels[vusorted$ix]))
### another way
varImpPlot(RF)
# cross validation
library(caret)
library(e1071)
num.folds = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.002, 0.1, 0.002))
CV = train(over50k ~ ., method = "rpart", trControl = num.folds, tuneGrid = cp.grid,
           data = train)
