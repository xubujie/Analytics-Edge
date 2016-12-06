# understand data 
setwd("~")
setwd("Desktop/Analytics Edge/Unit 4 Tree/data/")
boston = read.csv("boston.csv")
str(boston)
# split data
library(caTools)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

## linear regression model
linear.reg = lm(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD +
                  TAX + PTRATIO, data = train)
predict.linear = predict(linear.reg, newdata = test)
SSE.linear = sum((predict.linear - test$MEDV)^2)

## CART 
library(rpart)
library(rpart.plot)
tree = rpart(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD +
               TAX + PTRATIO, data = train)
prp(tree)
predict.tree = predict(tree, newdata = test)
SSE.tree = sum((predict.tree - test$MEDV)^2)

# randomForesst
library(randomForest)
randomForest = randomForest(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD +
                              TAX + PTRATIO, data = train, nodsize=20, ntree=200)
predict.randomForest = predict(randomForest, newdata = test)
SSE.randomForest = sum((predict.randomForest - test$MEDV)^2)


# cross validation
library(caret)
library(e1071)
num.flods = trainControl(method="cv", number=10)
cp.grid = expand.grid(.cp=seq(0,0.11,0.001))
tr.cross = train(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD +
                   TAX + PTRATIO, data=train,method="rpart", 
                 trControl=num.flods, tuneGrid=cp.grid)
best.tree = tr.cross$finalModel
prp(best.tree)
predict.best.tree = predict(best.tree, newdata = test)
SSE.best.tree = sum((predict.best.tree-test$MEDV)^2)
