setwd("Desktop/Analytics Edge/Unit 4 Tree/data/")
stevens = read.csv("stevens.csv")
str(stevens)

library(caTools)
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, split == TRUE)
test = subset(stevens, split == FALSE)
library(rpart)
library(rpart.plot)

stevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                      LowerCourt + Unconst, data = train, method = "class", minbucket = 25)
prp(stevensTree)
predictCART = predict(stevensTree, newdata = test, type = "class")

# ROCR
library(ROCR)
predictROC = predict(stevensTree, newdata = test)
pred = prediction(predictROC[,2], test$Reverse)
perf = performance(pred, "tpr", "fpr")
auc = as.numeric(performance(pred, "auc")@y.values)

# new tree
stevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                      LowerCourt + Unconst, data = train, method = "class", minbucket = 5)
prp(stevensTree2)
predictCART2 = predict(stevensTree2, newdata = test, type = "class")
table(test$Reverse, predictCART2)

stevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                       LowerCourt + Unconst, data = train, method = "class", minbucket = 100)
prp(stevensTree3)
predictCART3 = predict(stevensTree3, newdata = test, type = "class")
table(test$Reverse, predictCART3)

# random forest
library(randomForest)
set.seed(100)
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
stevensRandomForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                              LowerCourt + Unconst, data = train,
                              nodsize = 25, ntree = 200)
randomForestPred = predict(stevensRandomForest, newdata = test)
table(test$Reverse, randomForestPred)

set.seed(200)
stevensRandomForest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                                     LowerCourt + Unconst, data = train,
                                   nodsize = 25, ntree = 200)
randomForestPred2 = predict(stevensRandomForest2, newdata = test)
table(test$Reverse, randomForestPred2)

# Cross-validation
library(caret)
library(e1071)
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
      data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

stevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                       LowerCourt + Unconst, data = train, method = "class", cp = 0.17)
predictCV = predict(stevensTreeCV, newdata = test, type = "class")
table(test$Reverse, predictCV)
prp(stevensTreeCV)






















