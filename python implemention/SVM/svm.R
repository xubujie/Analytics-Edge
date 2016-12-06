library(e1071)
library(rpart)

data(Glass, package="mlbench")
# split data into a train and test set
index  <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index))/3)
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

# implement svm
svm.model <- svm(Type~., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])

# rpart
rpart.model <- rpart(Type~., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")

# confusion matrix
## svm
m1 <- table(pred=svm.pred, true=testset[,10])
acsvm = sum(diag(m1))/sum(m1)
acsvm
## rpart
m2 <- table(pred=rpart.pred, true=testset[,10])
acrpart <- sum(diag(m2))/sum(m2)
acrpart
