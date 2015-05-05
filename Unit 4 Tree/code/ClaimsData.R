setwd("Desktop/Analytics Edge/Unit 4 Tree/data/")
claims = read.csv("ClaimsData.csv")
str(claims)

table(claims$bucket2009)/nrow(claims)
library(caTools)
set.seed(88)
spl = sample.split(claims$bucket2009, SplitRatio = 0.6)
claimsTrain = subset(claims, spl == TRUE)
claimsTest = subset(claims, spl == FALSE)
mean(claimsTrain$age)
mean(claimsTrain$diabetes)

# baseline method
table(claimsTest$bucket2009, claimsTest$bucket2008)
penaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE,
                       nrow = 5)
penaltyError = as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008))*penaltyMatrix
error = sum(penaltyError)/nrow(claimsTest)


# CART model
library(rpart)
library(rpart.plot)
claimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression+
                     diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +
                     bucket2008 + reimbursement2008, data = claimsTrain, method = "class",
                   cp = 0.00005)
prp(claimsTree)
predictTest = predict(claimsTree, newdata = claimsTest, type = "class")
predTrue.matrix = as.matrix(table(claimsTest$bucket2009, predictTest))
accuracy = sum(diag(predTrue.matrix))/nrow(claimsTest)
error = sum(predTrue.matrix*penaltyMatrix)/nrow(claimsTest)

claimsTree2 = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression+
                     diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +
                     bucket2008 + reimbursement2008, data = claimsTrain, method = "class",
                   cp = 0.00005, parms = list(loss = penaltyMatrix))
predictTest2 = predict(claimsTree2, newdata = claimsTest, type = "class")
predTrue.matrix2 = as.matrix(table(claimsTest$bucket2009, predictTest2))
accuracy2 = sum(diag(predTrue.matrix2))/nrow(claimsTest)
error2 = sum(predTrue.matrix2*penaltyMatrix)/nrow(claimsTest)















