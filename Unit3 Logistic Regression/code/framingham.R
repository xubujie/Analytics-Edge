# make the frist model
framingham = read.csv("framingham.csv")
str(framingham)
library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

# make prediction on test set
predictTest = predict(framinghamLog, type="response", newdata = test)

# create a confusion matrix
table(test$TenYearCHD, predictTest > 0.5)
## accuaracy of the model
(1069+11)/(1069+6+187+11)
## accuaracy of baseline model
(1069+6)/(1069+6+187+11)

# ROCR & AUC
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)

# New model
framinghamLog2 = glm(TenYearCHD ~ male + age + education + cigsPerDay + 
                       BPMeds + prevalentHyp + totChol + sysBP + 
                       heartRate + glucose, data = train, family = binomial)
predictTest2 = predict(framinghamLog2, type = "response", newdata = test)
table(test$TenYearCHD, predictTest2 > 0.5)
accuracy = (1075+11)/(1075+3+187+11)
baseline_accurary = (1075+3)/(1075+3+187+11)

ROCRpred2 = prediction(predictTest2, test$TenYearCHD)
auc2 = as.numeric(performance(ROCRpred2, "auc")@y.values)