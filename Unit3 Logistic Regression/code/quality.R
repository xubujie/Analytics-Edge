setwd("Desktop/Analytics Edge/Unit3 Logistic Regression/data/")
quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)


library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio=0.75)
qualityTrain = subset(quality, split==TRUE)
qualityTest = subset(quality, split==FALSE)

# bulit logistic model
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, family = binomial, 
                 data = qualityTrain)

summary(QualityLog)

predictTrain = predict(QualityLog, type="response")

# Another model
QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, family=binomial,
                  data=qualityTrain)
summary(QualityLog2)

# Confusion table

# ROCR curve
library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))


# AUC
predictTest = predict(QualityLog, type="response",newdata=qualityTest)
ROCRpredTest = prediction(predictTest,qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest,"auc")@y.values)
















