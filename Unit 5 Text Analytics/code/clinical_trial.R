setwd("Desktop/Analytics Edge/Unit 5 Text Analytics/data/")
trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
str(trials)
max(nchar(trials$abstract))
table(nchar(trials$abstract))
which.min(nchar(trials$title))
trials$title[1258]

# bag of words
library(tm)
library(SnowballC)
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
sparseTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(sparseTitle))

corpusAbstrat = Corpus(VectorSource(trials$abstract))
corpusAbstrat = tm_map(corpusAbstrat, tolower)
corpusAbstrat = tm_map(corpusAbstrat, removePunctuation)
corpusAbstrat = tm_map(corpusAbstrat, removeWords, stopwords("english"))
corpusAbstrat = tm_map(corpusAbstrat, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstrat)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))
sort(colSums(dtmAbstract))

# building  a model
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split == TRUE)
test = subset(dtm, split == FALSE)
table(train$trial)

library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data = train, method = "class")
prp(trialCART)
trainPred = predict(trialCART)[,2]
max(trainPred)
## training accuracy
table(train$trial, trainPred > 0.5)
## test accuracy
testPred = predict(trialCART, newdata = test)[,2]
table(test$trial, testPred > 0.5)
# ROCR 
library(ROCR)
testROCR = prediction(testPred, test$trial)
testAUC = as.numeric(performance(testROCR, "auc")@y.values)






















