setwd("Desktop/Analytics Edge/Unit3 Logistic Regression/data/")
songs = read.csv("songs.csv")
str(songs)
str(subset(songs, year == 2010))
str(subset(songs, artistname == "Michael Jackson"))
subset(songs, artistname == "Michael Jackson" & Top10 == 1)
table(songs$timesignature)
songs[which.max(songs$tempo),]

# Creating our prediction model
songsTrain = subset(songs, year <= 2009)
songsTest = subset(songs, year > 2009)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[,!(names(songsTrain) %in% nonvars)]
songsTest = songsTest[,!(names(songsTest) %in% nonvars)]

songsLog1 = glm(Top10 ~ ., data = songsTrain, family = binomial)
cor(songsTrain[c("loudness", "energy")])

## model2
songsLog2 = glm(Top10 ~ . - loudness, data = songsTrain, family = binomial)

## model3
songsLog3 = glm(Top10 ~ . - energy, data = songsTrain, family = binomial)

# validating model
library(ROCR)
predTest = predict(songsLog3, newdata = songsTest, type = "response")
table(songsTest$Top10, predTest >= 0.45)
ROCRpredTest = prediction(predTest, songsTest$Top10)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

## accuracy of baseline model
table(songsTest$Top10)

## index
sensitivity = 19/59
specificity = 309/314
