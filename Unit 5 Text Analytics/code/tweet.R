setwd("Desktop/Analytics Edge/Unit 5 Text Analytics/data/")
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)

library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus = tm_map(corpus, stemDocument)
frequencies = DocumentTermMatrix(corpus)

inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq=20)
sparse = removeSparseTerms(frequencies, 0.995)
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

# cart
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data = trainSparse, method = "class")
prp(tweetCART)
predictCART = predict(tweetCART, newdata = testSparse, type = "class")
table(testSparse$Negative, predictCART)

# logistic 
log = glm(Negative ~ ., data = trainSparse, family = binomial)
log.pred = predict(log, newdata = testSparse, type = "response")
table(testSparse$Negative, log.pred >0.5)
