wiki = read.csv("Desktop/Analytics Edge/Unit 5 Text Analytics/data/wiki.csv", 
                stringsAsFactors = FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

# bags of word for add
library(tm)
library(SnowballC)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, tolower)
corpusAdded = tm_map(corpusAdded, removePunctuation)
corpusAdded = tm_map(corpusAdded, removeWords, c(stopwords("english")))
corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
# bags of word for remove
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, tolower)
corpusRemoved = tm_map(corpusRemoved, removePunctuation)
corpusRemoved = tm_map(corpusRemoved, removeWords, c(stopwords("english")))
corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

# set up data
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, split == TRUE)
test = subset(wikiWords, split == FALSE)
# accuracy of baseline model
table(test$Vandal)

# cart model
library(rpart)
library(rpart.plot)
cart = rpart(Vandal ~ ., data = train, method = "class")
prp(cart)
cart.pred = predict(cart, newdata = test, type = "class")
table(test$Vandal, cart.pred)


# cart model2 use class of key word
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE),1,0)
table(wikiWords2$HTTP)
train2 = subset(wikiWords2, split == TRUE)
test2 = subset(wikiWords2, split == FALSE)

cart2 = rpart(Vandal ~ ., data = train2, method = "class")
prp(cart2)
cart.pred2 = predict(cart2, newdata = test2, type = "class")
table(test2$Vandal, cart.pred2)

# cart model3 with new variable NumWordsAdded & NumWordsRemoved
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

train3 = subset(wikiWords2, split == TRUE)
test3 = subset(wikiWords2, split == FALSE)
cart3 = rpart(Vandal ~ ., data = train3, method = "class")
prp(cart3)
cart.pred3 = predict(cart3, newdata = test3, type = "class")
table(test3$Vandal, cart.pred3)

# cart use anthoer two variable Minor and Loggedin
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
train4 = subset(wikiWords3, split == TRUE)
test4 = subset(wikiWords3, split == FALSE)
cart4 = rpart(Vandal ~ ., data = train4, method = "class")
prp(cart4)
cart.pred4 = predict(cart4, newdata = test4, type = "class")
table(test4$Vandal, cart.pred4)

## cross validation
colnames(train4) = make.names(colnames(train4), unique = TRUE)
colnames(test4) = make.names(colnames(test4), unique = TRUE)
library(caret)
library(e1071)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0, 1, 0.01))
CV = train(Vandal ~ ., data = train4, method = "rpart", trControl = tr.control,
           tuneGrid = cp.grid)
best.cart = CV$finalModel
best.cart.pred = predict(best.cart, newdata = test4, type = "class")
table(test4$Vandal, best.cart.pred)

# randomForest 
library(randomForest)
randomForest = randomForest(Vandal ~ ., data = train4, nodsize = 20, ntree = 200)
randomForest.pred = predict(randomForest, newdata = test4, type = "class")
table(test4$Vandal, randomForest.pred)


# logistic regression
log = glm(Vandal ~ ., data = train4, family = binomial)
log.pred = predict(log, newdata = test4, type = "response")
table(test4$Vandal, log.pred > 0.5)
