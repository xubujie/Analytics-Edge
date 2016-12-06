tweets = read.csv("Desktop/Analytics Edge/Unit 7 Visualization/data/tweets.csv", stringsAsFactors = FALSE)
library(tm)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))

library(wordcloud)
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25))



negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets),scale = c(2, .25))

wordcloud(colnames(allTweets), colSums(allTweets), 
          scale = c(4, 0.4), random.order = FALSE,
          colors=brewer.pal(9, "YlOrRd")[5:9])

display.brewer.all() 
