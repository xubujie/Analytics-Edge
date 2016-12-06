library(ggplot2)
library(ggmap)
library(maps)
statesMap = map_data("state")
table(statesMap$group)

# draw the map of United States
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")

# logistic model for election
polling = read.csv("Desktop/Analytics Edge/Unit 7 Visualization/data/PollingImputed.csv")
train = subset(polling, polling$Year <= 2008)
test = subset(polling, polling$Year  > 2008)

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = train, family = binomial)
testPrediction = predict(mod2, newdata = test, type = "response")
testPredictionBinary = as.numeric(testPrediction > 0.5)

predictionDF = data.frame(testPrediction, testPredictionBinary, test$State)
table(testPredictionBinary)

predictionDF$region = tolower(predictionDF$test.State)
predictionMap = merge(statesMap, predictionDF, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]


ggplot(predictionMap, aes(x = long, y = lat, group = group,fill = testPredictionBinary)) + 
  geom_polygon(color = "black", linetype = 3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), 
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group,fill = testPredictionBinary)) + 
  geom_polygon(color = "black", size = 3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), 
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group,fill = testPredictionBinary)) + 
  geom_polygon(color = "black", alpha = 0.3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), 
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")