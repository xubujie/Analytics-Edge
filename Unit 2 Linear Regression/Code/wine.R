setwd("~/github/Analytics-Edge/Unit 2 Linear Regression/data/")
wine = read.csv("wine.csv")

model = lm(Price~HarvestRain+WinterRain,data=wine)
summary(model)
cor(wine$HarvestRain,wine$WinterRain)
