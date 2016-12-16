library(dplyr)
mtv <- read.csv("data/mvtWeek1.csv")
str(mtv)
summary(mtv)
head(mtv)
max(mtv$ID)


DateConvert = as.Date(strptime(mtv$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mtv$Month = months(DateConvert)
mtv$Weekday = weekdays(DateConvert)
mtv$Date = DateConvert



# plot
hist(mtv$Date, breaks=100)

boxplot(mtv$Date~mtv$Arrest)

# plot
Arrest = c()
Sum = c()
Portion = c()
for(i in 1:12){
  Arrest[i] <- sum(mtv$Arrest[which(mtv$Year==(2000+i))])
  Sum[i] <- length(mtv$Arrest[which(mtv$Year==(2000+i))])
  Portion[i] <- Arrest[i]/Sum[i]
}

# Problem 4
Top5 <- subset(mtv, LocationDescription == "STREET" | 
                 LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                 LocationDescription == "ALLEY" |
                 LocationDescription == "GAS STATION" |
                 LocationDescription == "DRIVEWAY - RESIDENTIAL" )

Top5$LocationDescription = factor(Top5$LocationDescription)

















