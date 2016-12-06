setwd("~/Desktop/Analytics Edge/Unit 7 Visualization/data/")
mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
str(mvt)
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

WeekDay = as.data.frame(table(mvt$Weekday))
# change the value name and order of weekdays
names(WeekDay)[1] = "Day"
WeekDay$Day = factor(WeekDay$Day, order = TRUE , 
                     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                "Saturday", "Sunday"))
library(ggplot2)
ggplot(data = WeekDay, aes(x = Day, y = Freq)) + geom_line(aes(group = 1), alpha = 0.3) +
  ylab("Motor Crime")

# heatmap
DayHours = as.data.frame(table(mvt$Weekday, mvt$Hour))
names(DayHours)[1:2] = c("Day", "Hour")
str(DayHours)
DayHours$Hour = as.numeric(as.character(DayHours$Hour))
DayHours$Day = factor(DayHours$Day, order = TRUE, levels = c("Monday", "Tuesday", 
                        "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(DayHours, aes(x = Hour, y = Freq)) + geom_line(aes(group = Day, color = Day),size = 2)

ggplot(DayHours, aes(x = Hour, y = Day)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name = "Total MV Thefts", low = "yellow", high = "red") +
  theme(axis.title.y = element_blank()) + theme_bw()

ggplot(DayHours, aes(x = Day, y = Hour)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name = "Total MV Thefts", low = "yellow", high = "red") +
  theme(axis.title.y = element_blank()) + theme_bw()

# plot 
library(maps)
library(ggmap)
chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

area = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude,2)))
area$Var1 = as.numeric(as.character(area$Var1))
area$Var2 = as.numeric(as.character(area$Var2))

ggmap(chicago) + geom_point(data = area, aes(x = Var1, y = Var2, color = Freq, size = Freq)) +
  scale_color_gradient(name = "Motor Crime", low = "yellow", high = "red") + 
  scale_size(name = "Motor Crime")

ggmap(chicago) + geom_tile(data = area, aes(x=Var1, y=Var2, alpha = Freq), fill = "red")

area2 = subset(area, Freq != 0)
ggmap(chicago) + geom_tile(data = area2, aes(x=Var1, y=Var2, alpha=Freq), fill = "red")

#



