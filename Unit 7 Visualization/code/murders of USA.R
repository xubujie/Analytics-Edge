getwd()
murder = read.csv("murders.csv")
statesMap = map_data("state")
str(murder)
str(statesMap)
library(ggplot2)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")

murder$region = tolower(murder$State)
murderMap = merge(murder, statesMap, by = "region")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + 
  geom_polygon(color = "black")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red")

murderMap$MurderRate = murderMap$Murders/murderMap$Population * 10000

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "black", high = "red", limits = c(0,1))

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,1))

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "black", high = "red", guide = "legend")