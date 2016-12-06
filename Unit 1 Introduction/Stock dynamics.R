setwd("Desktop/Analytics Edge/Unit 1//data")
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# Visualizing stock dynamics
plot(CocaCola$Date, CocaCola$StockPrice, xlab="Date", 
     ylab ="StockPrice", type = "l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")),lwd=2, lty=2)
abline(v=as.Date(c("1983-01-01")),lty=2)
abline(v=as.Date(c("1984-01-01")),lty=2)

#Visualizing stock dynamics 1995-2005
plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432],type="l",col="red",ylim=c(0,210))
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col="blue")
lines(GE$Date[301:432],GE$StockPrice[301:432],col="green")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],col="purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col="black")
abline(v=as.Date(c("2000-03-01")),lty=2)
abline(v=as.Date(c("1997-09-01")),lty=2)
abline(v=as.Date(c("1997-11-01")),lty=2)
abline(v=as.Date(c("2004-01-01")),lty=2)
abline(v=as.Date(c("2005-12-30")),lty=2)

# Monthly Trends 
tapply(IBM$StockPrice,months(IBM$Date),mean)
tapply(GE$StockPrice,months(GE$Date),mean)
tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)
