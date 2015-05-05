setwd("Desktop/Analytics Edge/Unit2 Linear Regression/data/")

data = read.csv("elantra.csv")
str(data)
train = subset(data, Year <= 2012)
test = subset(data, Year > 2012)
str(train)

# linear regression
salesReg = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data = train)
summary(salesReg)
salesReg2 = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data = train)
summary(salesReg2)

train$Month_factor = as.factor(train$Month)
salesReg3 = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month_factor, 
               data = train)

cor(train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# simplify model
salesReg4 = lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + Month_factor, 
               data = train)

# evaluate the model
test$Month_factor = as.factor(test$Month)

salesPred = predict(salesReg4, newdata=test)
SSE = sum((salesPred - test$ElantraSales)^2)
baseline = mean(train$ElantraSales)

SST = sum((test$ElantraSales - baseline)^2)
1 - SSE/SST
