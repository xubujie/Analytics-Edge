setwd("~/github/Analytics-Edge/Unit 2 Linear Regression/data/")
climate = read.csv("climate_change.csv")
climate_train = subset(climate, Year<=2006)
climate_test = subset(climate, Year>2006)

TempReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train)
summary(TempReg)

SSE = sum(TempReg$residuals^2)
RMSE = sqrt(SSE/nrow(climate_train))
SSE_test = sum((predict(TempReg, newdata=climate_test) - climate_test$Temp)^2)

# Understand the model 
cor(climate_train)

# Simplifying the model
TempReg2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=climate_train)

# step
TempReg3 = step(TempReg)

SSE_test = sum((predict(TempReg3, newdata=climate_test) - climate_test$Temp)^2)
SST_test = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R_2 = 1 - SSE_test/SST_test
