data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area,
                  state.center, state.division, state.name, state.region)

plot(statedata$x, statedata$y,cex = 0.5)
tapply(statedata$HS.Grad, statedata$state.region, mean)
boxplot(statedata$Murder ~ statedata$state.region)

northeast = subset(statedata, state.region == "Northeast")

stateNew = statedata[,1:8]
life.expReg = lm(Life.Exp ~ ., data=stateNew)
plot(statedata$Income, statedata$Life.Exp)

# simplify the model 
life.expReg2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost,
                  data = statedata)
life.expReg3 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost,
                  data = statedata)
life.expReg4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
life.expReg5 = step(life.expReg)

which.max
which.min

