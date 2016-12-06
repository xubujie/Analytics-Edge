data(state)
statedata = data.frame(state.x77)

# linear regression
linearReg = lm(Life.Exp ~ ., data = statedata)
summary(linearReg)
linear.pred = predict(linearReg)
linear.SSE = sum((linear.pred - statedata$Life.Exp)^2)

linearReg2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
linear.pred2 = predict(linearReg2)
linear.SSE2 = sum((linear.pred2 - statedata$Life.Exp)^2)

# CART
library(rpart)
library(rpart.plot)
cart = rpart(Life.Exp ~ ., data = statedata)
prp(cart)
cart.pred = predict(cart)
cart.SSE = sum((cart.pred - statedata$Life.Exp)^2)

cart2 = rpart(Life.Exp ~ ., data = statedata, minbucket = 5)
prp(cart2)
cart.pred2 = predict(cart2)
cart.SSE2 = sum((cart.pred2 - statedata$Life.Exp)^2)

cart3 = rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
prp(cart3)
cart.pred3 = predict(cart3)
cart.SSE3 = sum((cart.pred3 - statedata$Life.Exp)^2)

# cross validation - tuning the model
library(caret)
library(e1071)
set.seed(111)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.01,0.5,0.01))
CV = train(Life.Exp ~ ., data = statedata, method = "rpart", 
           trControl = tr.control, tuneGrid = cp.grid)

best.tree = CV$finalModel
prp(best.tree)
best.tree.pred = predict(best.tree)
best.tree.SSE = sum((best.tree.pred - statedata$Life.Exp)^2)

CV2 = train(Life.Exp ~ Area, data = statedata, method = "rpart", 
           trControl = tr.control, tuneGrid = cp.grid)
best.tree2 = CV2$finalModel
best.tree2.pred = predict(best.tree2)
best.tree2.SSE = sum((best.tree2.pred - statedata$Life.Exp)^2)



