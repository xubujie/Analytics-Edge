library("neuralnet")
#Going to create a neural network to perform sqare rooting

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <- as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

trainingdata <- cbind(traininginput, trainingoutput)
colnames(trainingdata) <- c("Input", "Output")

#trainiing nerual network
net.sqrt <- neuralnet(Output~Input, trainingdata, hidden=10, threshold = 0.01)
print(net.sqrt)

#plot neural network
plot(net.sqrt)


# Test the neural network on some training data
testdata <- as.data.frame((1:10)^2)
net.results <- compute(net.sqrt, testdata)

ls(net.results)
print(net.results$net.result)
#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)
