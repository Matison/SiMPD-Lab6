library("neuralnet")

#Going to create a neural network to perform prediction
#Type ?neuralnet for more information on the neuralnet library

#Generate training data
#And store them as a dataframe
traininginput <- as.data.frame(matrix(c(24.2,100-51200,4399,
									   24.3,100-25600,1999,
									   24.3,100-25600,4999,
									   24.3,100-25600,2399,
									   16.3,100-25600,1599,
									   24.3,100-25600,3295,
									   10.2,160-51200,10199, 
									   16.0,200-25600,2499, 
									   16.0,200-25600,3849,
							       16.0,200-25600,2449,
									   18.84,100-25600,1699,
									   16.0,200-25600,2499), nrow=12, ncol=3))
trainingoutput <- c(1269, 1379, 2349, 1299, 2299, 2160, 1499, 2259, 2699, 1419, 2269, 1489)

#Column bind the data into one variable
trainingdata <- cbind(traininginput, trainingoutput)

# Create Vector of Column Max and Min Values
maxs <- apply(trainingdata[,], 2, max)
mins <- apply(trainingdata[,], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.trainingdata <- as.data.frame(scale(trainingdata[,], center=mins, scale=maxs-mins))
trainingdata <- scaled.trainingdata

# Check out results
print(head(trainingdata, 10))

colnames(trainingdata) <- c("Resolution", "RangeSensitivity", "Lens", "Price") 
print(trainingdata)

#Train the neural network
#Going to have C(5, 4, 3) hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.price <- neuralnet(Price~Resolution+RangeSensitivity+Lens, trainingdata, hidden=c(5, 4, 3), threshold=0.001)
print(net.price)

#Plot the neural network
plot(net.price)

#Test the neural network on some training data
testdata <- as.data.frame(matrix(c(262, 64, 28,
                                   206, 75, 16,
                                   361, 56, 20), nrow=3, ncol=3))
scaled.testdata <- as.data.frame(scale(testdata[,], center=mins[1:3], scale=maxs[1:3]-mins[1:3]))
net.results <- compute(net.price, scaled.testdata) #Run them through the neural network

#Lets see what properties net.price has
ls(net.results)

#Lets see the results
print(net.results$net.result)