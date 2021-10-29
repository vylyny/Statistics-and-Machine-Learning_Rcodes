#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE) #to pass the arguments from the command line
#example call:Rscript rs5163_A1.R y_example1.txt x_example1.txt 1e-3 1e-10 output_example1.txt


#reading the input files of one column Y values with no header, and a data matrix of X values
y= read.table(args[1],header=F)
y=as.matrix(y)
x_raw=read.table(args[2],header=T)
X0=rep(1,nrow(x_raw))  #adding a new column of a constant value so that the intercept (theta zero) of the linear regression can be estimated
x=as.matrix(cbind(X0,x_raw)) #combind the new x column to the input x matrix

set.seed(12345) 
theta= runif(ncol(x),0,1) #pick a random number of theta values for x matrix to start with 
learning_rate=as.numeric(format(as.numeric(args[3]),sci=FALSE)) #formating the scientific notations of learning rate and epsilon to numeric values
epsilon= format(as.numeric(args[4]),sci=FALSE)
i=0 # iteration
cost <-  sum((x %*% theta - y)^2)
changeInCost <- 1 #pick a number larger than epsilon as change in cost function to begin the while loop
while(changeInCost > epsilon){
	i <- i + 1
    	theta <- theta - (-2)* (learning_rate/nrow(x)) * (t(x) %*% (y-x %*% theta)) #dividing by numbers of observations to get the new theta, otherwise the algorithm (somehow) does  not converge or takes too long to converge (?)
    	costNew <-  sum((x %*% theta - y)^2) #updating cost function with the new theta value in each iteration
    	cost <- append(cost, costNew) #append each new cost to the cost vector storage
    	changeInCost <- abs(cost[i+1] - cost[i]) #calculating the change in cost function per iteration with i as an index for the cost vector
}
cat("theta",theta)
results=x%*%theta #calculating results
final_RSS=sum((results-y)^2) #checking final RSS against the answer key
cat("final_RSS is: ", final_RSS) #looks good
cat("Thank you! Have a nice day...")
write.table(results,file=args[5],col.names=FALSE,row.names=FALSE) #write out the results of predicted y
