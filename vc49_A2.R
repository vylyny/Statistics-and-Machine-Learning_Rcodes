#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE) #to pass the arguments from the command line
#example call: Rscript rs5163_A2.R tests/example1_input.txt tests/my_output_example1.txt
#-----------packages required:
#install.packages("tidyr")
library("tidyr") #this is required for the "separate" function below
#-----------input loading and data wrangling to satisfy the glm data input 
input <- read.csv(args[1], sep="")
data= t(input) #transpose so that the obs are rows and predictors are columns
data=cbind(phenotype=rownames(data),data.frame(data,row.names=NULL))
data=separate(data=data,col=phenotype,into= c("cell_type","observation"))
data$cell_type=as.factor(as.character(data$cell_type)) #convert the predictor into factor to run glm
#---------------------------------------- writing the likelihood function

loglikelihood<- function(y,yhat){  #yhat is the predicted probabilty of y=1 given x
  return(sum(y*log(yhat)+(1-y)*log(1-yhat)))
}

#----------------------------------------Main function for forward selection
remaining=data[-c(1,2)] #start with all the genes (columns 1 and 2 of the df(data) are phenotypes)
genes_remaining=colnames(remaining) #names of gene remaining to be picked after each iteration
selected=c() #gene selected in each iteration
null<- glm(cell_type~1,data = data,family = "binomial") #start with a null model
current_best=formula(null)
max_LL=c() #find the best predictor in each iteration using max log likelihood
iteration=0
iteration_count=c()
CV.error.5=c()
while (iteration < 50){
  LL=c() #to concatenate the log likelihood value in each iteration
  iteration=iteration+1
  for (candidates in genes_remaining){
    fit <- as.formula(paste(current_best[2],"~",paste(current_best[3],candidates,sep = "+")))
    new_fit<- glm(fit,data = data,family = "binomial")
    p=predict(new_fit,type="response") #calculating the predicted values of y
    y=as.numeric(data[,1])
    y=y-1 #need to substract 1 because y was converted to value 1 and 2 instead of 0 and 1
    LL=append(LL,loglikelihood(y,p))
  }
  max=max(LL) 
  max_LL=append(max_LL,max) #appending max log likelihood per logistic model in each iteration
  best_candidates=genes_remaining[which.max(LL)] #printing the name of gene with max LL
  selected=append(selected,best_candidates) #gene selected if it has the max log likelihood compared to other candidates
  genes_remaining=genes_remaining[-c(which.max(LL))] #take out the best predictor from the iterative list of all predictors
  #updating new best model incorporating the new predictor selected above
  fit=as.formula(paste(current_best[2],"~",paste(current_best[3],best_candidates,sep = "+")))
  #cross validation for the model selected above
  set.seed(123) #5-fold CV
  folds <- split(sample(nrow(data), nrow(data),replace=FALSE), as.factor(1:5)) #generate random non-overlapped 5 folds of samples
  test_error=c()
  for (i in 1:5) {
    test=data[folds[[i]],] #test data with 1/5 of the data
    train=data[-folds[[i]],] # train data with the remaining 4/5 of the data
    model_forCV<- glm(fit,data=train,family = "binomial")
    predict_test=predict(model_forCV,newdata=test,type = "response")
    y=as.numeric(test[,1])
    y=y-1
    test_error_new=mean((y-predict_test)^2) #calculating the CV error in each iteration of the 5-fold CV
    test_error=append(test_error,test_error_new)
  }
  CV.error.5=append(CV.error.5,mean(test_error)) #find the average CV values 
  cat("doing iteration",iteration)
  current_best<-glm(fit,data = data,family = "binomial") #update model for next iteration
  current_best<-formula(current_best)
  iteration_count=append(iteration_count,iteration)
}

results=cbind(iteration_count,selected,CV.error.5,max_LL)
colnames(results)=c("Iteration","Predictor","CV","Log.Likelihood")
write.table(results,file=args[2],row.names = FALSE,col.names = TRUE,quote = FALSE)


