#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE) #to pass the arguments from the command line
#suppressing the warning and package installing messages
options( warn = -1 )
shhh <- suppressPackageStartupMessages
#example call: Rscript Team1_20genesClassifier.R train_data.txt test_data.txt output_subtypesPrediction.txt

#install packages
shhh(library(glmnet))
shhh(library(tidyr))
shhh(library(dplyr))
shhh(library(ggplot2))

#-----------------------------------------training the train data
train_data=read.csv(args[1], sep="")
mytrain= t(train_data) #transpose data
test_data=read.csv(args[2], sep="")
mytest=t(test_data)

#creating a new col for phenotype
mytrain=cbind(phenotype=rownames(mytrain),data.frame(mytrain,row.names=NULL))
mytrain=separate(data=mytrain,col=phenotype,into= c("cell_type"))
mytrain$cell_type=as.factor(as.character(mytrain$cell_type))

mytest=cbind(phenotype=rownames(mytest),data.frame(mytest,row.names=NULL))
mytest=separate(data=mytest,col=phenotype,into= c("cell_type"))
mytest$cell_type=as.factor(as.character(mytest$cell_type))

#creating standardized gene expression matrix and phenotype y to run LASSO-LR
x_train=as.matrix(mytrain[,-1])
x_Train_standardize= apply(x_train,2,scale) #standardized gene expression
y_train=mytrain$cell_type

x_test=as.matrix(mytest[,-1])
x_Test_standardize= apply(x_test,2,scale)
y_test=mytest$cell_type

#top20genes selected from the given dataset of 164 samples (using LASSO-LR: see Rmd script for details)
Top20genes=c("CD9","CRADD","ELOVL5","FLNB","CD27","PTPRK","CD99","LGALS1","ITPR3","CTGF","PBX1","CD3D","MEIS1","PARP1","AGPS","XBP1","PON2","SH3BP5","HLA.DRA","CD79A")
#subset the input gene matrix to only the top 20 selected genes
x_Train20=x_Train_standardize[,colnames(x_Train_standardize)%in%Top20genes]

# finding best lambda using 10 fold CV in the training data set 
set.seed(123)
cvfit=cv.glmnet(x_Train20, y_train, family="multinomial", type.multinomial = "grouped", parallel=TRUE,type.measure = "class",alpha = 1,standardize =FALSE)
#fitting these 20genes to the held-out input data with LASSO-LR
fit_20<- glmnet(x_Train20, y_train, family="multinomial", type.multinomial = "grouped",alpha = 1,standardize = FALSE,lambda =cvfit$lambda.1se)

#----------------------------------------predicting cell subtypes from the model fit using 20 genes above for the new test data input
x_Test20=x_Test_standardize[,colnames(x_Test_standardize)%in%Top20genes]
prediction_20=predict(fit_20,newx = x_Test20,type="class")

#----------------------------------------------write out predicted leukemia subtypes
Names=matrix(c("Sample_Number","Predicted_Cell_Type"),nrow=1,ncol = 2)
write.table(Names,file=args[3],quote = F,col.names = F,row.names = F)
write.table(prediction_20,file=args[3],quote = F,col.names = F,row.names = T,append = T)
cat("Done! Thank you...\n")
