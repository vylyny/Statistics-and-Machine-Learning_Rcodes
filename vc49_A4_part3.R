#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE) #to pass the arguments from the command line
suppressPackageStartupMessages(library(tidyverse))

#----------loading data 
input <- read.delim(args[1])
vector<- read.table(args[2], quote="\"", comment.char="")
random_vector=vector$V1
data=as.matrix(input)

#----------writing function for data projection onto a vector
projection<- function(data,random_vector){
    output=matrix(nrow = nrow(data),ncol = ncol(data))
  for (n in 1:nrow(data)){
    projected_point= ((data[n,]%*%random_vector)/(random_vector%*%random_vector))%*%random_vector
    output[n,]= projected_point
  }
 return(output)
}

#---------objective function
obj_function <- function(data,random_vector){
  data_projected=projection(data,random_vector)
  var_each=apply(data_projected,2,var)
  results=sum(var_each)
  return(-results)
}

#---------function to calculate variance of PC1,PC2,PC3
i=1
out=c()
while (i < 4){
  best.params=optim(par = random_vector,fn = obj_function,data=data)
  best_vector=best.params$par
  projected_on_bestVector=projection(data,best_vector)
  substract=data-projected_on_bestVector
  data=substract
  variance=-(best.params$value)
  out[i]=variance
  i=i+1
} 

#---------write out the total variance of PC1,PC2 and PC3
write.table(out,file=args[3],row.names=F,col.names=F,quote=F)
