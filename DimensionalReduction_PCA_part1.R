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

#---------calculating variance given the random vector
data_projected=projection(data,random_vector)
var_each=apply(data_projected,2,var)
results=sum(var_each)

#write out the total variance
write.table(results,file=args[3],row.names=F,col.names=F,quote=F)
