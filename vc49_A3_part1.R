#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE) #to pass the arguments from the command line
#-----------loading library tidyverse:
suppressPackageStartupMessages(library(tidyverse))
#----------loading data 
input <- read.csv(args[1], sep="")
data= t(input) #transpose input data to have samples as rows
#------------writing euclidian function to calculate distance from each data point to each centroid

euclid <- function(data,centroid) {
  distanceMatrix <- matrix(NA, nrow=dim(data)[1], ncol=dim(centroid)[1])
  for(i in 1:nrow(centroid)) {
    for(j in 1:nrow(data)){
      distanceMatrix[j,i] <- sqrt(rowSums((data[j,]-centroid[i,])^2))
    }
  }
  distanceMatrix
}
####-----------------------------------------Main K-means function
i=0 #iteration
clusters=read.table(args[2]) #intial cluster
centroid=aggregate(data,by=clusters,mean)[,-1] #calculting the average values of each feature (gene) for each cluster
diff_class_assignment= 5 #random number larger than 0 to start the while loop
while ( diff_class_assignment > 0){ #K-means converges when the class assignment stops changing (i.e difference in class assignment=0)
  i<- i+1
  Eucli_distance=euclid(data,centroid)
  clusters_new=apply(Eucli_distance,1,which.min) #based on the euclidian distance, recluster points to the closest clusters
  clusters<- cbind(clusters,clusters_new)
  diff_class_assignment=sum(abs(clusters[i+1]-clusters[i]))
  centroid=aggregate(data,by=clusters[i+1],mean)[,-1] #recalculate centroid based on new clustering
}
obj_function=mean(apply(Eucli_distance,1,min)) #calculating the objective function using the final cluster
write.table(clusters_new,file=args[3],row.names = F,col.names = F,quote = F)
write.table(obj_function,file=args[3],row.names = F,col.names = F,quote = F,append = T)
cat("Done-part I! Thank you!! \n")
