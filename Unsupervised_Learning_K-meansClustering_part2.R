#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE) #to pass the arguments from the command line
#-----------loading library tidyverse:
suppressPackageStartupMessages(library(tidyverse))
#----------loading data 
input <- read.csv(args[1], sep="")
data= t(input)
#------------writing euclidian function to calculate distance

euclid <- function(data,centroid) {
  distanceMatrix <- matrix(NA, nrow=dim(data)[1], ncol=dim(centroid)[1])
  for(i in 1:nrow(centroid)) {
    for(j in 1:nrow(data)){
      distanceMatrix[j,i] <- sqrt(rowSums((data[j,]-centroid[i,])^2))
    }
  }
  distanceMatrix
}
####-----------------------------------------Main K-means function for 25 random seedings
obj_function=c()
clusters_25=data.frame(matrix(ncol = 25,nrow = nrow(data))) #creating a dataframe for the 25 random seedings with "NA" to be updated each iteration
best_clusters=data.frame(matrix(ncol = 25,nrow = nrow(data))) #a dataframe to store best cluster per random seeding
for (r in 1:25){
  clusters=sample(1:7,nrow(data),replace = T) #generating random seeding for 7 clusters
  clusters_25[r]=clusters #updating the above random cluster info to the big cluster list of 25 cols
  centroid=aggregate(data,by=clusters_25[r],mean)[,-1] #estimating centroids
  diff_class_assignment= 5 #random number larger than 0 to start the while loop
  i=0 #iteration
  cluster_inOneRandomSeeding=clusters_25[r] #inital seeding to start the k means algorithm
while ( diff_class_assignment > 0){
  i<- i+1
  Eucli_distance=euclid(data,centroid)
  clusters_new=apply(Eucli_distance,1,which.min)
  cluster_inOneRandomSeeding<- cbind(cluster_inOneRandomSeeding,clusters_new) #candidate clusters before the algorithm converges
  diff_class_assignment=sum(abs(cluster_inOneRandomSeeding[i+1]-cluster_inOneRandomSeeding[i])) #assessing if the class assignment is still changing
  centroid=aggregate(data,by=cluster_inOneRandomSeeding[i+1],mean)[,-1] #calculating centroid for the updated cluster
  cat("doing while loop iteration",i,"\n")
}
best_clusters[r]=clusters_new #storing the optimal cluster in each iteration of random intial seeding
obj_function=append(obj_function,mean(apply(Eucli_distance,1,min)))
r=r+1 #continue to next loop of random seeding
cat("doing for loop",r,"\n")
}
min=which.min(obj_function) #find the order of min obj function in the list of all 25 obj functions per random seeding
final_cluster=best_clusters[min] #the index of best cluster corresponds to the index of best obj function
write.table(final_cluster,file=args[2],row.names = F,col.names = F,quote = F)
write.table(min(obj_function),file=args[2],row.names = F,col.names = F,quote = F,append = T)



