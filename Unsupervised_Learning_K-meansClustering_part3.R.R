#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE) #to pass the arguments from the command line
#-----------loading library tidyverse:
suppressPackageStartupMessages(library(tidyverse))

#----------loading data
input_data_example1 <- read.csv(args[1], sep="")
data= t(input_data_example1)

#------------------------------------writing euclidian function to calculate distance
euclid <- function(data,centroid) {
  distanceMatrix <- matrix(NA, nrow=dim(data)[1], ncol=dim(centroid)[1])
  for(i in 1:nrow(centroid)) {
    for(j in 1:nrow(data)){
      distanceMatrix[j,i] <- sqrt(rowSums((data[j,]-centroid[i,])^2))
    }
  }
  distanceMatrix
}
#-------------------------------------k-means function
k_means <- function(data,centroid) {
i=0
diff_class_assignment= 5 #random number larger than 0 to start the while loop
clusters=as.numeric(sample(1:2,nrow(data),replace = T)) #just some random number : doesnt mean anything
while ( diff_class_assignment > 0){
  i<- i+1
  Eucli_distance=euclid(data,centroid)
  clusters_new=apply(Eucli_distance,1,which.min)
  clusters<- cbind(clusters,clusters_new)
  clusters<- as.data.frame(clusters)
  diff_class_assignment=sum(abs(clusters[i+1]-clusters[i]))
  centroid=aggregate(data,by=clusters[i+1],mean)[,-1]
  cat("k-means iter",i,"\n")
}
obj_function=mean(apply(Eucli_distance,1,min)) #obj_function
results=append(clusters_new,obj_function)
return(results) #final cluster and obj_function(results[nrows+1])
}
####------------------------------------------------------------------Main GLOBAL K-means function--assuming k=4
k=1
centroid=apply(data, 2, mean) #inital centroid when k=1 (the average of all the data points)
clusters_K2K3K4=data.frame(matrix(ncol = 3,nrow = nrow(data))) #to store final clusters for all K

while (k < 4){ #giving kclusters=4; the start of this loop is doing k=2
  obj_function=c()
  clusters_candidates=data.frame(matrix(ncol = nrow(data),nrow = nrow(data)))
  for (n in 1:nrow(data)) { 
    centroid_candidate=data[n,] #try every data point as a potential centroid 
    centroid_combined=as.data.frame(rbind(centroid,centroid_candidate)) #combinding centroid candidate to the orginal centroid (k+1 rows)
    Doing_Kmeans=k_means(data,centroid_combined) #doing kmeans for each centroid/data point candidate
    obj_function=append(obj_function,Doing_Kmeans[nrow(data)+1]) #collecting obj_function for all data points 
    clusters_final=Doing_Kmeans[1:nrow(data)] #final cluster from k-means (one n at a time)
    clusters_candidates[n]=clusters_final #each best cluster for each n
    cat("This is sample candidate",n, "\n")
  }
  min_obj=which.min(obj_function) #select the cluster that gives lowest obj function
  clusters_K2K3K4[k]=clusters_candidates[min_obj] #updating to final list of clusters for each k
  centroid_update=aggregate(data,by=clusters_K2K3K4[k],mean)[,-1]
  centroid=centroid_update #updating centroid with this new cluster and continue the loop till k>4
  k=k+1
  cat ("***************That was k",k,"*****************\n")
}
final_clusters=clusters_K2K3K4[3] #extracting the final clusters of k=4
FINALobj_function=mean(apply(euclid(data,centroid),1,min)) #calculate final obj_function
##writing out results
write.table(final_clusters,file=args[2],row.names = F,col.names = F,quote = F)
write.table(FINALobj_function,file=args[2],row.names = F,col.names = F,quote = F,append = T)

