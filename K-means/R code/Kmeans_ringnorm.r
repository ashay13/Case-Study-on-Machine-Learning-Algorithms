#Please enter number of i: Iteration & n: number of Clusters
k = 2
i = 500

#Install Libraries
install.packages("xlsx")
library(readr)
library(xlsx)

#Import Dataset - Ringnorm
ringnorm_data <- read_csv("C:/Users/ASHAY/Desktop/Fall 2017/Assignments/AML/Assignment 1/ringnorm_data.csv", col_names = FALSE)
ringnorm_matrix = data.matrix(ringnorm_data)


#Calculate Euclidean Distance function
calculate_Euclidean_Dist = function(A,centroid){
  sum = 0
  
  for(i in 2:length(A)){
    sum = (A[i]-centroid[i-1])^2 + sum
  }
  
  return (sqrt(sum))
}

assignLable = function(cluster_dist){
  return(which.min(cluster_dist))
}

#check distance between a point and centroid1, centroid2.
#Assign point to closest centroid by updating label
updateCentroid = function(A,label,centroid){
  
  new_label = rep(NA,nrow(A))
  #Assign labels based on minimum euclidean disatance
  for (i in 1:nrow(A)) {
    cluster_dist = rep(0,nrow(centroid))
    for(j in 1:nrow(centroid)){
      cluster_dist[j] = calculate_Euclidean_Dist(A[i,],centroid[j,])
    }
    
    new_label[i] = assignLable(cluster_dist)
    
  }
  return(new_label)
}

#A: Dataset, k: number of clusters, i: number of iterations
evaluateKMeans = function(A,k,i){
  old_label = rep(0,nrow(A))
  new_label = sample(k,nrow(A),TRUE)
  iter = 1
  centroid = 0
  while((!all(new_label == old_label)) & (iter <= i)){
  old_label = new_label
  centroid = calculate_Centroid(A,old_label,k)
  new_label = updateCentroid(A,old_label,centroid)
  cat('Iteration - ',iter,'\n')
  
  iter = iter + 1
  }
  cat('**** Final Centroids ****\n')
  
  label_matrix = c(nrow(k))
  for(i in 1:length(new_label)){
    for(j in 1:k){
      if(new_label[i] == j){
        label_matrix[j] = paste(label_matrix[j],i,sep = ", ")
      }
    }  
    
  }
  write.xlsx(centroid, "Ringnorm_Centroids.xlsx")
  write.xlsx(label_matrix, "Cluster20.xlsx") 
  }

#Calculate values of centroid
#Input: A: Dataset of Matrix type, label: cluster_labels, k: number of clusters 
calculate_Centroid = function(A,label,k){
  
  centroid = matrix(nrow = k , ncol = (ncol(A))-1)
  for (i in 1:k) {
    for(j in 2:ncol(A)){
      centroid[i,(j-1)] = mean(A[label==i,j])
    }  
  }
  return(centroid)
}

#Execute Algorithm
evaluateKMeans(ringnorm_matrix,k,i)