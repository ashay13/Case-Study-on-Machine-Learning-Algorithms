#Install Libraries if required
#install.packages("xlsx")
library(readr)
library(xlsx)

#Calculate Euclidean Distance function
calculate_Euclidean_Dist = function(A,centroid){
  sum = 0
  for(i in 1:(length(A))){
    sum = (A[i]-centroid[i])^2 + sum
  }
  
  return (sqrt(sum))
}

#This function finds minimum distance between a point and cluster and return the label
assignLable = function(cluster_dist){
  return(which.min(cluster_dist))
}

#check distance between a point and centroids.
#Assign point to closest centroid by updating label
updateCentroid = function(A,label,centroid){
  
  #Assign labels based on minimum euclidean disatance
  for (i in 1:nrow(A)) {
    cluster_dist = rep(0,nrow(centroid))
    for(j in 1:nrow(centroid)){
      cluster_dist[j] = calculate_Euclidean_Dist(A[i,],centroid[j,])
    }
    
    label[i] = assignLable(cluster_dist)
    
  }
  return(label)
}

#This function will randomly initialize k number of Centroids 
initialize_Centroid = function(A,k){
  centroid = A[sample(nrow(A), k),]
  return(centroid)
}

#A: Dataset, k: number of clusters, i: number of iterations
evaluateKMeans = function(A,k,i){
  label = rep(0,nrow(A))
  #new_label = sample(k,nrow(A),TRUE)
  old_centroid = matrix(nrow = k , ncol = (ncol(A)))
  new_centroid = initialize_Centroid(A,k)
  iter = 1
  while((!identical(new_centroid,old_centroid)) & (iter <= i)){
    old_centroid = new_centroid
    label = updateCentroid(A,label,old_centroid)
    
    new_centroid = calculate_Centroid(A,label,k)
    #cat('Iteration - ',iter,'\n')
    iter = iter + 1
  
    
  }
  
  label_matrix = c(nrow(k))
  for(i in 1:length(label)){
    for(j in 1:k){
      if(label[i] == j){
        label_matrix[j] = paste(label_matrix[j],i)
      }
    }  
    
  }
  #write.xlsx(new_centroid,"Lloyd_Ionosphere_Centroid.xlsx")
  #write.xlsx(label_matrix, "Lloyd_Ionosphere_Cluster.xlsx")
  return(label)
}

#Calculate values of centroid
#Input: A: Dataset of Matrix type, label: cluster_labels, k: number of clusters 
calculate_Centroid = function(A,label,k){
  
  centroid = matrix(nrow = k , ncol = (ncol(A)))
  for (i in 1:k) {
    for(j in 1:ncol(A)){
      centroid[i,j] = mean(A[label==i,j])
    }  
  }
  return(centroid)
}

#This function will Calculate Clustering Error for each cluster and it will return total error.
calculate_error = function(ionosphere_label_matrix, centroid_label,k){

  cluster_error = rep(0,k)
  Total_error = 0
  for(i in 1:k){
    b=0
    g=0
    for(j in 1:nrow(ionosphere_label_matrix)){
      if(centroid_label[j] == i){
        if(isTRUE(all.equal.character(as.character(ionosphere_label_matrix[j,]),'b'))){
          b = b + 1
        }else if(isTRUE(all.equal.character(as.character(ionosphere_label_matrix[j,]),'g'))){
          g = g + 1
        }
    }
    }
    #Find Cluster-wise error
    if(g > b){
      error = b/(g+b)
      cluster_error[i] = error
      Total_error = Total_error+error
    }else{
      error = g/(g+b)
      cluster_error[i] = error
      Total_error = Total_error+error
    }
    
    
  }
  cat('Total Error for k = ',k,' is - ',Total_error,'\n')
  cat('Cluster-wise Error for k = ',k,' is ',cluster_error,'\n')
  return(Total_error)
}

#Execute Algorithm
Kmeans_Algorithm_with_Error = function(num_cluster,iter,runs){
  
  #Import Dataset - Ionosphere
  ionosphere <- read_csv("C:/Users/ASHAY/Desktop/Fall 2017/Assignments/AML/Assignment 1/ionosphere.csv", col_names = FALSE, col_types = cols(X35 = col_character()))
  ionosphere_matrix = data.matrix(subset(ionosphere, select = -c(35)))
  ionosphere_label_matrix = as.vector(subset(ionosphere, select = c(35)))
  
  all_error_matrix = matrix(nrow = num_cluster-1,ncol = runs)
  for(i in 2:num_cluster){
    for(j in 1:runs){
      centroid_label = evaluateKMeans(ionosphere_matrix,i,iter)
      total_error = calculate_error(ionosphere_label_matrix, centroid_label,i)
      all_error_matrix[i-1,j] = total_error
    }
    plot(all_error_matrix[i-1,],
         type="b", pch = 19, frame = FALSE, 
         xlab="Runs",
         ylab="Total Error",main = paste("Total Error for Number of Clusters(K) = ",i),
         xaxt="n")
    axis(1, at = seq(1, runs, by = 1), las=2)
  }
  
}

#Call this Function to execute Lloyd's Kmeans Algorithm & Generate Plot to observe Variation in Error with various cluster size
#It expects 2 argument (Max size of Cluster, Number of Iterations)
Kmeans_Algorithm_with_Error(5,30,20)