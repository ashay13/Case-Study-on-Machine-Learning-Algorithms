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

initialize_Centroid = function(A,k){
  centroid = A[sample(nrow(A), k), ]
  return(centroid)
}

#A: Dataset, k: number of clusters, i: number of iterations
evaluateKMeans = function(A,k,i){
  label = rep(0,nrow(A))
  old_centroid = matrix(nrow = k , ncol = (ncol(A)))
  new_centroid = initialize_Centroid(A,k)
  iter = 1
  while((!identical(new_centroid,old_centroid)) & (iter <= i)){
    old_centroid = new_centroid
    label = updateCentroid(A,label,old_centroid)
    
    new_centroid = calculate_Centroid(A,label,k)
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
  return(label)
}

#This function will Calculate Clustering Error for each cluster and it will return total error.
calculate_error = function(breast_cancer_label_matrix, centroid_label,k){
  cluster_error = rep(0,k)
  Total_error = 0
  for(i in 1:k){
    b=0
    m=0
    for(j in 1:nrow(breast_cancer_label_matrix)){
      if(centroid_label[j] == i){
        if(breast_cancer_label_matrix[j,] == 2){
          b = b + 1
        }else if(breast_cancer_label_matrix[j,] == 4){
          m = m + 1
        }
      }
    }
    
    #Find Cluster-wise error
    if(b > m){
      error = m/(b+m)
      cluster_error[i] = error
      Total_error = Total_error+error
    }else{
      error = b/(b+m)
      cluster_error[i] = error
      Total_error = Total_error+error
    }
  }
  cat('Total Error for k = ',k,' is - ',Total_error,'\n')
  #cat('Cluster-wise Error for k = ',k,' is - ',cluster_error,'\n')
  return(Total_error)
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



#Execute Algorithm
Kmeans_Algorithm_with_Error = function(max_cluster,iter,runs){
  
  #Import Dataset - Ionosphere
  breast_cancer_wisconsin <- read_csv("C:/Users/ASHAY/Desktop/Fall 2017/Assignments/AML/Assignment 1/breast-cancer-wisconsin.csv", col_names = FALSE, col_types = cols(X7 = col_integer()))
  clean_data = na.omit(breast_cancer_wisconsin)
  trimmed_data <- subset(clean_data, select = -c(1,11))
  breast_cancer_data_matrix = data.matrix(trimmed_data)
  breast_cancer_label_matrix = as.vector(subset(clean_data, select = c(11)))
  
  all_error_matrix = matrix(nrow = max_cluster-1,ncol = runs)
  for(i in 2:max_cluster){
    for(j in 1:runs){
      centroid_label = evaluateKMeans(breast_cancer_data_matrix,i,iter)
      total_error = calculate_error(breast_cancer_label_matrix, centroid_label,i)
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
Kmeans_Algorithm_with_Error(5,15,20)