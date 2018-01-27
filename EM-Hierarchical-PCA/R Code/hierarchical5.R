set.seed(123)
#Function to find error
findError = function(I_50_labels,clustercut){
  
  cluster_error = rep(0,2)
  Total_error = 0
  
  for(i in 1:2){
    b=0
    g=0
    for(j in 1:50){
      if(clustercut[j] == i){
        if(isTRUE(all.equal.character(as.character(I_50_labels[j,]),'b'))){
          b = b + 1
        }else if(isTRUE(all.equal.character(as.character(I_50_labels[j,]),'g'))){
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
  
  #cat("Total Error Rate : ",Total_error)
  
  return(Total_error)  
}



#Ionosphere Dataset
#Data gathering and Pre-processing
ionosphere = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                      col_names = FALSE, col_types = cols(X35 = col_character()))

ionosphere_matrix = data.frame(ionosphere)

#Sampling 50 records from Ionosphere Dataset
I_50 = ionosphere_matrix[sample(nrow(ionosphere_matrix),size=50,replace=TRUE),]

I_50_data = data.matrix(subset(I_50, select = -c(35)))

I_50_labels = as.vector(subset(I_50, select = c(35)))

distance_matrix = dist(x = I_50_data, method = "euclidean")

cluster = hclust(distance_matrix, method="complete")

#Plot Dendrogram
plot(cluster)

#Cut at clusters = 2
clustercut = cutree(cluster,2)

total_error_before_pca = findError(I_50_labels,clustercut)
cat("Total Error before performing PCA : ",total_error_before_pca)


##### Perform PCA on I_50

pca_I_50 = prcomp(x = I_50_data)
summary(pca_I_50)

#To consider 90% variance - select 9 Principle Components 
pcd = pca_I_50$x

reduced_data = subset(pcd, select = c(1:10))

reduced_data_distance_matrix = dist(x = reduced_data, method = "euclidean")

cluster_after_pca = hclust(reduced_data_distance_matrix, method="complete")

#Plot Dendrogram
plot(cluster_after_pca)

#Cut at clusters = 2
clustercut_after_pca = cutree(cluster_after_pca,2)

total_error_after_pca = findError(I_50_labels,clustercut_after_pca)
cat("Total Error after performing PCA : ",total_error_after_pca)