calculate_error = function(ionosphere_label_matrix, centroid_label,k){
  
  cluster_error = rep(0,k)
  error_matrix = matrix(nrow = k,ncol = 2)
  Total_error = 0
  for(i in 1:k){
    b=0
    g=0
    for(j in 1:nrow(ionosphere_label_matrix)){
      #cat('Length of Ionosphere Label Matrix ',nrow(ionosphere_label_matrix))
      p = centroid_label[j]
      if(p == i){
        z = as.character(ionosphere_label_matrix[j,])
        if(isTRUE(all.equal.character(z,'b'))){
          b = b + 1
        }else if(isTRUE(all.equal.character(z,'g'))){
          g = g + 1
        }
      }
    }
    error_matrix[i,1] = g
    error_matrix[i,2] = b
    
  }
  
  for(i in 1:k){
    
    g = error_matrix[i,1]
    b = error_matrix[i,2]
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
  
  cat('Total Error for k = ',k,' is - ',Total_error)
  
}