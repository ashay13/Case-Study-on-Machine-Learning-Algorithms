set.seed(123)
k = 4
i = 30

#Import Dataset - Ionosphere
ionosphere <- read_csv("C:/Users/ASHAY/Desktop/Fall 2017/Assignments/AML/Assignment 1/ionosphere.csv", 
                       col_names = FALSE)
clean_ionosphere_data <- subset(ionosphere, select = -c(35) )
ionosphere_matrix = data.matrix(clean_ionosphere_data)

#Calculate Euclidean Distance function
calculate_Euclidean_Dist = function(A,centroid){
  sum = 0
  for(i in 1:(length(A))){
    sum = (A[i]-centroid[i])^2 + sum
  }
  
  return (sum)
}

matequal <- function(x, y){
  x = unname(x,force = TRUE)
  if(length(x) == length(y)){
    flag = TRUE
    for(i in 1:length(x)){
      if(isTRUE(all.equal(x[i],y[i]))){
        
      }else{
        return(FALSE)
      }
    }
  }else{
    return(FALSE)
  }
  return(flag)
}
  
#This function will return next Centroid Vector
find_next_centroid = function(A,centroid,n){
  cat('Iteration - ',n,'\n')
  min_point_centroid_dist = rep(0,nrow(A))
  possible_centroid = matrix(nrow = nrow(A) , ncol = (ncol(A)))
  k = 1
  for(i in 1:nrow(A)){
    repeat_flag=0
    dist_nearest_cluster = rep(0,n)
    for(j in 1:n){
      if(matequal(A[i,],centroid[j,])){
        repeat_flag = 1
        break
      }
      dist_nearest_cluster[j]=calculate_Euclidean_Dist(A[i,],centroid[j,])
    }
    if(repeat_flag == 1){
      next
    }
    min_point_centroid_dist[k]=dist_nearest_cluster[which.min(dist_nearest_cluster)]
    possible_centroid[k,] = A[i,]
    k = k + 1
  }
  
  sum_of_points = sum(min_point_centroid_dist)
  probability = (sum_of_points)^-1 * min_point_centroid_dist
  return(possible_centroid[which.max(probability),])
}



evaluateKMeansPlusPlus = function(A,k,i){
  centroid = matrix(nrow = k , ncol = (ncol(A)))
  #centroid[1,] = A[sample(nrow(A), 1),]
  centroid[1,] = A[1,]
  for(i in 2:k){
    #probability = rep(0,nrow(A)-i+1)
    centroid[i,] = find_next_centroid(A,centroid,i-1)
  }
  View(centroid)
  
}


evaluateKMeansPlusPlus(ionosphere_matrix,k,i)