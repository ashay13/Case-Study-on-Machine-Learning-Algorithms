A = matrix(c(1,1,0,5,6,4,4,3,4,1,2,0),ncol=2)

#Function to Plot Graph
plot_Graph = function(data,label,centroid1,centroid2){
  plot(data[,1],data[,2],col=label+1,pch=16,cex=1.5)
  points(centroid1[1],centroid1[2],col=4,pch=10)
  points(centroid2[1],centroid2[2],col=6,pch=10)
  }

#Calculate Euclidean Distance function
calculate_Euclidean_Dist = function(a,b){
  return (sqrt((a[1]-b[1])^2 + (a[2]-b[2])^2))
 }

#check distance between a point and centroid1, centroid2.
#Assign point to closest centroid by updating label
updateCentroid = function(c,label,centroid1,centroid2){
  
  new_label = rep(NA,nrow(c))
  #Assign labels based on minimum euclidean disatance
  for (i in 1:nrow(c)) {
    if(calculate_Euclidean_Dist(c[i,],centroid1) < calculate_Euclidean_Dist(c[i,],centroid2)){
      new_label[i] = 1
    }else{
      new_label[i] = 2
    }  
  }
  return(new_label)
}

#A: Dataset, k: number of clusters, i: number of iterations
evaluateKMeans = function(A,k,i){
  set.seed(123)
  old_label = rep(0,nrow(A))
  new_label = sample(k,nrow(A),TRUE)
  print(new_label)
  iter = 1
  centroid1 = 0
  centroid2 = 0
  while((!all(new_label == old_label)) & (iter <= i)){
    old_label = new_label
    list_centroid = calculate_Centroid(A,old_label,k)
    centroid1 = list_centroid$centroid1
    centroid2 = list_centroid$centroid2
    cat('Iteration ',iter,'\n')
    cat('Centroid 1 - ',centroid1,'\nCentroid 2 - ',centroid2,'\n')
    new_label = updateCentroid(A,label,centroid1,centroid2)
    iter = iter + 1
  }
  cat('**** Final Centroids ****\nCentroid 1 - ',centroid1,'\nCentroid 2 - ',centroid2)
  plot_Graph(A,new_label,centroid1,centroid2)
}

#Calculate values of centroid
#Input: A: Dataset, label: cluster_labels, k: number of clusters 
calculate_Centroid = function(c,label,k){
  
  #Compute centroid 1
  centroid1.x = c[label==1,1]
  centroid1.y = c[label==1,2]
  centroid1 = c(mean(centroid1.x),mean(centroid1.y))
  
  #Compute centroid 2
  centroid2.x = c[label==2,1]
  centroid2.y = c[label==2,2]
  centroid2 = c(mean(centroid2.x),mean(centroid2.y))
  
  newlist = list("centroid1" = centroid1, "centroid2" = centroid2)
  return(newlist)
}

#Execute Algorithm
evaluateKMeans(A,2,20)
