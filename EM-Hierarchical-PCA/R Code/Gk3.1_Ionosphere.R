#Libraries
#install.packages('readr')
#install.packages('mvtnorm')
#install.packages('ggplot2')
#install.packages('reshape2')
library(reshape2)
library(ggplot2)
library(mvtnorm)
library(readr)


#Import Data
getDataSet = function(){
  ionosphere = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                        col_names = FALSE, col_types = cols(X2 = col_skip(),X35 = col_character()))
  return(ionosphere)
}


multivariateNormalDistribution = function(x,mean,co,prior){
  dmv = dmvnorm(x = x,mean = mean,sigma = co)
  return(dmv*prior)
}

findWeightedProbability = function(j,i,ionosphere_matrix,clusterMean,covarianceMatrix,priors){
  
  mvnd_currentCluster = 0
  mvnd_allCluster = 0
  for(a in 1:nrow(clusterMean)){
    mvnd = multivariateNormalDistribution(ionosphere_matrix[j,],clusterMean[a,],covarianceMatrix[[a]],priors[a])
    if(a == i){
      mvnd_currentCluster = mvnd
    }
    mvnd_allCluster = mvnd + mvnd_allCluster
  }
  
  f = mvnd_currentCluster/mvnd_allCluster
  
  return(f)
}

updateClusterMean = function(i,weightedProbability,ionosphere_matrix){
  numerator = 0
  denominator = 0
  
  numerator = sum(weightedProbability[,i]*ionosphere_matrix[i,])
  denominator = sum(weightedProbability[,i])
  
  return(numerator/denominator)
}

updatePriors = function(i,weightedProbability,n){
  s = sum(weightedProbability[,i])
  p = s/n
  return(p)
}

findMeanDifference = function(clusterMean_old,clusterMean_new){
  return(sum((clusterMean_new - clusterMean_old)^2))
}


EvaluateEMAlgorithm = function(ionosphere_matrix, k,threshold){
  dimension = ncol(ionosphere_matrix)
  
  #Initialize parameters
  
  #Cluster Mean
  clusterMean_new = matrix(nrow = k,ncol = dimension)
  clusterMean_old = matrix(nrow = k,ncol = dimension)
  clusterMean_new = ionosphere_matrix[sample(nrow(ionosphere_matrix),size=k,replace=TRUE),]
  
  #Covariance Matrix for each cluster
  covarianceMatrix = rep(list(data.matrix(diag(dimension))), k)
  
  #Uniformly distributed prior probabilities
  priors = rep((1/k),k)
  
  #Weighted Probability (Wij)
  weightedProbability = matrix(nrow = nrow(ionosphere_matrix),ncol = k)
  
  #threshold
  t = 9999999
  iter = 1
  
  while(t > threshold){
    
    clusterMean_old = clusterMean_new
    
    #Begin Expectation Step
    for(i in 1:k){
      for(j in 1:nrow(ionosphere_matrix)){
        weightedProbability[j,i] = findWeightedProbability(j,i,ionosphere_matrix,clusterMean_old,covarianceMatrix,priors) + 0.00000005
      }
    }
    
    #Begin Maximization Step
    
    #Re-estimate cluster means
    for(i in 1:k){
      clusterMean_new[i,] = updateClusterMean(i,weightedProbability,ionosphere_matrix)
    }
    
    #Re-estimate Covariance Matrix
    for(i in 1:k){
      temp = cov.wt(x = ionosphere_matrix,wt = weightedProbability[,i],center = clusterMean_new[i,])
      covarianceMatrix[[i]] = temp$cov
      
    }
    
    #Re-estimate priors
    for(i in 1:k){
      priors[i] = updatePriors(i,weightedProbability,nrow(ionosphere_matrix))    
    }
    
    #Compute cluster mean difference
    t = findMeanDifference(clusterMean_old,clusterMean_new)
    #cat("Mean difference = ",t,"\n")
    iter = iter + 1
  }
  
  ## EM Alogorithm is successfully. Now, let's find labels for each data
  cluster_label = rep(0,nrow(ionosphere_matrix))
  for(i in 1:nrow(weightedProbability)){
    cluster_label[i] = which.max(weightedProbability[i,])
  }
  
  returnList = list("clusterLabel" = cluster_label, "iter" = iter-1, "mean" = clusterMean_new)
  return(returnList)
}

#Calculate Euclidean Distance function
calculate_Euclidean_Dist = function(TrueMean,PredictedMean){
  sum = 0
  for(i in 1:(length(PredictedMean))){
    sum = (TrueMean[i]-PredictedMean[i])^2 + sum
  }
  
  return (sqrt(sum))
}

#Calculate Error when K=2
findErrorForK2 = function(trueLabels,clusterLabel){
  
  #cluster_error = rep(0,2)
  Total_error = 0
  for(i in 1:2){
    b=0
    g=0
    for(j in 1:nrow(trueLabels)){
      if(clusterLabel[j] == i){
        if(isTRUE(all.equal.character(as.character(trueLabels[j,]),'b'))){
          b = b + 1
        }else if(isTRUE(all.equal.character(as.character(trueLabels[j,]),'g'))){
          g = g + 1
        }
      }
    }
    if(b != 0 & g != 0){
      #Find Cluster-wise error
      if(g > b){
        error = b/(g+b)
        #cluster_error[i] = error
        Total_error = Total_error+error
      }else{
        error = g/(g+b)
        #cluster_error[i] = error
        Total_error = Total_error+error
      }  
    }
  }
  return(Total_error)  
}

#This Function Calculates Number of Good and Bad in clusters 
calculateNumberOfGoodAndBad = function(k,clusterLabel,trueLabels){
  g = 0
  b = 0
  for(i in 1:length(clusterLabel)){
    if(clusterLabel[i] == k){
      if(isTRUE(all.equal.character(as.character(trueLabels[i,]),'b'))){
        b = b + 1
      }else if(isTRUE(all.equal.character(as.character(trueLabels[i,]),'g'))){
        g = g + 1
      }
    }
  }
  returnList = list("good" = g, "bad" = b)
  return(returnList) 
}


#Calculate Error for K = 3,4,5
findErrorForOtherK = function(trueGood,trueBad,predictMean,clusterLabel,trueLabels){
  
  #TotalError = 0
  newClusterLabel = rep(NA,length(clusterLabel))
  
  for(k in 1:nrow(predictMean)){
    good_dist = calculate_Euclidean_Dist(trueGood,predictMean[k,])
    bad_dist = calculate_Euclidean_Dist(trueBad,predictMean[k,])
    
    if(good_dist > bad_dist){
      for(i in 1:length(clusterLabel)){
        if(clusterLabel[i] == k){
          newClusterLabel[i] = 1
        }
      }
    }
    else if(good_dist < bad_dist){
      for(i in 1:length(clusterLabel)){
        if(clusterLabel[i] == k){
          newClusterLabel[i] = 2
        }
      }
    }
  }
  TotalError = findErrorForK2(trueLabels = trueLabels, clusterLabel = newClusterLabel)
  
  return(TotalError)
}

#=======================================================================================================================================
# Let's start K-means Algorithm on Ionosphere Dataset.  
#

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
  returnList = list("clusterLabel" = label, "iter" = iter-1, "mean" = new_centroid)
  return(returnList)}

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

#########################################################################################################

# Lets start with comparing K-means clustering algorithm & EM Algorithm
#
# First Download the Dataset and preprocess it.
ionosphere = getDataSet()
ionosphere_matrix = data.matrix(subset(ionosphere, select = -c(34)))
ionosphere_label_matrix = as.vector(subset(ionosphere, select = c(34)))

# Here I'm calculating true centroids based on labels
ionosphere = as.data.frame(ionosphere)

good = ionosphere[ionosphere$X35 == 'g', ]
bad = ionosphere[ionosphere$X35 == 'b', ]

good = data.matrix(subset(good, select = -c(34)))
true_good = colMeans(good)


#Reporting Variables initialization
All.iter = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Algorithm", "K", "Iteration"))
All.error = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Algorithm", "K", "Error"))



bad = data.matrix(subset(bad, select = -c(34)))
true_bad = colMeans(bad)

####################################################################################################  

# 1. First evaluate EM Algorithm for k=2,3..5 20 Runs Each. Let's set Threshold value to 0.0005
threshold = 0.001

EM_iter = matrix(nrow = 4 , ncol = 20)
EM_error = matrix(nrow = 4 , ncol = 20)
for(k in 2:5){
  cat("EM k = ",k,"\n")
  for(run in 1:20){
    cat("run = ",run)
    returnList = EvaluateEMAlgorithm(ionosphere_matrix,k,threshold)
    
    cluster_label = returnList$clusterLabel
    iter = returnList$iter
    clusterMean = returnList$mean
    
    EM_iter[k-1,run] = iter
    
    if(k == 2){
      EM_error[k-1,run] = findErrorForK2(ionosphere_label_matrix,cluster_label)
    }
    else{
      EM_error[k-1,run] = findErrorForOtherK(true_good,true_bad,clusterMean,cluster_label,ionosphere_label_matrix)
    }
  }
}

cat("******************* Number of Iterations in EM - ",EM_iter)
cat("******************* Error in EM - ",EM_error)


####################################################################################################  

# 2. Now evaluate K-means Algorithm for k=2,3..5 20 Runs Each.

KM_iter = matrix(nrow = 4 , ncol = 20)
KM_error = matrix(nrow = 4 , ncol = 20)
for(k in 2:5){
  cat("KM k = ",k,"\n")
  for(run in 1:20){
    cat("run = ",run)
    
    returnKmeans = evaluateKMeans(ionosphere_matrix,k,20)
    
    cluster_label = returnKmeans$clusterLabel
    iter = returnKmeans$iter
    clusterMean = returnKmeans$mean
    
    KM_iter[k-1,run] = iter
    
    if(k == 2){
      KM_error[k-1,run] = findErrorForK2(ionosphere_label_matrix,cluster_label)
    }
    else{
      KM_error[k-1,run] = findErrorForOtherK(true_good,true_bad,clusterMean,cluster_label,ionosphere_label_matrix)
    }
  }
}

cat("******************* Number of Iterations in K-Means - ",KM_iter)
cat("******************* Error in K-Means - ",KM_error)


#______________________________________________________________________________________________________________  

##Whisker plot for Iteration

trans1 = t(EM_iter)
colnames(trans1) <- c("K2","K3","K4","K5")
temp1 = melt(trans1)
colnames(temp1) <- c("Run","Cluster","Iteration")
temp1 = data.frame(temp1)
temp1$Algorithm <- "EM"

trans2 = t(KM_iter)
colnames(trans2) <- c("K2","K3","K4","K5")
temp2 <- melt(trans2)
colnames(temp2) <- c("Run","Cluster","Iteration")
temp2 <- data.frame(temp2)
temp2$Algorithm <- "K-Means"
temp3 = rbind(temp1,temp2)

ggplot(data = temp3, aes(x=Cluster, y=Iteration)) + geom_boxplot(aes(fill=Algorithm))


#-----------------------------------------------------------------------------------------

##Whisker plot for Error

trans3 = t(EM_error)
colnames(trans3) <- c("K2","K3","K4","K5")
temp3 = melt(trans3)
colnames(temp3) <- c("Run","Cluster","Error")
temp3 = data.frame(temp3)
temp3$Algorithm <- "EM"

trans4 = t(KM_error)
colnames(trans4) <- c("K2","K3","K4","K5")
temp4 <- melt(trans4)
colnames(temp4) <- c("Run","Cluster","Error")
temp4 <- data.frame(temp4)
temp4$Algorithm <- "K-Means"
temp5 = rbind(temp3,temp4)

ggplot(data = temp5, aes(x=Cluster, y=Error)) + geom_boxplot(aes(fill=Algorithm))

