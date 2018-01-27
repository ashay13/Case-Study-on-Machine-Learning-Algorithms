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
getDataSet = function() {
  ringnorm = read_csv("http://mldata.org/repository/data/download/csv/ringnorm-ida/",
                      col_names = FALSE)
  return(ringnorm)
}

multivariateNormalDistribution = function(x, mean, co, prior) {
  dmv = dmvnorm(x = x, mean = mean, sigma = co)
  return(dmv * prior)
}

findWeightedProbability = function(j,
                                   i,
                                   ringnorm_matrix,
                                   clusterMean,
                                   covarianceMatrix,
                                   priors) {
  mvnd_currentCluster = 0
  mvnd_allCluster = 0
  for (a in 1:nrow(clusterMean)) {
    mvnd = multivariateNormalDistribution(ringnorm_matrix[j, ],
                                          clusterMean[a, ],
                                          covarianceMatrix[[a]],
                                          priors[a])
    if (a == i) {
      mvnd_currentCluster = mvnd
    }
    mvnd_allCluster = mvnd + mvnd_allCluster
  }
  
  f = mvnd_currentCluster / mvnd_allCluster
  
  return(f)
}

updateClusterMean = function(i, weightedProbability, ringnorm_matrix) {
  numerator = 0
  denominator = 0
  
  numerator = sum(weightedProbability[, i] * ringnorm_matrix[i, ])
  denominator = sum(weightedProbability[, i])
  
  return(numerator / denominator)
}


findMeanDifference = function(clusterMean_old, clusterMean_new) {
  return(sum((clusterMean_new - clusterMean_old) ^ 2))
}


EvaluateEMAlgorithm = function(ringnorm_matrix, k, threshold) {
  dimension = ncol(ringnorm_matrix)
  
  #Initialize parameters
  
  #Cluster Mean
  clusterMean_new = matrix(nrow = k, ncol = dimension)
  clusterMean_old = matrix(nrow = k, ncol = dimension)
  clusterMean_new = ringnorm_matrix[sample(nrow(ringnorm_matrix), size =
                                             k, replace = TRUE), ]
  
  #Covariance Matrix for each cluster
  covarianceMatrix = rep(list(data.matrix(diag(dimension))), k)
  
  #Uniformly distributed prior probabilities
  priors = rep((1 / k), k)
  
  #Weighted Probability (Wij)
  weightedProbability = matrix(nrow = nrow(ringnorm_matrix), ncol = k)
  
  #threshold
  t = 9999999
  iter = 1
  
  while (t > threshold) {
    clusterMean_old = clusterMean_new
    
    #Begin Expectation Step
    for (i in 1:k) {
      for (j in 1:nrow(ringnorm_matrix)) {
        weightedProbability[j, i] = findWeightedProbability(j,
                                                            i,
                                                            ringnorm_matrix,
                                                            clusterMean_old,
                                                            covarianceMatrix,
                                                            priors) + 0.00000005
      }
    }
    
    #Begin Maximization Step
    
    #Re-estimate cluster means
    for (i in 1:k) {
      clusterMean_new[i, ] = updateClusterMean(i, weightedProbability, ringnorm_matrix)
    }
    
    #Compute cluster mean difference
    t = findMeanDifference(clusterMean_old, clusterMean_new)
    #cat("Mean difference = ",t,"\n")
    iter = iter + 1
  }
  
  ## EM Alogorithm is successfully. Now, let's find labels for each data
  cluster_label = rep(0, nrow(ringnorm_matrix))
  for (i in 1:nrow(weightedProbability)) {
    cluster_label[i] = which.max(weightedProbability[i, ])
  }
  
  returnList = list("clusterLabel" = cluster_label,
                    "iter" = iter - 1,
                    "mean" = clusterMean_new)
  return(returnList)
}

#Calculate Euclidean Distance function
calculate_Euclidean_Dist = function(TrueMean, PredictedMean) {
  sum = 0
  for (i in 1:(length(PredictedMean))) {
    sum = (TrueMean[i] - PredictedMean[i]) ^ 2 + sum
  }
  
  return (sqrt(sum))
}

#Calculate Error when K=2
findErrorForK2 = function(trueLabels, clusterLabel) {
  #cluster_error = rep(0,2)
  Total_error = 0
  for (i in 1:2) {
    plus = 0
    minus = 0
    for (j in 1:nrow(trueLabels)) {
      if (clusterLabel[j] == i) {
        if (trueLabels[j, ] == -1) {
          minus = minus + 1
        } else if (trueLabels[j, ] == 1) {
          plus = plus + 1
        }
      }
    }
    
    if (plus != 0 & minus != 0) {
      #Find Cluster-wise error
      if (plus > minus) {
        error = minus / (plus + minus)
        #cluster_error[i] = error
        Total_error = Total_error + error
      } else{
        error = plus / (plus + minus)
        #cluster_error[i] = error
        Total_error = Total_error + error
      }
    }
  }
  return(Total_error)
}

#This Function Calculates Number of Good and Bad in clusters
calculateNumberOfPlusAndMinus = function(k, clusterLabel, trueLabels) {
  plus = 0
  minus = 0
  for (i in 1:length(clusterLabel)) {
    if (clusterLabel[i] == k) {
      if (trueLabels[i, ] == -1) {
        minus = minus + 1
      } else if (trueLabels[i, ] == 1) {
        plus = plus + 1
      }
    }
  }
  returnList = list("plus" = plus, "minus" = minus)
  return(returnList)
}


#Calculate Error for K = 3,4,5
findErrorForOtherK = function(truePlus,
                              trueMinus,
                              predictMean,
                              clusterLabel,
                              trueLabels) {
  #TotalError = 0
  newClusterLabel = rep(0, length(clusterLabel))
  
  for (k in 1:nrow(predictMean)) {
    plus_dist = calculate_Euclidean_Dist(truePlus, predictMean[k, ])
    minus_dist = calculate_Euclidean_Dist(trueMinus, predictMean[k, ])
    
    if (plus_dist > minus_dist) {
      for (i in 1:length(clusterLabel)) {
        if (clusterLabel[i] == k) {
          newClusterLabel[i] = 1
        }
      }
    }
    else if (plus_dist < minus_dist) {
      for (i in 1:length(clusterLabel)) {
        if (clusterLabel[i] == k) {
          newClusterLabel[i] = 2
        }
      }
    }
  }
  TotalError = findErrorForK2(trueLabels = trueLabels, clusterLabel = newClusterLabel)
  
  return(TotalError)
}

#=======================================================================================================================================
# Let's start K-means Algorithm on ringnorm Dataset.
#

#This function finds minimum distance between a point and cluster and return the label
assignLable = function(cluster_dist) {
  return(which.min(cluster_dist))
}

#check distance between a point and centroids.
#Assign point to closest centroid by updating label
updateCentroid = function(A, label, centroid) {
  #Assign labels based on minimum euclidean disatance
  for (i in 1:nrow(A)) {
    cluster_dist = rep(0, nrow(centroid))
    for (j in 1:nrow(centroid)) {
      cluster_dist[j] = calculate_Euclidean_Dist(A[i, ], centroid[j, ])
    }
    
    label[i] = assignLable(cluster_dist)
    
  }
  return(label)
}

#This function will randomly initialize k number of Centroids
initialize_Centroid = function(A, k) {
  centroid = A[sample(nrow(A), k), ]
  return(centroid)
}

#A: Dataset, k: number of clusters, i: number of iterations
evaluateKMeans = function(A, k, i) {
  label = rep(0, nrow(A))
  #new_label = sample(k,nrow(A),TRUE)
  old_centroid = matrix(nrow = k , ncol = (ncol(A)))
  new_centroid = initialize_Centroid(A, k)
  iter = 1
  while ((!identical(new_centroid, old_centroid)) & (iter <= i)) {
    old_centroid = new_centroid
    label = updateCentroid(A, label, old_centroid)
    
    new_centroid = calculate_Centroid(A, label, k)
    iter = iter + 1
  }
  
  label_matrix = c(nrow(k))
  for (i in 1:length(label)) {
    for (j in 1:k) {
      if (label[i] == j) {
        label_matrix[j] = paste(label_matrix[j], i)
      }
    }
  }
  returnList = list("clusterLabel" = label,
                    "iter" = iter - 1,
                    "mean" = new_centroid)
  return(returnList)
}

#Calculate values of centroid
#Input: A: Dataset of Matrix type, label: cluster_labels, k: number of clusters
calculate_Centroid = function(A, label, k) {
  centroid = matrix(nrow = k , ncol = (ncol(A)))
  for (i in 1:k) {
    for (j in 1:ncol(A)) {
      centroid[i, j] = mean(A[label == i, j])
    }
  }
  return(centroid)
}

#########################################################################################################

# Lets start with comparing K-means clustering algorithm & EM Algorithm
#
# First Download the Dataset and preprocess it.
ringnorm = getDataSet()
ringnorm_matrix = data.matrix(subset(ringnorm, select = -c(1)))
ringnorm_label_matrix = as.vector(subset(ringnorm, select = c(1)))

# Here I'm calculating true centroids based on labels
ringnorm = as.data.frame(ringnorm)

plus = ringnorm[ringnorm$X1 == 1,]
minus = ringnorm[ringnorm$X1 == -1,]

plus = data.matrix(subset(plus, select = -c(1)))
true_plus = colMeans(plus)

minus = data.matrix(subset(minus, select = -c(1)))
true_minus = colMeans(minus)

#Reporting Variables initialization
All.iter = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Algorithm", "K", "Iteration"))
All.error = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Algorithm", "K", "Error"))


####################################################################################################

# 1. First evaluate EM Algorithm for k=2,3..5 20 Runs Each. Let's set Threshold value to 0.0005
threshold = 0.0001

EM_iter = matrix(nrow = 4 , ncol = 20)
EM_error = matrix(nrow = 4 , ncol = 20)
for (k in 2:5) {
  cat("EM k = ", k)
  for (run in 1:20) {
    cat("EM run = ", run)
    returnList = EvaluateEMAlgorithm(ringnorm_matrix, k, threshold)
    
    cluster_label = returnList$clusterLabel
    iter = returnList$iter
    clusterMean = returnList$mean
    
    EM_iter[k - 1, run] = iter
    
    if (k == 2) {
      EM_error[k - 1, run] = findErrorForK2(ringnorm_label_matrix, cluster_label)
    }
    else{
      EM_error[k - 1, run] = findErrorForOtherK(true_plus,
                                                true_minus,
                                                clusterMean,
                                                cluster_label,
                                                ringnorm_label_matrix)
    }
  }
}

cat("******************* Number of Iterations in EM - ", EM_iter)
cat("******************* Error in EM - ", EM_error)


####################################################################################################

# 2. Now evaluate K-means Algorithm for k=2,3..5 20 Runs Each.

KM_iter = matrix(nrow = 4 , ncol = 20)
KM_error = matrix(nrow = 4 , ncol = 20)
for (k in 2:5) {
  cat("KM k = ", k)
  for (run in 1:20) {
    cat("KM run = ", run)
    returnKmeans = evaluateKMeans(ringnorm_matrix, k, 60)
    
    cluster_label = returnKmeans$clusterLabel
    iter = returnKmeans$iter
    clusterMean = returnKmeans$mean
    
    KM_iter[k - 1, run] = iter
    
    if (k == 2) {
      KM_error[k - 1, run] = findErrorForK2(ringnorm_label_matrix, cluster_label)
    }
    else{
      KM_error[k - 1, run] = findErrorForOtherK(true_plus,
                                                true_minus,
                                                clusterMean,
                                                cluster_label,
                                                ringnorm_label_matrix)
    }
  }
}

cat("******************* Number of Iterations in K-Means - ", KM_iter)
cat("******************* Error in K-Means - ", KM_error)



#______________________________________________________________________________________________________________

##Whisker plot for Iteration

trans1 = t(EM_iter)
colnames(trans1) <- c("K2", "K3", "K4", "K5")
temp1 = melt(trans1)
colnames(temp1) <- c("Run", "Cluster", "Iteration")
temp1 = data.frame(temp1)
temp1$Algorithm <- "EM"

trans2 = t(KM_iter)
colnames(trans2) <- c("K2", "K3", "K4", "K5")
temp2 <- melt(trans2)
colnames(temp2) <- c("Run", "Cluster", "Iteration")
temp2 <- data.frame(temp2)
temp2$Algorithm <- "K-Means"
temp3 = rbind(temp1, temp2)

ggplot(data = temp3, aes(x = Cluster, y = Iteration)) + geom_boxplot(aes(fill =
                                                                           Algorithm))


#-----------------------------------------------------------------------------------------

##Whisker plot for Error

trans3 = t(EM_error)
colnames(trans3) <- c("K2", "K3", "K4", "K5")
temp3 = melt(trans3)
colnames(temp3) <- c("Run", "Cluster", "Error")
temp3 = data.frame(temp3)
temp3$Algorithm <- "EM"

trans4 = t(KM_error)
colnames(trans4) <- c("K2", "K3", "K4", "K5")
temp4 <- melt(trans4)
colnames(temp4) <- c("Run", "Cluster", "Error")
temp4 <- data.frame(temp4)
temp4$Algorithm <- "K-Means"
temp5 = rbind(temp3, temp4)

ggplot(data = temp5, aes(x = Cluster, y = Error)) + geom_boxplot(aes(fill =
                                                                       Algorithm))