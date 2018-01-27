#Credit Approval Dataset
#Divide Data into 5 folds
library(readr)
crx <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
                col_names = FALSE, col_types = cols(X11 = col_number(), 
                                                    X14 = col_number()))
crx[crx == '?'] = NA

crx = na.omit(crx)
crx$X2 = as.double(crx$X2)
crx = crx[-which(crx$X7 == 'o'), ]
crx = crx[-which(crx$X13 == 'p'), ]
crx = crx[-which(crx$X4 == 'l'), ]

crx$X1 = as.integer(factor(crx$X1))
crx$X4 = as.integer(factor(crx$X4))
crx$X5 = as.integer(factor(crx$X5))
crx$X6 = as.integer(factor(crx$X6))
crx$X7 = as.integer(factor(crx$X7))
crx$X9 = as.integer(factor(crx$X9))
crx$X10 = as.integer(factor(crx$X10))
crx$X12 = as.integer(factor(crx$X12))
crx$X13 = as.integer(factor(crx$X13))


temp = sample(x = seq(1,nrow(crx),1),replace = FALSE)
point = seq(130,520,130)
folds = list()
for(i in 1:4){
  folds[[i]] = crx[temp[(point[i]-129):point[i]],]
}
folds[[5]] = crx[temp[521:649],]

#Create Training And Test Dataset

training = list()
testing = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  testing[[i]] = folds[[i]]
  training[[i]] = rbind(folds[[f[1]]],folds[[f[2]]],folds[[f[3]]],folds[[f[4]]])
}

#______________________________________________________________________________________________

#Calculate Euclidean Distance function
calculate_Euclidean_Dist = function(train,test){
  
  #test_matrix = matrix(data = test,byrow = TRUE,nrow = nrow(train),ncol = length(test))
  distance_vector = apply(train,1,function(x)sqrt(sum((x-test)^2)))
  return (distance_vector)
}

#Calculate Manhattan Distance function
calculate_Manhattan_Dist = function(train,test){
  
  distance_vector = apply(train,1,function(x)sum(abs(x-test)))
  return (distance_vector)
}

KNN_algorithm_normal_voting_Manhattan = function(trainingData, trainingLabel, testData,k){
  
  testlabels = rep(0,nrow(testData))
  for(i in 1:nrow(testData)){
    print(i)
    distvector = calculate_Manhattan_Dist(train = trainingData,test = testData[i,])
    index = which(distvector %in% sort(distvector)[1:k])
    
    #Distance-weighted Voting
    klabel = trainingLabel[index,]
    
    testlabels[i] = names(which(table(klabel) == max(table(klabel))))
  }
  return(testlabels)
}

KNN_algorithm_normal_voting_Euclidean = function(trainingData, trainingLabel, testData,k){
  testlabels = rep(0,nrow(testData))
  for(i in 1:nrow(testData)){
    print(i)
    distvector = calculate_Euclidean_Dist(train = trainingData,test = testData[i,])
    index = which(distvector %in% sort(distvector)[1:k])
    
    #Distance-weighted Voting
    klabel = trainingLabel[index,]
    
    testlabels[i] = names(which(table(klabel) == max(table(klabel))))
  }
  return(testlabels)
}



#______________________________________________________________________________________________________________

#Start with loop which will iterate over 5 different combinations of training and testing dataset and simultneously calculate error
nn = c(1,9,19,29,49)
result_matrix_euclidean = matrix(ncol = 5,nrow = 5)
result_matrix_manhattan = matrix(ncol = 5,nrow = 5)
row = 0
for(k in nn){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    cat("fold = ",i,"\n")
    train_data = training[[i]]
    train_label = train_data[,16]
    train_data = train_data[,1:15]
    test_data = testing[[i]]
    test_label = test_data[,16]
    test_data = test_data[,1:15]
    
    #Weighted Voting Euclidean
    myTestLabel = KNN_algorithm_normal_voting_Euclidean(trainingData = train_data,trainingLabel = train_label,testData = test_data,k = k)
    error = mean(test_label != myTestLabel) * 100
    cat("Error = ",error,"\n")
    result_matrix_euclidean[row,i] = error
    
    #Weighted Voting Manhattan
    myTestLabel2 = KNN_algorithm_normal_voting_Manhattan(trainingData = train_data,trainingLabel = train_label,testData = test_data,k = k)
    error2 = mean(test_label != myTestLabel2) * 100
    cat("Error = ",error2,"\n")
    result_matrix_manhattan[row,i] = error2
    
  }  
}
rownames(result_matrix_euclidean) = c("K1","K9","K19","K29","K49")
rownames(result_matrix_manhattan) = c("K1","K9","K19","K29","K49")
boxplot(x = result_matrix_manhattan,use.cols = FALSE,xlab = "Various K Values",ylab = "% Error",main="Error rate for different K with Manhattan Distance")
boxplot(x = result_matrix_euclidean,use.cols = FALSE,xlab = "Various K Values",ylab = "% Error",main="Error rate for different K with Euclidean Distance")
