install.packages("class")
library(class)
library(readr)

#CAR DATASET__________________________________________________________________________________________________
car = read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                     col_names = FALSE)
flag1 = seq(1,5,1)
clean_data = cbind(as.integer(factor(car$X1)),as.integer(factor(car$X2)),as.integer(factor(car$X3)),as.integer(factor(car$X4)),as.integer(factor(car$X5)),as.integer(factor(car$X6)),as.integer(factor(car$X7)))
ind = seq(1,nrow(clean_data),1)
x= sample(ind,replace = FALSE)
p=seq(345,1380,345)
fold=list()
for(i in 1:4)
{
  fold[[i]]=clean_data[x[(p[i]-344):p[i]],]
}
fold[[5]]=clean_data[x[1381:1728],]
train = list()
test= list()
for(j in 1:5)
{
  f1=setdiff(flag1,j)
  test[[j]] = fold[[j]] 
  train[[j]] = rbind(fold[[f1[1]]],fold[[f1[2]]],fold[[f1[3]]],fold[[f1[4]]])
}

x = c(1,9,19,29,49)
error_matrix = matrix(ncol = 5,nrow = 5)
row = 0
for(k in x){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    train_data = train[[i]]
    train_label = train_data[,7]
    train_data = train_data[,1:6]
    test_data = test[[i]]
    test_label = test_data[,7]
    test_data = test_data[,1:6]
    computed_label = knn(train = train_data,test = test_data,cl = train_label,k = k)
    error_matrix[row,i] = mean(test_label != computed_label) * 100
  }  
}

rownames(error_matrix) = c("K1","K9","K19","K29","K49")
boxplot(x = error_matrix,use.cols = FALSE,xlab = "Various K Values",ylab = "% Error",main="Error rate for different K with KNN R Package")

#_____________________________________________________________________________________________________________________________________________

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

nn = c(1,9,19,29,49)
error_matrix = matrix(ncol = 5,nrow = 5)
row = 0
for(k in nn){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    cat("fold = ",i,"\n")
    train_data = training[[i]]
    train_label = as.matrix(train_data[,16])
    train_data = as.data.frame(train_data[,1:15])
    test_data = testing[[i]]
    test_label = as.matrix(test_data[,16])
    test_data = as.data.frame(test_data[,1:15])
    
    myTestLabel = knn(train = train_data,test = test_data,cl = train_label[,1],k = k)
    error = mean(test_label != myTestLabel) * 100
    error_matrix[row,i] = error
    
  }  
}
rownames(error_matrix) = c("K1","K9","K19","K29","K49")
boxplot(x = error_matrix,use.cols = FALSE,xlab = "Various K Values",ylab = "% Error",main="Error rate for different K with KNN R Package")

#__________________________________________________________________________________________________________________

#Ionosphere Dataset

ionosphere <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                       col_names = FALSE, col_types = cols(X1 = col_skip(), X2 = col_skip()))
indices = seq(1,nrow(ionosphere),1)
temp = sample(x = indices,replace = FALSE)
point = seq(70,350,70)
folds = list()
for(i in 1:4){
  folds[[i]] = ionosphere[temp[(point[i]-69):point[i]],]
}
folds[[5]] = ionosphere[temp[281:351],]

#Create Training And Test Dataset

training = list()
testing = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  testing[[i]] = folds[[i]]
  training[[i]] = rbind(folds[[f[1]]],folds[[f[2]]],folds[[f[3]]],folds[[f[4]]])
}

nn = c(1,9,19,29,49)
error_matrix = matrix(ncol = 5,nrow = 5)
row = 0
for(k in nn){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    train_data = training[[i]]
    train_label = as.matrix(train_data[,33])
    train_data = as.data.frame(train_data[,1:32])
    test_data = testing[[i]]
    test_label = test_data[,33]
    test_data = as.data.frame(test_data[,1:32])
    
    #Normal Voting Euclidean
    myTestLabel = knn(train = train_data,test = test_data,cl = train_label[,1],k = k)
    error = mean(test_label != as.data.frame(myTestLabel)) * 100
    error_matrix[row,i] = error
    
  }  
}
rownames(error_matrix) = c("K1","K9","K19","K29","K49")
boxplot(x = error_matrix,use.cols = FALSE,xlab = "Various K Values",ylab = "% Error",main="Error rate for different K with KNN R Package")

