#Validation over Ionosphere Dataset
#install.packages("naivebayes")
library("naivebayes")

library(readr)
#Ionosphere Dataset
#Divide Data into 5 folds

ionosphere <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                       col_names = FALSE, col_types = cols(X1 = col_skip(), X2 = col_skip()))

convertToCategorical = function(dataColumn){
  b=quantile(dataColumn,c(0,1/3,2/3,1))
  b[1]=b[1]-.00005
  newColumn = cut(dataColumn, breaks=b, labels=c("low","middle","high"))
  return(newColumn)
}

newData = apply(X = ionosphere[,1:32],MARGIN = 2,FUN = function(x)convertToCategorical(x))
newData = cbind(newData,ionosphere$X35)

indices = seq(1,nrow(newData),1)
temp = sample(x = indices,replace = FALSE)
point = seq(70,350,70)
folds = list()
for(i in 1:4){
  folds[[i]] = newData[temp[(point[i]-69):point[i]],]
}
folds[[5]] = newData[temp[281:351],]

#Create Training And Test Dataset

training = list()
testing = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  testing[[i]] = folds[[i]]
  training[[i]] = rbind(folds[[f[1]]],folds[[f[2]]],folds[[f[3]]],folds[[f[4]]])
}


library(e1071)

error_rate = rep(0,5)
for(i in 1:5){
  cat("Dataset - ",i)
  train_data = as.data.frame(training[[i]])
  test_data = as.data.frame(testing[[i]])
  test_label = test_data$V33
  test_data = test_data[,1:32]
  model <- naiveBayes(V33 ~ ., data = train_data)
  preds <- predict(model, newdata = test_data)
  
  error_rate[i] = mean(preds != test_label) * 100
}

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Error rate in R package of Naive Bayes Classifier",sub = "Ionosphere Dataset")

#______________________________________________________________________________________________________________________________________

crx <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
                col_names = FALSE, col_types = cols(X11 = col_number(), 
                                                    X14 = col_number()))
crx[crx == '?'] = NA
clean_dataset = na.omit(crx)
clean_dataset$X2 = as.double(clean_dataset$X2)
clean_dataset = clean_dataset[-which(clean_dataset$X7 == 'o'), ]
clean_dataset = clean_dataset[-which(clean_dataset$X13 == 'p'), ]
clean_dataset = clean_dataset[-which(clean_dataset$X4 == 'l'), ]

indices = seq(1,nrow(clean_dataset),1)
temp = sample(x = indices,replace = FALSE)
point = seq(130,520,130)
folds = list()
for(i in 1:4){
  folds[[i]] = clean_dataset[temp[(point[i]-129):point[i]],]
}
folds[[5]] = clean_dataset[temp[521:649],]
#Create Training And Test Dataset

training = list()
testing = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  testing[[i]] = folds[[i]]
  training[[i]] = rbind(folds[[f[1]]],folds[[f[2]]],folds[[f[3]]],folds[[f[4]]])
}

error_rate = rep(0,5)
for(i in 1:5){
  cat("Dataset - ",i)
  train_data = training[[i]]
  train_label = train_data[,16]
  test_data = testing[[i]]
  test_label = test_data[,16]
  test_data = test_data[,1:15]
  
  model <- naive_bayes(formula = X16 ~ .,data = train_data)
  pred = predict(object = model,newdata = test_data)
  
  error_rate[i] = mean(as.character(pred) != test_label) * 100
}

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Error rate in R package of Naive Bayes Classifier",sub = "Credit Approval Dataset")


#--------------------------------------------------------------------------------------------------------------------------------------

car <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                col_names = FALSE)
indices = seq(1,nrow(car),1)
temp = sample(x = indices,replace = FALSE)
point = seq(345,1380,345)
folds = list()
for(i in 1:4){
  folds[[i]] = car[temp[(point[i]-344):point[i]],]
}
folds[[5]] = car[temp[1381:1728],]

#Create Training And Test Dataset

training = list()
testing = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  testing[[i]] = folds[[i]]
  training[[i]] = rbind(folds[[f[1]]],folds[[f[2]]],folds[[f[3]]],folds[[f[4]]])
}


error_rate = rep(0,5)
for(i in 1:5){
  cat("Dataset - ",i)
  train_data = training[[i]]
  train_label = train_data[,7]
  test_data = testing[[i]]
  test_label = test_data[,7]
  test_data = test_data[,1:6]
  
  model <- naive_bayes(formula = X7 ~ .,data = train_data)
  pred = predict(object = model,newdata = test_data)
  
  error_rate[i] = mean(as.character(pred) != test_label) * 100
}

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Error rate in R package of Naive Bayes Classifier",sub = "Car Evaluation Dataset")
