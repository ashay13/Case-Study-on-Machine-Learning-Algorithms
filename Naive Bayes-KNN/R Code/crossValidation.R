library(readr)

#Ionosphere Dataset
#Divide Data into 5 folds
k = 5
ionosphere <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                       col_names = FALSE, col_types = cols(X2 = col_skip()))
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
  write_csv(x = testing[[i]],path =  paste("ionosphere_test_",i,".csv",sep = ""),append = FALSE,col_names = TRUE)
  write_csv(x = training[[i]],path =  paste("ionosphere_train_",i,".csv",sep = ""),append = FALSE,col_names = TRUE)
}

#__________________________________________________________________________________________________________________________________


#Car Evaluation Dataset
#Divide Data into 5 folds
k = 5
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
  write_csv(x = testing[[i]],path =  paste("car_test_",i,".csv",sep = ""),append = FALSE,col_names = TRUE)
  write_csv(x = training[[i]],path =  paste("car_train_",i,".csv",sep = ""),append = FALSE,col_names = TRUE)
}

#__________________________________________________________________________________________________________________


#Credit Approval Dataset
#Divide Data into 5 folds
k = 5
crx <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
                col_names = FALSE)
indices = seq(1,nrow(crx),1)
temp = sample(x = indices,replace = FALSE)
point = seq(138,690,138)
folds = list()
for(i in 1:5){
  folds[[i]] = crx[temp[(point[i]-137):point[i]],]
}

#Create Training And Test Dataset

training = list()
testing = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  testing[[i]] = folds[[i]]
  training[[i]] = rbind(folds[[f[1]]],folds[[f[2]]],folds[[f[3]]],folds[[f[4]]])
  write_csv(x = testing[[i]],path =  paste("crx_test_",i,".csv",sep = ""),append = FALSE,col_names = TRUE)
  write_csv(x = training[[i]],path =  paste("crx_train_",i,".csv",sep = ""),append = FALSE,col_names = TRUE)
}
