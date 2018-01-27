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
}

#_____________________________________________________________________________________________________

RunNaiveBayes = function(train_data,train_label,test_data){
  prior = rep(0,4)
  prior[1] = as.numeric(table(train_label)[1]/length(train_label))
  prior[2] = as.numeric(table(train_label)[2]/length(train_label))
  prior[3] = as.numeric(table(train_label)[3]/length(train_label))
  prior[4] = as.numeric(table(train_label)[4]/length(train_label))
  
  lab = c("acc","good","unacc","vgood")
  computed_label = rep(NA,nrow(test_data))
  for(i in 1:nrow(test_data)){
    probability = matrix(ncol = 4,nrow = ncol(test_data))
    for(j in 1:ncol(test_data)){
      temp = cbind(train_data[,j],train_label)
      temp_table = table(temp)
      index = which(row.names(temp_table) == as.character(test_data[i,j]))
      nc = temp_table[index,]
      n = colSums(temp_table)
      #m = 1 / prior[1]
      m = 4
      probability[j,1] = (nc[1] + (m * 0.25)) / (n[1] + m)
      probability[j,2] = (nc[2] + (m * 0.25)) / (n[2] + m)
      probability[j,3] = (nc[3] + (m * 0.25)) / (n[3] + m)
      probability[j,4] = (nc[4] + (m * 0.25)) / (n[4] + m)
      #print(probability)
    }
    product = apply(probability,MARGIN = 2,FUN = prod)
    finalProb = product * prior
    ind = which.max(finalProb)
    computed_label[i] = lab[ind]
  }
  return(computed_label)
}

#_____________________________________________________________________________________________________________
error_rate = rep(0,5)
for(i in 1:5){
  trainData = training[[i]]
  testData = testing[[i]]
  train_label = trainData[,7]
  test_label = testData[,7]
  train_data = trainData[,1:6]
  test_data = testData[,1:6]
  computedLabel = RunNaiveBayes(train_data,train_label,test_data)
  error_rate[i] = mean(computedLabel != test_label) * 100
}

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Error rate in Car Evaluation Dataset")
