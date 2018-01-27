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

#__________________________________________________________________________________________________________________________________

RunNaiveBayes = function(train_data,train_label,test_data){
  cl = length(names(table(train_label)))
  prior = rep(0,cl)
  prior[1] = as.numeric(table(train_label)[1]/length(train_label))
  prior[2] = as.numeric(table(train_label)[2]/length(train_label))
  
  lab = c("g","b")
  computed_label = rep("0",nrow(test_data))
  for(i in 1:nrow(test_data)){
    probability = matrix(ncol = 2,nrow = ncol(test_data))
    for(j in 1:ncol(test_data)){
      temp = as.data.frame(cbind(train_data[,j],train_label))
      temp_table = table(temp)
      
      if(row.names(temp_table)[1] == "low"){
        text = c("low","middle","high")
        index = which(row.names(temp_table) == text[as.integer((test_data[i,j]))])  
      }else{
        index = which(row.names(temp_table) == as.character(test_data[i,j]))  
      }
      
      nc = temp_table[index,]
      n = colSums(temp_table)
      #m = 1 / prior[1]
      m = 2
      probability[j,1] = (nc[1] + (m * 0.5)) / (n[1] + m)
      probability[j,2] = (nc[2] + (m * 0.5)) / (n[2] + m)
      #print(probability)
    }
    product = apply(probability,MARGIN = 2,FUN = prod)
    finalProb = product * prior
    #print(finalProb)
    ind = which.max(finalProb)
    #print(ind)
    computed_label[i] = lab[ind]
  }
  return(computed_label)
}

#__________________________________________________________________________________________________________

error_rate = rep(0,5)
for(i in 1:5){
  cat("Dataset - ",i)
  train_data = training[[i]]
  train_label = train_data[,33]
  train_data = train_data[,1:32]
  test_data = testing[[i]]
  test_label = test_data[,33]
  test_data = test_data[,1:32]
  
  compLabel = RunNaiveBayes(train_data,train_label,test_data)
  error_rate[i] = mean(compLabel == test_label) * 100
}

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Error rate in Ionosphere Dataset")


