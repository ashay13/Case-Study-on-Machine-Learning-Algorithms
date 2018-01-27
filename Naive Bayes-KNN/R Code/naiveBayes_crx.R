#Credit Approval Dataset
#Divide Data into 5 folds
library(readr)
crx <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
                col_names = FALSE, col_types = cols(X11 = col_number(), 
                                                    X14 = col_number()))
crx[crx == '?'] = NA

clean_dataset = na.omit(crx)
clean_dataset$X2 = as.double(clean_dataset$X2)
clean_dataset = clean_dataset[-which(clean_dataset$X7 == 'o'), ]
clean_dataset = clean_dataset[-which(clean_dataset$X13 == 'p'), ]
clean_dataset = clean_dataset[-which(clean_dataset$X4 == 'l'), ]

b2=quantile(clean_dataset$X2,c(0,1/3,2/3,1))
b2[1]=b2[1]-.00005
clean_dataset$X2 = cut(clean_dataset$X2, breaks=b2, labels=c("low","middle","high"))

b2=quantile(clean_dataset$X3,c(0,1/3,2/3,1))
b2[1]=b2[1]-.00005
clean_dataset$X3 = cut(clean_dataset$X3, breaks=b2, labels=c("low","middle","high"))

b2=quantile(clean_dataset$X8,c(0,1/3,2/3,1))
b2[1]=b2[1]-.00005
clean_dataset$X8 = cut(clean_dataset$X8, breaks=b2, labels=c("low","middle","high"))

b2=quantile(clean_dataset$X11,c(0,1/3,2/3,1))
b2[1]=b2[1]-.00005
clean_dataset$X11 = cut(clean_dataset$X11, breaks=b2, labels=c("low","middle","high"))

b2=quantile(clean_dataset$X14,c(0,1/3,2/3,1))
b2[1]=b2[1]-.00005
clean_dataset$X14 = cut(clean_dataset$X14, breaks=b2, labels=c("low","middle","high"))

b2=quantile(clean_dataset$X15,c(0,1/3,2/3,1))
b2[1]=b2[1]-.00005
clean_dataset$X15 = cut(clean_dataset$X15, breaks=b2, labels=c("low","middle","high"))


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

#------------------------------------------------------------------------------------------------------------

RunNaiveBayes = function(train_data,train_label,test_data){
  cl = length(names(table(train_label)))
  prior = rep(0,cl)
  prior[1] = as.numeric(table(train_label)[1]/nrow(train_label))
  prior[2] = as.numeric(table(train_label)[2]/nrow(train_label))
  
  lab = c("-","+")
  computed_label = rep("0",nrow(test_data))
  for(i in 1:nrow(test_data)){
    probability = matrix(ncol = 2,nrow = ncol(test_data))
    for(j in 1:ncol(test_data)){
      temp = cbind(train_data[,j],train_label)
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

#------------------------------------------------------------------------------------------------

error_rate = rep(0,5)
for(i in 1:5){
  cat("Dataset - ",i)
  train_data = training[[i]]
  train_label = train_data[,16]
  train_data = train_data[,1:15]
  test_data = testing[[i]]
  test_label = test_data[,16]
  test_data = test_data[,1:15]
  
  compLabel = RunNaiveBayes(train_data,train_label,test_data)
  error_rate[i] = mean(compLabel != test_label) * 100
}

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Error rate in Credit Approval Dataset")
