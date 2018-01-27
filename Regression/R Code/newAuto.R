#install.packages('ISLR')
require('ISLR')
require('matlib')
dataset = Auto
mpg01 = rep(0,nrow(dataset))

for(i in 1:nrow(dataset)){
  if(dataset$mpg[i]>median(dataset$mpg)){
    mpg01[i] = 1
  }else{
    mpg01[i] = 0
  }
}


new.Auto = cbind(mpg01,dataset$cylinders,dataset$displacement,dataset$horsepower,dataset$weight)
colmean = colMeans(new.Auto[,2:5])
colsd = sqrt(diag(var(new.Auto[,2:5])))
new.Auto = cbind(mpg01,scale(new.Auto[,2:5]))
