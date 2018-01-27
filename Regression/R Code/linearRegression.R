#install.packages('ISLR')
require('ISLR')
#install.packages('matlib')
require('matlib')

#install.packages('corpcor')
require('corpcor')

dataset_raw = cbind(Auto$horsepower,Auto$mpg)
colmean = mean(dataset_raw[,1])
colsd = sqrt(var(dataset_raw[,1]))
dataset = cbind(scale(dataset_raw[,1]),dataset_raw[,2])
dataset = cbind(1,dataset)
#colnames(dataset) = c('x0','horsepower','mpg')

x = as.matrix(dataset[,c(1,2)]) 
y = as.matrix(dataset[,3])
m = nrow(dataset)

costFunction = function(x,y,theta){
  j0 = 0
  j1 = 0
  cost = 0
  for(i in 1:nrow(x)){
    h0x = (theta[1] * x[i,1]) + (theta[2] * x[i,2])
    j0 = j0 + ((h0x - y[i]))
    j1 = j1 + ((h0x - y[i]) * x[i,2])
    cost = (h0x-y[i])^2 + cost
  }
  
  cost = cost/(2*nrow(x))
  j0 = j0/nrow(x)
  j1 = j1/nrow(x)
  returnList = list("j0"=j0,"j1"=j1,"cost"=cost)
  return(returnList)
}

PerformLinear = function(alpha,iter){
  giv = (220 - colmean)/colsd
  giv = c(1,giv)
  theta = c(0,0)
  new_theta = c(0,0)
  J.theta = matrix(nrow = iter,ncol = 3)
  for(i in 1:iter){
    theta = new_theta
    returnJ = costFunction(x,y,theta)
    j0 = returnJ$j0
    j1 = returnJ$j1
    cost = returnJ$cost
      J.theta[i,1] = theta[1]
      J.theta[i,2] = theta[2]
      J.theta[i,3] = cost
    
    new_theta[1] = theta[1] - (alpha * j0)
    new_theta[2] = theta[2] - (alpha * j1)
  }

  
  #Q1.1
  cat("Theta0 = ",new_theta[1]," ,Theta1 = ",new_theta[2],"\n")
  #Q1.3
  h0x_prg = (new_theta[1] * giv[1]) + (new_theta[2] * giv[2])
  
  #Q1.2
  cat("Prediction for 220 Hoursepower = ",h0x_prg,"\n")
  plot(dataset[,2],dataset[,3],pch = 16, cex = 1.3, col = "yellow",xlab = "Horsepower",ylab = "mpg")
  abline(new_theta[1],new_theta[2],col="red")
  
  return(J.theta)
}

#plot.data = PerformLinear(0.00001,2500000)
plot.data = PerformLinear(0.1,100)
plot(plot.data[,3])

#Contour Plot
xx=as.data.frame(plot.data) 
plot_ly(data = xx,x=xx[,1],y=xx[,2],z=xx[,3],type = "contour",contours = list(showlabels = TRUE)) %>%
  colorbar(title = "Cost J(theta)'s")

