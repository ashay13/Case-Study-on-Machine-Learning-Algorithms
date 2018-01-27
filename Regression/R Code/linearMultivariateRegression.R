#install.packages('ISLR')
require('ISLR')
require('matlib')
dataset = Auto
x=as.matrix(cbind(dataset$cylinders,dataset$displacement,dataset$horsepower,dataset$weight,dataset$acceleration,dataset$year,dataset$origin))
colmean_x = colMeans(x)
colsd_x = sqrt(diag(var(x)))  

y=dataset$mpg
#y = scale(y)
x = scale(x)
x = cbind(1,x)

dimension = ncol(x)
row = nrow(x)

costFunction = function(x,y,theta){
  
  j_theta = rep(0,ncol(x))
  cost = 0
  for(i in 1:nrow(x)){
    h0x=0
    for(j in 1:ncol(x)){
      h0x = (theta[j] * x[i,j]) + h0x
    }
    
    for(j in 1:ncol(x)){
      j_theta[j] = j_theta[j] + ((h0x - y[i]) * x[i,j])
    }
    cost = (h0x - y[i])^2 + cost
  }
  
  cost = cost/(2*nrow(x))
  j_theta = j_theta/nrow(x)
  
  returnList = list("jtheta" = j_theta,"cost" = cost)
  return(returnList)
}

PerformMultivariateLinear = function(alpha,iter){
  
  theta = rep(0,ncol(x))
  new_theta = rep(0,ncol(x))
  cost_vect = rep(0,iter)
  for(i in 1:iter){
    theta = new_theta
    returnList = costFunction(x,y,theta)
    returnJ = returnList$jtheta
    cost = returnList$cost
    cost_vect[i] = cost
    for(j in 1:ncol(x)){
      new_theta[j] = theta[j] - (alpha * returnJ[j])  
    }
  }
  
  plot(cost_vect)
  
  #1.6  
  print(new_theta)
  return(new_theta)
}

out_theta = PerformMultivariateLinear(0.003,1400)

#1.7
#This part calculates mpg from theta predicted by my program

given = c(4,300,200,3500,11,70,2)
given = given - colmean_x
given = given/colsd_x
given = c(1,given)
predict=0
for(j in 1:ncol(x)){
  predict = (out_theta[j] * given[j]) + predict
}
print(predict)



#Q1.8
out_theta3 = PerformMultivariateLinear(3,100)
predict3=0
for(j in 1:ncol(x)){
  predict3 = (out_theta3[j] * given[j]) + predict3
}
print(predict3)

out_theta0.3 = PerformMultivariateLinear(0.3,100)
predict0.3=0
for(j in 1:ncol(x)){
  predict0.3 = (out_theta0.3[j] * given[j]) + predict0.3
}
print(predict0.3)

out_theta0.03 = PerformMultivariateLinear(0.03,100)
predict0.03=0
for(j in 1:ncol(x)){
  predict0.03 = (out_theta0.03[j] * given[j]) + predict0.03
}
print(predict0.03)

out_theta0.00003 = PerformMultivariateLinear(0.00003,100)
predict0.00003=0
for(j in 1:ncol(x)){
  predict0.00003 = (out_theta0.00003[j] * given[j]) + predict0.00003
}
print(predict0.00003)


#Q1.9
cf_theta = inv(t(x) %*% x) %*% (t(x) %*% y)
cat("Coefficients using normal equations = ",cf_theta,"\n")
predict_normal=0
for(j in 1:ncol(x)){
  predict_normal = (cf_theta[j] * given[j]) + predict_normal
}
print(predict_normal)

