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

x = new.Auto[,2:5]
x = cbind(1,x)
y = new.Auto[,1]
dimension = ncol(new.Auto)
row = nrow(new.Auto)

calculateError = function(x,theta){
  prediction = rep(-1,nrow(x))
  #for(i in 1:nrow(x)){
    mpg = sigmoidFunction(x %*% theta)
    for(i in 1:length(mpg)){
      if(mpg[i] < 0.5){
        prediction[i] = 0
      }else if(mpg[i] > 0.5){
        prediction[i] = 1
      }  
    }
    #print(mpg)
    
  
  #print(prediction)
  result = (prediction - y)^2
  #print(result)
  return((sum(result)/nrow(x))*100)
}


sigmoidFunction = function(z){
  g = 1/(1+exp(-z))
  return(g)
}

costFunction = function(x,y,theta){
  m=nrow(x)
  
  #Calculate cost at this iteration
  g = sigmoidFunction(x %*% theta)
  cost = (1/m)*sum((-y*log(g)) - ((1-y)*log(1-g)))
  
  #Calculate J_theta
  temp = g-y
  new_temp = matrix(nrow = m,ncol = 5)
  for(i in 1:m){
  new_temp[i,] = x[i,] * temp[i]  
  }
  
  new_temp = colSums(new_temp)/m 
  
  returnList = list("cost" = cost,"jtheta" = new_temp)
  return(returnList)
}

PerformMultivariateLinear = function(alpha,iter){
  
  theta = rep(0,ncol(x))
  new_theta = rep(0,ncol(x))
  cost_vect = rep(0,iter)
  for(i in 1:iter){
    theta = new_theta
    returnList = costFunction(x,y,theta)
    cost = returnList$cost
    j_theta  = returnList$jtheta
    cost_vect[i] = cost
    for(j in 1:ncol(x)){
      new_theta[j] = theta[j] - (alpha * j_theta[j])  
    }
  }
  plot(cost_vect)
  print(new_theta)
  return(new_theta)
}

#out_theta = PerformMultivariateLinear(0.001,15000)

#3.2
out_theta = PerformMultivariateLinear(0.01,1000)
print(calculateError(x,out_theta))

print(out_theta)

#3.4
given = c(8,340,200,3500)
given = (given - colmean)/colsd
given = c(1,given)
predict= sigmoidFunction(given %*% out_theta)
print(predict)


#3.1
input = c(1,2,3,4,5,6,7,8,9,10)
output = rep(0,10)
for(i in 1:10){
 output[i] = sigmoidFunction(input[i]) 
}

plot(input,output)

#3.5
out_theta3 = PerformMultivariateLinear(3,100)
predict= sigmoidFunction(given %*% out_theta3)
print(predict)

out_theta0.3 = PerformMultivariateLinear(0.3,100)
predict= sigmoidFunction(given %*% out_theta0.3)
print(predict)

out_theta0.03 = PerformMultivariateLinear(0.03,100)
predict= sigmoidFunction(given %*% out_theta0.03)
print(predict)

out_theta0.00003 = PerformMultivariateLinear(0.00003,100)
predict= sigmoidFunction(given %*% out_theta0.00003)
print(predict)