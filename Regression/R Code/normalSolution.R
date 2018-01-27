require('ISLR')
#Q1.5 
data_x = cbind(1,Auto$horsepower)
data_y = Auto$mpg
pre_theta = pseudoinverse(t(data_x) %*% data_x) %*% t(data_x) %*% data_y
abline(pre_theta[1],pre_theta[2],col="blue")
cat("Coefficients with Normal Equations ",pre_theta,"\n")
giv = c(1,220)
h0x_th = (pre_theta[1] * giv[1]) + (pre_theta[2] * giv[2])
cat("Predicted value for mpg when horsepower is 220 = ",h0x_th)

#Q1.9

dataset = Auto
x=as.matrix(cbind(dataset$cylinders,dataset$displacement,dataset$horsepower,dataset$weight,dataset$acceleration,dataset$year,dataset$origin))
colmean_x = colMeans(x)
colsd_x = sqrt(diag(var(x)))  

y=dataset$mpg
x = scale(x)
x = cbind(1,x)

given = c(4,300,200,3500,11,70,2)
given = given - colmean_x
given = given/colsd_x
given = c(1,given)


cf_theta = inv(t(x) %*% x) %*% (t(x) %*% y)
cat("Coefficients using normal equations = ",cf_theta,"\n")
predict_normal=0
for(j in 1:ncol(x)){
  predict_normal = (cf_theta[j] * given[j]) + predict_normal
}
print(predict_normal)