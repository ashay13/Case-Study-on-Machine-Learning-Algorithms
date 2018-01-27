require('ISLR')
dataset = Auto
x=as.matrix(cbind(dataset$cylinders,dataset$displacement,dataset$horsepower,dataset$weight,dataset$acceleration,dataset$year,dataset$origin))
y=dataset$mpg
y = scale(y)
x = scale(x)
data = data.frame(cbind(x,y))
lr = lm(X8~X1+X2+X3+X4+X5+X6+X7,data)
coe = lr$coefficients
sum(coe * given)
