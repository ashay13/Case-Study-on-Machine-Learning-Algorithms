library(readr)
ionosphere <- read_csv("C:/Users/ASHAY/Desktop/Fall 2017/Assignments/AML/Assignment 1/ionosphere.csv", col_names = FALSE)
#View(ionosphere)
mydata = as.data.frame(unclass(ionosphere))
summary(mydata)
dim(mydata)
myDataClean = na.omit(mydata)
clean_ionosphere_data <- subset(mydata, select = -c(35))
scaled_data = as.matrix((clean_ionosphere_data))
#scaled_data
#kmm = kmeans(scaled_data,3,iter.max = 15,nstart = 50)
set.seed(123)
k.max = 10
data = scaled_data
wss = sapply(2:k.max,function(k){
  kmeans(data,k,iter.max = 30)$tot.withinss
})
wss

plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
