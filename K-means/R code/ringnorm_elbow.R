library(readr)
ringnorm <- read_csv("C:/Users/ASHAY/Desktop/Fall 2017/Assignments/AML/Assignment 1/ringnorm_data.csv", col_names = FALSE)
mydata = as.data.frame(unclass(ringnorm))
summary(mydata)
dim(mydata)
myDataClean = na.omit(mydata)
clean_ringnorm_data <- subset(mydata, select = -c(1))
scaled_data = as.matrix((clean_ringnorm_data))
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
