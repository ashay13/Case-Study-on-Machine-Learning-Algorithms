library(readr)
ionosphere <- read_csv("C:/Users/ASHAY/Desktop/Fall 2017/Assignments/AML/Assignment 1/ionosphere.csv", col_names = FALSE)
mydata = as.data.frame(unclass(ionosphere))
myDataClean = na.omit(mydata)
clean_ionosphere_data <- subset(mydata, select = -c(35))
scaled_data = as.matrix((clean_ionosphere_data))
#scaled_data
kmm = kmeans(scaled_data,2,iter.max = 30,algorithm = "Lloyd")
write.xlsx(kmm$centers,"Packaged_Lloyd_Ionosphere_Centroids.xlsx")