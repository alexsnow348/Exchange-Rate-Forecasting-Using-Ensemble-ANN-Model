library(neural)
source("requried_functions_N_libraries.R")
source("Data_Processing.R")
dataset<- Data_Processing("/home/wut/Desktop/Link to Data/FYP Program/Raw Data/alldata.csv",3,"Euro")
train_dataset <- dataset[[1]]
test_dataset <- dataset[[2]]
date <- dataset[[3]]
usd_non_normalize <- dataset[[4]]

train_input <- as.matrix(train_dataset[,1:3])
train_output <- as.matrix(train_dataset[,4])
test_input <- as.matrix(test_dataset[,1:3])
test_actual <- as.vector(test_dataset[,4])

neurons <- 2;
## Not run: 
data<-rbftrain(train_input,neurons,train_output, alfa= 0.1, it= 1000, sigma=NaN,visual = F)
result <- rbf(test_input,data$weight,data$dist,data$neurons,data$sigma)
head(result)

result <- as.vector(result)
error <- test_actual - result
rmse(error)
mae(error)
hist(error)
x<-t(matrix(-5:10*24,1,16));
y<-t(matrix(sin(pi/180*(-5:10*24)),1,16));
neurons<-8;
## Not run: 
data<-rbftrain(x,neurons,y,sigma=NaN)
result <- rbf(x,data$weight,data$dist,data$neurons,data$sigma)
head(result)
