


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
test_actual <- as.vector(test_dataset[,4])
result <- denormalized(result)
test_actual <- denormalized(test_actual)
error <- test_actual - result
rmse(error)
mae(error)
hist(error)


