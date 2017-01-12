
#Main Modeling & Training and Validation

## Libraries

library("neural")
library("rnn")
library("RSNNS")

source("requried_functions_N_libraries.R")
source("Data_Processing.R")
dataset<- Data_Processing("/home/wut/Desktop/Link to Data/FYP Program/Raw Data/alldata.csv",3,"Euro")
train_dataset <- dataset[[1]]
test_dataset <- dataset[[2]]
date <- dataset[[3]]
usd_non_normalize <- dataset[[4]]

## HETROGENEOUS  MODEL
## Train the network using neuralnet (First network:  MLP)

# Training
exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay, learningrate = 0.1,
                            data = train_dataset, hidden = 2)

# Testing and Error Result
model_results <- neuralnet::compute(exchange_model, test_dataset[1:3])
predicted_oneDayhead <- model_results$net.result
predict_value <- denormalized(predicted_oneDayhead)
actual <- denormalized(test_dataset[,4])
error <- actual - predict_value
rmse(error)
mae(error)
hist(error)

## Traini the network using RNN (Second network: RNN)

## Training
input_day1 <- as.matrix(train_dataset[,1])
input_day2 <- as.matrix(train_dataset[,2])
input_day3 <- as.matrix(train_dataset[,3])
output_matrix <- as.matrix(train_dataset[,4])

input_try <- array( c(input_day1,input_day2,input_day3) , dim=c(dim(input_day1),3) )
output_try <- array(output_matrix, dim=c(dim(output_matrix),1))
inital_weight <- sample(1:length(input_day1),length(input_day1), replace = F)
set.seed(1)
inital_weight <- normalizeData(inital_weight, type = "0_1")

exchange_model2 <- trainr(Y=output_matrix,
                X=input_try,
                learningrate   =  0.1,
                sigmoid = "logistic",
                hidden_dim  = 2,
                numepochs = 200
                
)

# Testing and Error Result

test_input_day1 <- as.matrix(test_dataset[,1])
test_input_day2 <- as.matrix(test_dataset[,2])
test_input_day3 <- as.matrix(test_dataset[,3])

test_input_try <- array( c(test_input_day1,test_input_day2,test_input_day3), dim=c(dim(test_input_day3),3) )

test_result <- predictr(exchange_model2, test_input_try )
predict_value2 <- denormalized(test_result)
error2 <- actual - predict_value2
hist( actual-predict_value2 )
rmse(error2)
mae(error2)

## Traini the network using RNN (Third network: RBF)

## Training
train_input <- as.matrix(train_dataset[,1:3])
train_output <- as.matrix(train_dataset[,4])
test_input <- as.matrix(test_dataset[,1:3])


data<-rbftrain(train_input,neurons=2,train_output, alfa= 0.1, it= 1000, sigma=NaN,visual = F)


## Testing and Error Result

result <- rbf(test_input,data$weight,data$dist,data$neurons,data$sigma)
result <- as.vector(result)
predict_value3 <- denormalized(result)
error3 <- actual - predict_value3
rmse(error3)
mae(error3)
hist(error3)


## Hetrogeneous  predicted_data and errors
all_predicted <- c(predict_value,predict_value2,predict_value3)
all_predicted <-as.data.frame(all_predicted)
names(all_predicted)<- c("First MLP", "Second RNN", "Third RBF")


## Final Fusion Funtion
min_value <-apply(all_predicted,1, min)
max_value <- apply(all_predicted,1,max)
mean_value <- apply(all_predicted,1,mean)

error_min <- actual - min_value
error_max <- actual - max_value
error_mean <- actual - mean_value

error_all_after_fusion <- as.data.frame(cbind(error_min,error_max,error_mean))

# Example of invocation of functions
rmse(error) 
mae(error) 

# Example of invocation of functions
rmse(error) 
mae(error) 

## Final reasult
data_result <- cbind(test_date[702:1168,5],actual,predict_value,error)
names(data_result) <- c("Date","Actual_Value","Predicted_Value", "Error")
data_error_hidden2 <- cbind(error,error2,error3)
names(data_error_hidden2) <- c("MLP_One_Error","MLP_Two_Error","MLP_Three_Error")

# Writing to xlsx file
library(xlsx)
write.xlsx(data_result, "data_result.xlsx")
write.xlsx(error_all_after_fusion, "error_all_after_fusion.xlsx")
hist(error)
