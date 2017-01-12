library("rnn")
library("RSNNS")
source("requried_functions_N_libraries.R")
source("Data_Processing.R")
dataset<- Data_Processing("/home/wut/Desktop/Link to Data/FYP Program/Raw Data/alldata.csv",3,"Euro")
train_dataset <- dataset[[1]]
test_dataset <- dataset[[2]]
date <- dataset[[3]]
usd_non_normalize <- dataset[[4]]

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

model <- trainr(Y=output_matrix,
                X=input_try,
                learningrate   =  0.1,
                sigmoid = "logistic",
                hidden_dim  = 2,
                numepochs = 200
                
)

## Testing

test_input_day1 <- as.matrix(test_dataset[,1])
test_input_day2 <- as.matrix(test_dataset[,2])
test_input_day3 <- as.matrix(test_dataset[,3])

test_input_try <- array( c(test_input_day1,test_input_day2,test_input_day3) , dim=c(dim(test_input_day3),3) )

test_result <- predictr(model, test_input_try )
predict_value <- (denormalized(test_result))
actual <- denormalized(test_dataset[,4])
error <- actual - predict_value
hist( actual-predict_value )
rmse(error)
mae(error)
