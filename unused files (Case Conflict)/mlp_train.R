
## libraries 
library("neuralnet")

source("requried_functions_N_libraries.R")
source("Data_Processing.R")
dataset<- Data_Processing("/home/wut/Desktop/Link to Data/FYP Program/Raw Data/alldata.csv",3,"Euro")
train_dataset <- dataset[[1]]
test_dataset <- dataset[[2]]
date <- dataset[[3]]
usd_non_normalize <- dataset[[4]]

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