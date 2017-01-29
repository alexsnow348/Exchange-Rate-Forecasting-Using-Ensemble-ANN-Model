
### Filter the best_usd result_usd

library("dplyr")
# HOMO model

result_usd <- as.tbl(result_HOMO_USD)
min_value <- min(result_usd$RMSE)
best_usd <- filter(result_usd, RMSE == min(result_usd$RMSE))
for (i in 1:length(result_usd$RMSE)) {
        if(result_usd$RMSE[i]== min_value){
                row_select <- i
        }
               
}

predictor_order <- best_usd$Predictor_Order
neurons <- best_usd$Neurons
activation_func <- best_usd$Activation_Function
learning_rate <- best_usd$Learning_Rate

## Getting Error and actual value from the result_usd LIST to plot
predict_value<- Result_USD_HOMO_LIST[[row_select]][[5]][[1]]       ##[[the row number in result_HOMO_*]][[5]][[1]]
predict_error <- Result_USD_HOMO_LIST[[row_select]][[5]][[2]]      ##[[the row number in result_HOMO_*]][[5]][[2]]


homo_model1_usd <- Result_USD_HOMO_LIST[[row_select]][[5]][[6]][[1]]   ##[[the row number in result_HOMO_*][[5]][[6]][[1]]
homo_model2_usd <- Result_USD_HOMO_LIST[[row_select]][[5]][[6]][[2]]   ##[[the row number in result_HOMO_*][[5]][[6]][[2]]
homo_model3_usd <- Result_USD_HOMO_LIST[[row_select]][[5]][[6]][[3]]   ##[[the row number in result_HOMO_*][[5]][[6]][[3]]

#******* Training 70% ******
predict_value<- Result_USD_HOMO_LIST_70[[row_select]][[5]][[1]]       ##[[the row number in result_HOMO_*]][[5]][[1]]
predict_error <- Result_USD_HOMO_LIST_70[[row_select]][[5]][[2]]      ##[[the row number in result_HOMO_*]][[5]][[2]]


homo_model1_usd <- Result_USD_HOMO_LIST_70[[row_select]][[5]][[6]][[1]]   ##[[the row number in result_HOMO_*][[5]][[6]][[1]]
homo_model2_usd <- Result_USD_HOMO_LIST_70[[row_select]][[5]][[6]][[2]]   ##[[the row number in result_HOMO_*][[5]][[6]][[2]]
homo_model3_usd <- Result_USD_HOMO_LIST_70[[row_select]][[5]][[6]][[3]]   ##[[the row number in result_HOMO_*][[5]][[6]][[3]]

#******* Training 80% ******
predict_value<- Result_USD_HOMO_LIST_80[[row_select]][[5]][[1]]       ##[[the row number in result_HOMO_*]][[5]][[1]]
predict_error <- Result_USD_HOMO_LIST_80[[row_select]][[5]][[2]]      ##[[the row number in result_HOMO_*]][[5]][[2]]


homo_model1_usd <- Result_USD_HOMO_LIST_80[[row_select]][[5]][[6]][[1]]   ##[[the row number in result_HOMO_*][[5]][[6]][[1]]
homo_model2_usd <- Result_USD_HOMO_LIST_80[[row_select]][[5]][[6]][[2]]   ##[[the row number in result_HOMO_*][[5]][[6]][[2]]
homo_model3_usd <- Result_USD_HOMO_LIST_80[[row_select]][[5]][[6]][[3]]   ##[[the row number in result_HOMO_*][[5]][[6]][[3]]

test_data <- data_set[[1]][[predictor_order-2]][[3]]                  ## [[currency]][[predictor_order-2]][[1:6]] 1.Traindata,
validate_data <- data_set[[1]][[predictor_order-2]][[2]]              # 2.Validate Data 3. TestData  4.validate_date 5. test_Date
								      # 6. actual whole dataset


#Applying three models to valadatation dataset
non_normalize <- data_set[[1]][[predictor_order - 2]][[6]]

model_results <- neuralnet::compute(homo_model1_usd, test_data[1:predictor_order])
predicted_oneDayhead <- denormalized(model_results$net.result,non_normalize)

model_results2 <- neuralnet::compute(homo_model2_usd, test_data[1:predictor_order])
predicted_oneDayhead2 <- denormalized(model_results2$net.result,non_normalize)

model_results3 <- neuralnet::compute(homo_model3_usd, test_data[1:predictor_order])
predicted_oneDayhead3 <- denormalized(model_results3$net.result,non_normalize)



all_predicted <- cbind(predicted_oneDayhead,predicted_oneDayhead2,predicted_oneDayhead3)
all_predicted <-as.data.frame(all_predicted)

actual <- denormalized(test_data[,predictor_order+1],non_normalize)


min_value <-apply(all_predicted,1, min)
max_value <- apply(all_predicted,1,max)
mean_value <- apply(all_predicted,1,mean)

error_min <- actual - min_value
error_max <- actual - max_value
error_mean <- actual - mean_value

error_all_after_fusion <- as.data.frame(cbind(error_min,error_max,error_mean))
names(error_all_after_fusion) <- c("MIN","MAX","MEAN")



rmse_min <- rmse(error_all_after_fusion$MIN)
mae_min <- mae(error_all_after_fusion$MIN)
rmse_max <- rmse(error_all_after_fusion$MAX)
mae_max <- mae(error_all_after_fusion$MAX)
rmse_mean <- rmse(error_all_after_fusion$MEAN)
mae_mean<- mae(error_all_after_fusion$MEAN)

rmse_rate <- min(rmse_min,rmse_max,rmse_mean)

if(rmse_rate == rmse_max ){
        final_result_test_homo = list(unname(max_value),unname(error_max),"MAX",rmse_rate,mae_max)
}

if(rmse_rate == rmse_min ){
        final_result_test_homo = list(unname(min_value),unname(error_min),"MIN",rmse_rate,mae_min)
}

if(rmse_rate == rmse_mean){
        final_result_test_homo = list(unname(mean_value),unname(error_mean),"MEAN",rmse_rate,mae_mean)       
}



########################################      Hetro Model
result_hetro <- as.tbl(result_HETRO_USD)
min_hetro_value <- min(result_hetro$RMSE)
best_hetro_usd <- filter(result_hetro, RMSE == min_hetro_value)
for (i in 1:length(result_hetro$RMSE)) {
        if(result_hetro$RMSE[i]== min_hetro_value){
                row_select_hetro <- i
        }
        
}

predictor_order_hetro <- best_hetro_usd$Predictor.Order
neurons_hetro <- best_hetro_usd$Neurons
activation_func_htero <- best_hetro_usd$Activation.Func
learning_rate_hetro <- best_hetro_usd$Learning.Rate

## Getting Error and actual value from the result_usd LIST to plot
predict_value<- Result_USD_HETRO_LIST[[row_select]][[5]][[1]]       ##[[the row number in result_HOMO_*]][[5]][[1]]
predict_error <- Result_USD_HETRO_LIST[[row_select]][[5]][[2]]      ##[[the row number in result_HOMO_*]][[5]][[2]]


hetro_model1_usd <- Result_USD_HETRO_LIST[[row_select]][[5]][[6]][[1]]   ##[[the row number in result_HOMO_*][[5]][[6]][[1]]
hetro_model2_usd <- Result_USD_HETRO_LIST[[row_select]][[5]][[6]][[2]]   ##[[the row number in result_HOMO_*][[5]][[6]][[2]]
hetro_model3_usd <- Result_USD_HETRO_LIST[[row_select]][[5]][[6]][[3]]   ##[[the row number in result_HOMO_*][[5]][[6]][[3]]

#******* Training 70% ******
predict_value<- Result_USD_HETRO_LIST_70[[row_select]][[5]][[1]]       ##[[the row number in result_HOMO_*]][[5]][[1]]
predict_error <- Result_USD_HETRO_LIST_70[[row_select]][[5]][[2]]      ##[[the row number in result_HOMO_*]][[5]][[2]]


hetro_model1_usd <- Result_USD_HETRO_LIST_70[[row_select]][[5]][[6]][[1]]   ##[[the row number in result_HOMO_*][[5]][[6]][[1]]
hetro_model2_usd <- Result_USD_HETRO_LIST_70[[row_select]][[5]][[6]][[2]]   ##[[the row number in result_HOMO_*][[5]][[6]][[2]]
hetro_model3_usd <- Result_USD_HETRO_LIST_70[[row_select]][[5]][[6]][[3]]   ##[[the row number in result_HOMO_*][[5]][[6]][[3]]

#******* Training 80% ******
predict_value<- Result_USD_HETRO_LIST_80[[row_select]][[5]][[1]]       ##[[the row number in result_HOMO_*]][[5]][[1]]
predict_error <- Result_USD_HETRO_LIST_80[[row_select]][[5]][[2]]      ##[[the row number in result_HOMO_*]][[5]][[2]]


hetro_model1_usd <- Result_USD_HETRO_LIST_80[[row_select]][[5]][[6]][[1]]   ##[[the row number in result_HOMO_*][[5]][[6]][[1]]
hetro_model2_usd <- Result_USD_HETRO_LIST_80[[row_select]][[5]][[6]][[2]]   ##[[the row number in result_HOMO_*][[5]][[6]][[2]]
hetro_model3_usd <- Result_USD_HETRO_LIST_80[[row_select]][[5]][[6]][[3]]   ##[[the row number in result_HOMO_*][[5]][[6]][[3]]

test_data <- data_set[[1]][[predictor_order-2]][[3]]                  ## [[currency]][[predictor_order-2]][[1:6]] 1.Traindata,
validate_data <- data_set[[1]][[predictor_order-2]][[2]]              # 2.Validate Data 3. TestData  4.validate_date 5. Test_Date
                                                                          # 6. actual whole dataset
                                                                

#Applying three models to valadatation dataset
non_normalize <- data_set[[1]][[predictor_order - 2]][[6]]

# Model 1
model_results <- neuralnet::compute(hetro_model1_usd, test_data[1:predictor_order])
predicted_oneDayhead <- denormalized(model_results$net.result,non_normalize)

# Model 2
test_input_array_1 <- list()
for (i in 1:predictor_order){
        input_day <- as.matrix(test_data[,i]) 
        test_input_array_1 <- c(test_input_array_1,input_day)
}

test_input_array_1 <- as.numeric(test_input_array_1)


test_input <- array( test_input_array_1, dim=c(dim(input_day),predictor_order) )

model_results2 <- predictr(hetro_model2_usd, test_input)
predicted_oneDayhead2 <- denormalized(model_results2,non_normalize)

## Model 3
test_input <- as.matrix(test_data[,1:predictor_order])
model_results3 <-  predict(hetro_model3_usd,test_input)
model_results3 <- as.vector(model_results3)
predicted_oneDayhead3 <- denormalized(model_results3,non_normalize)




all_predicted <- cbind(predicted_oneDayhead,predicted_oneDayhead2,predicted_oneDayhead3)
all_predicted <-as.data.frame(all_predicted)

actual <- denormalized(test_data[,predictor_order+1],non_normalize)


min_value <-apply(all_predicted,1, min)
max_value <- apply(all_predicted,1,max)
mean_value <- apply(all_predicted,1,mean)

error_min <- actual - min_value
error_max <- actual - max_value
error_mean <- actual - mean_value

error_all_after_fusion <- as.data.frame(cbind(error_min,error_max,error_mean))
names(error_all_after_fusion) <- c("MIN","MAX","MEAN")



rmse_min <- rmse(error_all_after_fusion$MIN)
mae_min <- mae(error_all_after_fusion$MIN)
rmse_max <- rmse(error_all_after_fusion$MAX)
mae_max <- mae(error_all_after_fusion$MAX)
rmse_mean <- rmse(error_all_after_fusion$MEAN)
mae_mean<- mae(error_all_after_fusion$MEAN)

rmse_rate <- min(rmse_min,rmse_max,rmse_mean)

if(rmse_rate == rmse_max ){
        final_result_test_hetro = list(unname(max_value),unname(error_max),"MAX",rmse_rate,mae_max)
}

if(rmse_rate == rmse_min ){
        final_result_test_hetro = list(unname(min_value),unname(error_min),"MIN",rmse_rate,mae_min)
}

if(rmse_rate == rmse_mean){
        final_result_test_hetro = list(unname(mean_value),unname(error_mean),"MEAN",rmse_rate,mae_mean)       
}

