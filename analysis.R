
## Getting Error and actual value from the Result LIST to plot
predict_value<- Result_USD_HOMO_LIST[[1]][[5]][[1]]       ##[[the row number in result_HOMO_*]][[5]][[1]]
predict_error <- Result_USD_HOMO_LIST[[1]][[5]][[2]]      ##[[the row number in result_HOMO_*]][[5]][[2]]


homo_model1 <- Result_USD_HOMO_LIST[[1]][[5]][[6]][[1]]   ##[[the row number in result_HOMO_*][[5]][[6]][[1]]
homo_model2 <- Result_USD_HOMO_LIST[[1]][[5]][[6]][[2]]   ##[[the row number in result_HOMO_*][[5]][[6]][[2]]
homo_model3 <- Result_USD_HOMO_LIST[[1]][[5]][[6]][[3]]   ##[[the row number in result_HOMO_*][[5]][[6]][[3]]

validate_data <- data_set[[1]][[1]][[3]]                  ## [[currency]][[predictor_order-2]][[1:6]] 1.Traindata,
                                                          # 2. TestData 3. Validate Data 4.Test_Date 5. Validate_Date
                                                          # 6. actual whole dataset


#Applying three models to valadatation dataset
usd_non_normalize <- data_set[[1]][[predictor_order - 2]][[6]]

model_results <- neuralnet::compute(homo_model1, validate_data[1:predictor_order])
predicted_oneDayhead <- denormalized(model_results$net.result,usd_non_normalize)

model_results2 <- neuralnet::compute(homo_model2, validate_data[1:predictor_order])
predicted_oneDayhead2 <- denormalized(model_results2$net.result,usd_non_normalize)

model_results3 <- neuralnet::compute(homo_model3, validate_data[1:predictor_order])
predicted_oneDayhead3 <- denormalized(model_results3$net.result,usd_non_normalize)



all_predicted <- cbind(predicted_oneDayhead,predicted_oneDayhead2,predicted_oneDayhead3)
all_predicted <-as.data.frame(all_predicted)

actual <- denormalized(validate_data[,predictor_order+1],usd_non_normalize)
actual <- denormalized(test_dataset[,predictor_order+1],usd_non_normalize)

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
        final_result = list(unname(max_value),unname(error_max),"MAX",rmse_rate,mae_max)
}

if(rmse_rate == rmse_min ){
        final_result = list(unname(min_value),unname(error_min),"MIN",rmse_rate,mae_min)
}

if(rmse_rate == rmse_mean){
        final_result = list(unname(mean_value),unname(error_mean),"MEAN",rmse_rate,mae_mean)       
}

