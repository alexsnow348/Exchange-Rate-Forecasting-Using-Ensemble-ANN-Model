
#********************** RBF Network construction function *********************#

##Arguments
# train_dataset: the 60 % of the data to train the network
# test_dataset: the 40 % of the data to test the network
# predictor_order: the no. of supplied past historical data (sample input range: 3 <-> 10)
# learning_rate : the learning rate to train the network (sample input range : 1 <-> 0.05)

# Return Values 
# Predicted Value and Error Results

RBF <- function(train_dataset, test_dataset, usd_non_normalize,neurons, predictor_order, learning_rate){
        require("neural")
        train_input <- as.matrix(train_dataset[,1:predictor_order])
        train_output <- as.matrix(train_dataset[,predictor_order+1])
        test_input <- as.matrix(test_dataset[,1:predictor_order])
        test_actual <- as.vector(test_dataset[,predictor_order+1])
         
        data<-rbftrain(train_input,neurons,train_output, alfa= learning_rate , it= 100, sigma=NaN,visual = F)
        result <- rbf(test_input,data$weight,data$dist,data$neurons,data$sigma)
        result <- as.vector(result)
        result <- denormalized(result,usd_non_normalize)
        test_actual <- denormalized(test_actual,usd_non_normalize)
        error <- test_actual - result
        rmse(error)
        mae(error)
        final_result <- list(predict_value,error)
        return(final_result)
}


