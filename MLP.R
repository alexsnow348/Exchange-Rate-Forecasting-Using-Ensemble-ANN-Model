
#********************** MLP Network construction function *********************#

##Arguments
# train_dataset: the 60 % of the data to train the network
# test_dataset: the 40 % of the data to test the network
# predictor_order: the no. of supplied past historical data (sample input range: 3 <-> 10)
# learning_rate : the learning rate to train the network (sample input range : 1 <-> 0.05)

# Return Values 
# Predicted Value and Error Results


MLP <- function(train_dataset,test_dataset,usd_non_normalize, predictor_order, learning_rate, weights){
        require("neuralnet")
        hidden_neurons = ceiling((predictor_order + 1)/2)
        
        # Training
        if(predictor_order == 3){
                exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay, learningrate = learning_rate,
                                            data = train_dataset, hidden = hidden_neurons, startweights = weights) 
        }
        
        if(predictor_order == 4){
                exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay + fourthDay, learningrate = learning_rate,
                                            data = train_dataset, hidden = hidden_neurons, startweights = weights) 
        }
        if(predictor_order == 5){
                exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay + fourthDay + FifthDay, learningrate = learning_rate,
                                            data = train_dataset, hidden = hidden_neurons, startweights = weights) 
        }
        if(predictor_order == 6){
                exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay + fourthDay + FifthDay + SixthDay, learningrate = learning_rate,
                                            data = train_dataset, hidden = hidden_neurons, startweights = weights) 
        }
        
        if(predictor_order == 7){
                exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay + fourthDay + FifthDay + SixthDay + SeventhDay , 
                                            learningrate = learning_rate, data = train_dataset, hidden = hidden_neurons,startweights = weights) 
        }
        if(predictor_order == 8){
                exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay + fourthDay + FifthDay + SixthDay + SeventhDay + eighthDay,
                                            learningrate = learning_rate, data = train_dataset, hidden = hidden_neurons, startweights = weights) 
        }
        
        if(predictor_order == 9){
                exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay + fourthDay + FifthDay + SixthDay + SeventhDay+eighthDay+ninethDay, 
                                            learningrate = learning_rate, data = train_dataset, hidden = hidden_neurons, startweights = weights) 
        }
        if(predictor_order == 10){
                exchange_model <- neuralnet(oneDayAhead ~ firstDay + secondDay + thirdDay + fourthDay + FifthDay + SixthDay + SeventhDay + eighthDay + ninethDay+tenthDay,
                                            learningrate = learning_rate, data = train_dataset, hidden = hidden_neurons, startweights = weights) 
        }
        
       
        
        # Testing and Error Result
        model_results <- neuralnet::compute(exchange_model, test_dataset[1:predictor_order])
        predicted_oneDayhead <- model_results$net.result
        predict_value <- denormalized(predicted_oneDayhead,usd_non_normalize)
        actual <- denormalized(test_dataset[,predictor_order+1],usd_non_normalize)
        error <- actual - predict_value
        final_result <- list(predict_value,error)
        return(final_result)
}


