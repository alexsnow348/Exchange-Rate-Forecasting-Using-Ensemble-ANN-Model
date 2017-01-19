## HOMOGENEOUS MODEL


#********************** HOMOGENEOUS Network construction function *********************#

##Arguments
# train_dataset: the 60 % of the data to train the network
# test_dataset: the 40 % of the data to test the network
# predictor_order: the no. of supplied past historical data (sample input range: 3 <-> 10)
# learning_rate : the learning rate to train the network (sample input range : 1 <-> 0.05)

# Return Values 
# 


HOMO <- function(train_dataset,test_dataset,usd_non_normalize,neurons,predictor_order,activation_func,learning_rate){
        source("MLP.R")
       
## FIRST MLP
        set.seed(1)
        weight_size =length(train_dataset[,1])
        weight1 <- sample(1:10000,size = weight_size,replace = F)
        weight1 = normalized(weight1)
        
        ## Train the network using neuralnet (First MLP)
        first <- MLP( train_dataset,test_dataset,usd_non_normalize,predictor_order, neurons,learning_rate,activation_func,weight1)
        
        ## First performance ERROR
        first_mae <- mae(first[[2]])
        first_rmse<- rmse(first[[2]])
        first_model <- first[[3]]
        
## SECOND MLP
        set.seed(2)
        weight_size =length(train_dataset[,1])
        weight2 <- sample(1:10000,size = weight_size,replace = F)
        weight2 = normalized(weight2)
        ## Train the network using neuralnet (First MLP)
        second <- MLP( train_dataset,test_dataset,usd_non_normalize,predictor_order,neurons,learning_rate,activation_func,weight2)
        
        ## Second Performance ERROR
       second_mae<- mae(second[[2]])
       second_rmse<-  rmse(second[[2]])
       second_model <- second[[3]]
        
## THIRD MLP
        set.seed(3)
        weight_size =length(train_dataset[,1])
        weight3 <- sample(1:10000,size = weight_size,replace = F)
        weight3 = normalized(weight3)
        ## Train the network using neuralnet (First MLP)
        third <-  MLP(train_dataset,test_dataset,usd_non_normalize,predictor_order,neurons,learning_rate,activation_func,weight3)
        
        ## Third Performance ERROR
        third_mae <- mae(third[[2]])
        third_rmse <- rmse(third[[2]])
        third_model <- third[[3]]
      
### Three model
        models <- list(first_model,second_model,third_model)
        
## Predicted Value from all Three Network
       
        all_predicted <- cbind(first[[1]],second[[1]],third[[1]])
        all_predicted <-as.data.frame(all_predicted)
        
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
                final_result = list(unname(max_value),unname(error_max),"MAX",rmse_rate,mae_max,models)
        }
        
        if(rmse_rate == rmse_min ){
                final_result = list(unname(min_value),unname(error_min),"MIN",rmse_rate,mae_min,models)
        }
        
        if(rmse_rate == rmse_mean){
                final_result = list(unname(mean_value),unname(error_mean),"MEAN",rmse_rate,mae_mean,models)       
        }
        
        return(final_result)
        
}