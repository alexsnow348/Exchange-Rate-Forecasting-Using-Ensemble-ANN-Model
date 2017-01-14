## HETROGENEOUS MODEL


#********************** HETROGENEOUS Network construction function *********************#

##Arguments
# train_dataset: the 60 % of the data to train the network
# test_dataset: the 40 % of the data to test the network
# predictor_order: the no. of supplied past historical data (sample input range: 3 <-> 10)
# learning_rate : the learning rate to train the network (sample input range : 1 <-> 0.05)

# Return Values 
# 

HETRO <- function(){
        source("MLP1.R")
        source("RNN.R")
        source("RBF.R")
        require(RSNNS)
## FIRST MLP
        set.seed(1)
        weight_size =length(train_dataset[,1])
        weight1 <- sample(1:10,size = weight_size,replace = T)
        weight1 = normalizeData(weight1, type = "0_1")
        
        ## Train the network using neuralnet (First MLP)
        first <- MLP1( train_dataset,test_dataset,usd_non_normalize,predictor_order,neurons,learning_rate,weight1)
        
        ## First performance ERROR
        first_mae <- mae(first[[2]])
        first_rmse<- rmse(first[[2]])
        
## SECOND RNN
        set.seed(2)
        weight_size =length(train_dataset[,1])
        weight2 <- sample(1:1000,size = weight_size,replace = F)
        weight2 = normalizeData(weight2, type = "0_1")
        seond <- RNN(train_dataset, test_dataset,usd_non_normalize, predictor_order, learning_rate)
        
        ## Second Performance ERROR
        second_mae<- mae(second[[2]])
        second_rmse<-  rmse(second[[2]])

## THIRD MLP
        set.seed(3)
        weight_size =length(train_dataset[,1])
        weight3 <- sample(1:1000,size = weight_size,replace = F)
        weight3 = normalizeData(weight3, type = "0_1")
        
        ## Train the network using neuralnet (First MLP)
        third <-  first <- MLP( train_dataset,test_dataset,usd_non_normalize,predictor_order,learning_rate,weight3)
        
        ## Third Performance ERROR
        third_mae <- mae(third[[2]])
        third_rmse <- rmse(third[[2]])
        
}