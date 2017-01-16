
#*********************************************************  MAIN ********************************************************************************#
library(xlsx)
# Loading required funtions and pre-processing the data set
source("data_spliting_V.R")

# Spliting the data
data_set <- data_spliting()                             # Usage of Data_set
                                                        # data_set[[language_type]][[Predictor_order]][1:4]
                                                        #****Lanuage Type **** #   #**** Predictor_ordr****#
                                                        #   1. USD    5. PON   #   #  1. PO_3   5. PO_7
                                                        #   2. AUS    6. SIN   #   #  2. PO_4   6. PO_8
                                                        #   3. CAN    7, SWI   #   #  3. PO_5   7. PO_9
train_dataset <- list()                                 #   4. EURO            #   #  4. PO_6   8. PO_10
test_dataset <- list() 
predictor_order <- seq(3,10,1)
test_date <- list()
usd_non_normalize <- list()
result_usd <- list()
actual_usd <- list()
learning_rate <- seq(0.1,1,0.1)
activation_func <- c("logistic", "tanh")


#************************************************************ HOMOGENEOUS *************************************************************************#

source("HOMO.R")

#************************************* U.S. Dollar **************************************************#      

Result_USD_HOMO_LIST <- list()
count <- 1
### Changes in Neurons and Learning Functins and Learning Rate

                for (i in 1:length(predictor_order)) {
                        result_HOMO_usd_PO3 <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                                          "MAE"=numeric(),"Activation_Function"=character(),"Learning_Rate"=numeric(),
                                                          stringsAsFactors=FALSE)
                        usd_non_normalize_PO3 <- data.frame()
                test_date_PO3 <- data.frame()
                actual_usd_PO3 <- data.frame()
                
                result_usd_PO3 <- list()
                
                train_dataset[[i]] <- data_set[[1]][[i]][[1]]
                test_dataset[[i]] <- data_set[[1]][[i]][[2]]
                test_date[[i]] <- data_set[[1]][[i]][[4]]
                usd_non_normalize[[i]]<- data_set[[1]][[i]][[6]]
                actual_usd[[i]] <- test_dataset[[i]][,i+3]
                
                if(predictor_order[i]==3){ neurons<-seq(2,20,1)}
                if(predictor_order[i]==4){ neurons<-seq(3,20,1)}
                if(predictor_order[i]==5){ neurons<-seq(3,20,1)}
                if(predictor_order[i]==6){ neurons<-seq(4,20,1)}
                if(predictor_order[i]==7){ neurons<-seq(4,20,1)}
                if(predictor_order[i]==8){ neurons<-seq(5,20,1)}
                if(predictor_order[i]==9){ neurons<-seq(5,20,1)}
                if(predictor_order[i]==10){ neurons<-seq(6,20,1)}
                
                for (l in 1:length(learning_rate) ){     
                                for (k in 1:length(activation_func)) {
                                        
                                        for (j in 1:length(neurons)) {
                                                train_dataset_PO3 <- train_dataset[[i]]
                                                test_dataset_PO3 <- test_dataset[[i]]
                                                test_date_PO3 <- test_date[[i]]
                                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                                actual_usd_PO3 <-  actual_usd[[i]]
                                                
                                                result_usd_PO3[[j]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                                result_HOMO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                                            activation_func[k],learning_rate[l])
                                                
                                                Result_USD_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                                      activation_func[k], result_usd_PO3[[j]])
                                                count <- count +1
                                        }
                                        
                                # Writing to xlsx file
                                #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
                                #write.xlsx(result_HOMO_usd_PO3, write)
                        }
                }
                
         }

#************************************* Australian Dollar **************************************************#

Result_AUS_HOMO_LIST <- list()
count <- 1

        for (i in 1:length(predictor_order)) {
        result_HOMO_usd_PO3 <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                          "MAE"=numeric(),"Activation_Function"=character(),"Learning_Rate"=numeric(),
                                          stringsAsFactors=FALSE)
        usd_non_normalize_PO3 <- data.frame()
        test_date_PO3 <- data.frame()
        actual_usd_PO3 <- data.frame()
        
        result_usd_PO3 <- list()
        
        train_dataset[[i]] <- data_set[[2]][[i]][[1]]
        test_dataset[[i]] <- data_set[[2]][[i]][[2]]
        test_date[[i]] <- data_set[[2]][[i]][[4]]
        usd_non_normalize[[i]]<- data_set[[2]][[i]][[6]]
        actual_usd[[i]] <- test_dataset[[i]][,i+3]
        
        if(predictor_order[i]==3){ neurons<-seq(2,20,1)}
        if(predictor_order[i]==4){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==5){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==6){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==7){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==8){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==9){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==10){ neurons<-seq(6,20,1)}
        
        for (l in 1:length(learning_rate) ){     
                for (k in 1:length(activation_func)) {
                        
                        for (j in 1:length(neurons)) {
                                train_dataset_PO3 <- train_dataset[[i]]
                                test_dataset_PO3 <- test_dataset[[i]]
                                test_date_PO3 <- test_date[[i]]
                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                actual_usd_PO3 <-  actual_usd[[i]]
                                
                                result_usd_PO3[[j]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HOMO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_AUS_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                        }
                        
                        # Writing to xlsx file
                        #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
                        #write.xlsx(result_HOMO_usd_PO3, write)
                }
        }
        
}

#************************************* Canadian Dollar **************************************************#

Result_CAN_HOMO_LIST <- list()
count <- 1

        for (i in 1:length(predictor_order)) {
        result_HOMO_usd_PO3 <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                          "MAE"=numeric(),"Activation_Function"=character(),"Learning_Rate"=numeric(),
                                          stringsAsFactors=FALSE)
        usd_non_normalize_PO3 <- data.frame()
        test_date_PO3 <- data.frame()
        actual_usd_PO3 <- data.frame()
        
        result_usd_PO3 <- list()
        
        train_dataset[[i]] <- data_set[[3]][[i]][[1]]
        test_dataset[[i]] <- data_set[[3]][[i]][[2]]
        test_date[[i]] <- data_set[[3]][[i]][[4]]
        usd_non_normalize[[i]]<- data_set[[3]][[i]][[6]]
        actual_usd[[i]] <- test_dataset[[i]][,i+3]
        
        if(predictor_order[i]==3){ neurons<-seq(2,20,1)}
        if(predictor_order[i]==4){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==5){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==6){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==7){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==8){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==9){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==10){ neurons<-seq(6,20,1)}
        
        for (l in 1:length(learning_rate) ){     
                for (k in 1:length(activation_func)) {
                        
                        for (j in 1:length(neurons)) {
                                train_dataset_PO3 <- train_dataset[[i]]
                                test_dataset_PO3 <- test_dataset[[i]]
                                test_date_PO3 <- test_date[[i]]
                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                actual_usd_PO3 <-  actual_usd[[i]]
                                
                                result_usd_PO3[[j]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HOMO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_CAN_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                        }
                        
                        # Writing to xlsx file
                        #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
                        #write.xlsx(result_HOMO_usd_PO3, write)
                }
        }
        
}

#****************************************** EURO ********************************************************#

Result_EURO_HOMO_LIST <- list()
count <- 1

        for (i in 1:length(predictor_order)) {
        result_HOMO_usd_PO3 <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                          "MAE"=numeric(),"Activation_Function"=character(),"Learning_Rate"=numeric(),
                                          stringsAsFactors=FALSE)
        usd_non_normalize_PO3 <- data.frame()
        test_date_PO3 <- data.frame()
        actual_usd_PO3 <- data.frame()
        
        result_usd_PO3 <- list()
        
        train_dataset[[i]] <- data_set[[4]][[i]][[1]]
        test_dataset[[i]] <- data_set[[4]][[i]][[2]]
        test_date[[i]] <- data_set[[4]][[i]][[4]]
        usd_non_normalize[[i]]<- data_set[[4]][[i]][[6]]
        actual_usd[[i]] <- test_dataset[[i]][,i+3]
        
        if(predictor_order[i]==3){ neurons<-seq(2,20,1)}
        if(predictor_order[i]==4){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==5){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==6){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==7){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==8){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==9){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==10){ neurons<-seq(6,20,1)}
        
        for (l in 1:length(learning_rate) ){     
                for (k in 1:length(activation_func)) {
                        
                        for (j in 1:length(neurons)) {
                                train_dataset_PO3 <- train_dataset[[i]]
                                test_dataset_PO3 <- test_dataset[[i]]
                                test_date_PO3 <- test_date[[i]]
                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                actual_usd_PO3 <-  actual_usd[[i]]
                                
                                result_usd_PO3[[j]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HOMO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_EURO_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                        }
                        
                        # Writing to xlsx file
                        #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",learning_func[k],"_LR_",learning_rate[l],".xlsx")
                        #write.xlsx(result_HOMO_usd_PO3, write)
                }
        }
        
}


#****************************************** Pound Sterling **********************************************#

Result_PON_HOMO_LIST <- list()
count <- 1

        for (i in 1:length(predictor_order)) {
        result_HOMO_usd_PO3 <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                          "MAE"=numeric(),"Activation_Function"=character(),"Learning_Rate"=numeric(),
                                          stringsAsFactors=FALSE)
        usd_non_normalize_PO3 <- data.frame()
        test_date_PO3 <- data.frame()
        actual_usd_PO3 <- data.frame()
        
        result_usd_PO3 <- list()
        
        train_dataset[[i]] <- data_set[[5]][[i]][[1]]
        test_dataset[[i]] <- data_set[[5]][[i]][[2]]
        test_date[[i]] <- data_set[[5]][[i]][[4]]
        usd_non_normalize[[i]]<- data_set[[5]][[i]][[6]]
        actual_usd[[i]] <- test_dataset[[i]][,i+3]
        
        if(predictor_order[i]==3){ neurons<-seq(2,20,1)}
        if(predictor_order[i]==4){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==5){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==6){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==7){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==8){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==9){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==10){ neurons<-seq(6,20,1)}
        
        for (l in 1:length(learning_rate) ){     
                for (k in 1:length(activation_func)) {
                        
                        for (j in 1:length(neurons)) {
                                train_dataset_PO3 <- train_dataset[[i]]
                                test_dataset_PO3 <- test_dataset[[i]]
                                test_date_PO3 <- test_date[[i]]
                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                actual_usd_PO3 <-  actual_usd[[i]]
                                
                                result_usd_PO3[[j]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HOMO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_PON_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                        }
                        
                        # Writing to xlsx file
                        #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
                        #write.xlsx(result_HOMO_usd_PO3, write)
                }
        }
        
}

#****************************************** Singapore Dollar ********************************************#

Result_SIN_HOMO_LIST <- list()
count <- 1

        for (i in 1:length(predictor_order)) {
        result_HOMO_usd_PO3 <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                          "MAE"=numeric(),"Activation_Function"=character(),"Learning_Rate"=numeric(),
                                          stringsAsFactors=FALSE)
        usd_non_normalize_PO3 <- data.frame()
        test_date_PO3 <- data.frame()
        actual_usd_PO3 <- data.frame()
        
        result_usd_PO3 <- list()
        
        train_dataset[[i]] <- data_set[[6]][[i]][[1]]
        test_dataset[[i]] <- data_set[[6]][[i]][[2]]
        test_date[[i]] <- data_set[[6]][[i]][[4]]
        usd_non_normalize[[i]]<- data_set[[6]][[i]][[6]]
        actual_usd[[i]] <- test_dataset[[i]][,i+3]
        
        if(predictor_order[i]==3){ neurons<-seq(2,20,1)}
        if(predictor_order[i]==4){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==5){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==6){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==7){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==8){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==9){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==10){ neurons<-seq(6,20,1)}
        
        for (l in 1:length(learning_rate) ){     
                for (k in 1:length(activation_func)) {
                        
                        for (j in 1:length(neurons)) {
                                train_dataset_PO3 <- train_dataset[[i]]
                                test_dataset_PO3 <- test_dataset[[i]]
                                test_date_PO3 <- test_date[[i]]
                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                actual_usd_PO3 <-  actual_usd[[i]]
                                
                                result_usd_PO3[[j]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HOMO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_SIN_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                        }
                        
                        # Writing to xlsx file
                        #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
                        #write.xlsx(result_HOMO_usd_PO3, write)
                }
        }
        
}


#******************************************  Swiss Franc    ********************************************#

Result_SWI_HOMO_LIST <- list()
count <- 1

        for (i in 1:length(predictor_order)) {
        result_HOMO_usd_PO3 <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                          "MAE"=numeric(),"Activation_Function"=character(),"Learning_Rate"=numeric(),
                                          stringsAsFactors=FALSE)
        usd_non_normalize_PO3 <- data.frame()
        test_date_PO3 <- data.frame()
        actual_usd_PO3 <- data.frame()
        
        result_usd_PO3 <- list()
        
        train_dataset[[i]] <- data_set[[7]][[i]][[1]]
        test_dataset[[i]] <- data_set[[7]][[i]][[2]]
        test_date[[i]] <- data_set[[7]][[i]][[4]]
        usd_non_normalize[[i]]<- data_set[[7]][[i]][[6]]
        actual_usd[[i]] <- test_dataset[[i]][,i+3]
        
        if(predictor_order[i]==3){ neurons<-seq(2,20,1)}
        if(predictor_order[i]==4){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==5){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==6){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==7){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==8){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==9){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==10){ neurons<-seq(6,20,1)}
        
        for (l in 1:length(learning_rate) ){     
                for (k in 1:length(activation_func)) {
                        
                        for (j in 1:length(neurons)) {
                                train_dataset_PO3 <- train_dataset[[i]]
                                test_dataset_PO3 <- test_dataset[[i]]
                                test_date_PO3 <- test_date[[i]]
                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                actual_usd_PO3 <-  actual_usd[[i]]
                                
                                result_usd_PO3[[j]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HOMO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_SWI_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                        }
                        
                        # Writing to xlsx file
                        #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
                        #write.xlsx(result_HOMO_usd_PO3, write)
                }
        }
        
}



#************************************************************ HETROGENEOUS *************************************************************************#

source("HETRO.R")
        
#************************************* U.S. Dollar **************************************************#   
Result_USD_HETRO_LIST <- list()
count <- 1

for (i in 1:length(predictor_order)) {
        result_HETRO_usd_PO3 <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                       "MAE"=numeric(),"Fusion Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
     
        usd_non_normalize_PO3 <- data.frame()
        test_date_PO3 <- data.frame()
        actual_usd_PO3 <- data.frame()
        
        result_usd_PO3 <- list()
        
        train_dataset[[i]] <- data_set[[7]][[i]][[1]]
        test_dataset[[i]] <- data_set[[7]][[i]][[2]]
        test_date[[i]] <- data_set[[7]][[i]][[4]]
        usd_non_normalize[[i]]<- data_set[[7]][[i]][[6]]
        actual_usd[[i]] <- test_dataset[[i]][,i+3]
        
        if(predictor_order[i]==3){ neurons<-seq(2,20,1)}
        if(predictor_order[i]==4){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==5){ neurons<-seq(3,20,1)}
        if(predictor_order[i]==6){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==7){ neurons<-seq(4,20,1)}
        if(predictor_order[i]==8){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==9){ neurons<-seq(5,20,1)}
        if(predictor_order[i]==10){ neurons<-seq(6,20,1)}
                
        
        
        for (l in 1:length(learning_rate) ){     
                for (k in 1:length(activation_func)) {
                        
                        for (j in 1:length(neurons)) {
                                train_dataset_PO3 <- train_dataset[[i]]
                                test_dataset_PO3 <- test_dataset[[i]]
                                test_date_PO3 <- test_date[[i]]
                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                actual_usd_PO3 <-  actual_usd[[i]]
                                
                                result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HETRO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_USD_HETRO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                        }
                        
                }
        }

}
      




