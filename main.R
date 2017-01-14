
#***************************************** MAIN *************************************************#

# Loading required funtions and pre-processing the data set
source("data_spliting.R")

# Spliting the data
data_set <- data_spliting()                             # Usage of Data_set
                                                        # data_set[[language_type]][[Predictor_order]][1:4]
                                                        #****Lanuage Type **** #   #**** Predictor_ordr****#
                                                        #   1. USD    5. PON   #   #  1. PO_3   5. PO_7
# HOMOGENEOUS MODEL                                     #   2. AUS    6. SIN   #   #  2. PO_4   6. PO_8
source("HOMO.R")                                        #   3. CAN    7, SWI   #   #  3. PO_5   7. PO_9
train_dataset <- list()                                 #   4. EURO            #   #  4. PO_6   8. PO_10
test_dataset <- list() 
predictor_order <- seq(3,10,1)
test_date <- list()


learning_rate <- 0.1

#************************************************************ HOMOGENEOUS *************************************************************************#

#USD_ONLY MODEL (HOMO)
        result_HOMO_usd <- data.frame()
        usd_non_normalize <- list()
        result_usd <- list()
        actual_usd <- list()
        for (i in 1:length(predictor_order)) {
               
                train_dataset[[i]] <- data_set[[1]][[i]][[1]]
                test_dataset[[i]] <- data_set[[1]][[i]][[2]]
                test_date[[i]] <- data_set[[1]][[i]][[3]]
                usd_non_normalize[[i]]<- data_set[[1]][[i]][[4]]
                actual_usd[[i]] <- test_dataset[[i]][,i+3]
                neurons <- ceiling((i+3)/2)
                
                result_usd[[i]] <-  HOMO(train_dataset[[i]],test_dataset[[i]],usd_non_normalize[[i]], neurons = neurons, predictor_order[i], learning_rate)
                result_HOMO_usd <-rbind(result_HOMO_usd, c(i+2,ceiling((predictor_order[i]+1)/2), result_usd[[i]][4], result_usd[[i]][5],
                                                           result_usd[[i]][3],learning_rate)) 

        }
        names(result_HOMO_usd) <- c("Predictor_Order","Neurons","RMSE","MAE","Fusion_Func","Learning_Rate")
        
#AUS_ONLY MODEL (HOMO)        
        
        
        
        
        
        
        
        
        
#************************************************************ HOMOGENEOUS *************************************************************************#

#USD_ONLY MODEL (HETRO)
           for (i in 1:length(predictor_order)) {
                
                train_dataset[[i]] <- data_set[[1]][[i]][[1]]
                test_dataset[[i]] <- data_set[[1]][[i]][[2]]
                test_date[[i]] <- data_set[[1]][[i]][[3]]
                usd_non_normalize[[i]]<- data_set[[1]][[i]][[4]]
                actual_data[[i]] <- test_dataset[[i]][,i+3]
                neurons <- ceiling((i+3)/2)
                
                result_usd[[i]] <-  HETRO(train_dataset[[i]],test_dataset[[i]],usd_non_normalize[[i]], neurons = neurons, predictor_order[i], learning_rate)
                result_towrite_usd <-rbind( result_towrite_usd, c(i+2,ceiling((predictor_order[i]+1)/2), result_usd[[i]][4], result_usd[[i]][5],result_usd[[i]][3])) 
                
        }
        
        names(result_towrite_usd) <- c("Predictor Order","Neurons","RMSE","MAE","OPIMIZED FUSION FUNC")



# Writing to xlsx file
library(xlsx)
write.xlsx(result_towrite, "old_MLP_result.xlsx")


