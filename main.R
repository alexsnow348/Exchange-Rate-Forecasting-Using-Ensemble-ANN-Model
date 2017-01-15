
#*********************************************************  MAIN ********************************************************************************#
library(xlsx)
# Loading required funtions and pre-processing the data set
source("data_spliting.R")

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

learning_rate <- seq(0.1,1,0.1)
learning_func <- as.character(learning_func)

#************************************************************ HOMOGENEOUS *************************************************************************#
source("HOMO.R")

#USD_ONLY MODEL (HOMO)
        result_HOMO_usd <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                      "MAE"=numeric(),"Learning_Rate"=numeric(),"Learning_Function"=character(),
                                      stringsAsFactors=FALSE)
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
                
                result_usd[[i]] <-  HOMO(train_dataset[[i]],test_dataset[[i]],usd_non_normalize[[i]], 
                                         neurons = neurons, predictor_order[i],learning_func[2], learning_rate[i])
                result_HOMO_usd[i,] <- c(i+2,ceiling((predictor_order[i]+1)/2), result_usd[[i]][4],
                                                           result_usd[[i]][5],learning_rate[i],learning_func[2]) 

        }
     
### Changes in Neuron
        
        # Predictor Order 3
        
        result_HOMO_usd_PO3 <- data.frame()
        usd_non_normalize_PO3 <- data.frame()
        test_date_PO3 <- data.frame()
        actual_usd_PO3 <- data.frame()
        neurons <- seq(3,20,1)
        
        result_usd_PO3 <- list()
        
        for (i in 1:length(neurons)) {
                
              
                train_dataset_PO3 <- data_set[[1]][[7]][[1]]
                test_dataset_PO3 <- data_set[[1]][[7]][[2]]
                test_date_PO3 <- data_set[[1]][[7]][[3]]
                usd_non_normalize_PO3 <- data_set[[1]][[7]][[4]]
                actual_usd_PO3 <- test_dataset[[1]][,4]
             
                
                result_usd_PO3[[i]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, neurons = neurons[i], 9, learning_rate)
                result_HOMO_usd_PO3 <-rbind(result_HOMO_usd_PO3, c(9,neurons[i], result_usd_PO3[[i]][4],result_usd_PO3[[i]][5],learning_rate)) 
                
        }
        names(result_HOMO_usd_PO3) <- c("Predictor_Order","Neurons","RMSE","MAE","Learning_Rate")
        write.xlsx(result_HOMO_usd_PO3, "result_HOMO_usd_PO3.xlsx")
        
        
## For Looping whole procrocess
Result_USD_HOMO_LIST <- list()
count <- 1
### Changes in Neurons and Learning Functins and Learning Rate

                for (i in 1:length(predictor_order)) {
                        result_HOMO_usd_PO3 <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
                                                          "MAE"=numeric(),"Learning_Function"=character(),"Learning_Rate"=numeric(),
                                                          stringsAsFactors=FALSE)
                        usd_non_normalize_PO3 <- data.frame()
                test_date_PO3 <- data.frame()
                actual_usd_PO3 <- data.frame()
                
                result_usd_PO3 <- list()
                
                train_dataset[[i]] <- data_set[[1]][[i]][[1]]
                test_dataset[[i]] <- data_set[[1]][[i]][[2]]
                test_date[[i]] <- data_set[[1]][[i]][[3]]
                usd_non_normalize[[i]]<- data_set[[1]][[i]][[4]]
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
                                for (k in 1:length(learning_func)) {
                                        
                                        for (j in 1:length(neurons)) {
                                                train_dataset_PO3 <- train_dataset[[i]]
                                                test_dataset_PO3 <- test_dataset[[i]]
                                                test_date_PO3 <- test_date[[i]]
                                                usd_non_normalize_PO3 <-   usd_non_normalize[[i]]
                                                actual_usd_PO3 <-  actual_usd[[i]]
                                                
                                                result_usd_PO3[[j]] <-  HOMO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                                             neurons = neurons[j], predictor_order[i], learning_func[k],learning_rate[l])
                                                result_HOMO_usd_PO3[j,] <-c(predictor_order[i],neurons[j],
                                                                                                   result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                                                                   learning_func[k],learning_rate[l])
                                                
                                                Result_USD_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                                      learning_func[k], result_usd_PO3[[j]])
                                                count <- count +1
                                        }
                                        
                                # Writing to xlsx file
                                #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",learning_func[k],"_LR_",learning_rate[l],".xlsx")
                                #write.xlsx(result_HOMO_usd_PO3, write)
                        }
                }
                
         }


#************************************************************ HETROGENEOUS *************************************************************************#
source("HETRO.R")
        
        #USD_ONLY MODEL (HETRO)     
       
        result_HETRO_usd <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                       "MAE"=numeric(),"Fusion Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
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
                
                result_usd[[i]] <-  HETRO(train_dataset[[i]],test_dataset[[i]],usd_non_normalize[[i]], neurons = neurons, predictor_order[i], learning_rate)
                result_HETRO_usd[i,] <-c(i+2,ceiling((predictor_order[i]+1)/2), result_usd[[i]][4],result_usd[[i]][5],result_usd[[i]][[3]],learning_rate) 
        
          }






