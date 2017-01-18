
#*********************************************************  MAIN ********************************************************************************#

# Loading required funtions and pre-processing the data set
# Data Source

url <- "/home/wut/Desktop/Link to Data/FYP Program/Raw Data/alldata.csv"
#url <- "E:/WUT FYP DATA/FYP Program/FYP Program/Data/alldata.csv"
source("data_spliting.R")

# Train 60%
train_per <- 0.6
data_set <- data_spliting(url,train_per)

                                                        # Usage of Data_set
                                                        # data_set[[language_type]][[Predictor_order]][1:6]
                                                        #****Lanuage Type **** #   #**** Predictor_ordr****#
                                                        #   1. USD    5. AUD   #   #  1. PO_3   5. PO_7
                                                        #   2. GBP    6. CAD   #   #  2. PO_4   6. PO_8
                                                        #   3. EUR    7, SGD   #   #  3. PO_5   7. PO_9
                                                        #   4. CHF             #   #  4. PO_6   8. PO_10
                                        

train_dataset <- list()                                 
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

#************************************* USD **************************************************#      

Result_USD_HOMO_LIST <- list()
count <- 1
count2 <- 1
### Changes in Neurons and Learning Functins and Learning Rate

        for (i in 1:length(predictor_order)) {
                        result_HOMO_USD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                                                             neurons = neurons[j], predictor_order[i], 
                                                                             activation_func[k],learning_rate[l])
                                                
                                                result_HOMO_USD[count2,] <-c(predictor_order[i],neurons[j],
                                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                                            activation_func[k],learning_rate[l])
                                                
                                                Result_USD_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                                      activation_func[k], result_usd_PO3[[j]])
                                                count <- count +1
                                                count2 <- count2 +1
                                        }
                                        
                        }
                }
                
         }

# Writing result to xlsx file
library(xlsx)
write.xlsx(result_HOMO_USD,"result_HOMO_USD_Train_60.xlsx")
  
#************************************* GBP  **************************************************#

Result_GBP_HOMO_LIST <- list()
count <- 1
count2 <- 1
        for (i in 1:length(predictor_order)) {
                result_HOMO_GBP <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                                             neurons = neurons[j], predictor_order[i], 
                                                             activation_func[k],learning_rate[l])
                                
                                result_HOMO_GBP[count2,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_GBP_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 +1
                        }
                        
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HOMO_GBP,"result_HOMO_GBP_Train_60.xlsx")

#************************************* EUR  **************************************************#

Result_EUR_HOMO_LIST <- list()
count <- 1
count2 <- 1
        for (i in 1:length(predictor_order)) {
                result_HOMO_EUR <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                result_HOMO_EUR[count2,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_EUR_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 +1
                        }
                        
                }
        }
        
        }

# Writing result to xlsx file
write.xlsx(result_HOMO_EUR,"result_HOMO_EUR_Train_60.xlsx")

#****************************************** CHF ***********************************************#

Result_CHF_HOMO_LIST <- list()
count <- 1
count2 <- 1

        for (i in 1:length(predictor_order)) {
                result_CHF_EURO <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                result_CHF_EURO[count2,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_CHF_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 +1
                        }
                        
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HOMO_CHF,"result_HOMO_CHF_Train_60.xlsx")

#****************************************** AUD ***********************************************#

Result_AUD_HOMO_LIST <- list()
count <- 1
count2 <- 1

        for (i in 1:length(predictor_order)) {
        result_HOMO_AUD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                result_HOMO_AUD[count2,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_AUD_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 +1
                        }
                        
                        # Writing to xlsx file
                        #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
                        #write.xlsx(result_HOMO_usd_PO3, write)
                }
        }
        
        }

# Writing result to xlsx file
write.xlsx(result_HOMO_AUD,"result_HOMO_AUD_Train_60.xlsx")


#****************************************** CAD ***********************************************#

Result_CAD_HOMO_LIST <- list()
count <- 1
count2 <- 1
        for (i in 1:length(predictor_order)) {
        result_HOMO_CAD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                result_HOMO_CAD[count2,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_CAD_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 +1
                        }
                        
                        # Writing to xlsx file
                        #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
                        #write.xlsx(result_HOMO_usd_PO3, write)
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HOMO_CAD,"result_HOMO_CAD_Train_60.xlsx")

#******************************************  SGD   ********************************************#

Result_SGD_HOMO_LIST <- list()
count <- 1
count2 <- 1

        for (i in 1:length(predictor_order)) {
                result_HOMO_SGD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                result_HOMO_SGD[count2,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_SGD_HOMO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                      activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 +1
                        }
                        
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HOMO_SGD,"result_HOMO_SGD_Train_60.xlsx")

#************************************************************ HETROGENEOUS *************************************************************************#

source("HETRO.R")
        
#************************************* USD *****************************************************#   
Result_USD_HETRO_LIST <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
        result_HETRO_USD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                       "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
     
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
                                
                                result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                             neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HETRO_USD[count2,] <-c(predictor_order[i],neurons[j],
                                                            result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                            activation_func[k],learning_rate[l])
                                
                                Result_USD_HETRO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 + 1
                        }
                        
                }
        }

}

# Writing result to xlsx file
write.xlsx(result_HETRO_USD,"result_HETRO_USD_Train_60.xlsx")
      
#************************************* GBP ****************************************************# 
Result_GBP_HETRO_LIST <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
        result_HETRO_GBP <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                       "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
        
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
                                
                                result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                              neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HETRO_GBP[count2,] <-c(predictor_order[i],neurons[j],
                                                              result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                              activation_func[k],learning_rate[l])
                                
                                Result_GBP_HETRO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 + 1
                        }
                        
                }
        }
        
}


# Writing result to xlsx file
write.xlsx(result_HETRO_GBP,"result_HETRO_GBP_Train_60.xlsx")

#************************************* EUR ***************************************************#
Result_EUR_HETRO_LIST <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
        result_HETRO_EUR <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                       "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
        
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
                                
                                result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                              neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HETRO_EUR[count2,] <-c(predictor_order[i],neurons[j],
                                                              result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                              activation_func[k],learning_rate[l])
                                
                                Result_EUR_HETRO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 + 1
                        }
                        
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HETRO_EUR,"result_HETRO_EUR_Train_60.xlsx")

#****************************************** CHF ***********************************************#
Result_CHF_HETRO_LIST <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
        result_HETRO_CHF <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                       "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
        
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
                                
                                result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                              neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HETRO_CHF[count2,] <-c(predictor_order[i],neurons[j],
                                                              result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                              activation_func[k],learning_rate[l])
                                
                                Result_CHF_HETRO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 + 1
                        }
                        
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HETRO_CHF,"result_HETRO_CHF_Train_60.xlsx")

#****************************************** AUD  **********************************************#
Result_AUD_HETRO_LIST <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
        result_HETRO_AUD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                        "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
        
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
                                
                                result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                              neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HETRO_AUD[count2,] <-c(predictor_order[i],neurons[j],
                                                               result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                               activation_func[k],learning_rate[l])
                                
                                Result_AUD_HETRO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                        activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 + 1
                        }
                        
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HETRO_AUD,"result_HETRO_AUD_Train_60.xlsx")

#****************************************** CAD **********************************************#
Result_CAD_HETRO_LIST <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
        result_HETRO_CAD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                       "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
        
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
                                
                                result_usd_CAD[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                                              neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
                                result_HETRO_CAD[count2,] <-c(predictor_order[i],neurons[j],
                                                              result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                              activation_func[k],learning_rate[l])
                                
                                Result_SIN_HETRO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 + 1
                        }
                        
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HETRO_CAD,"result_HETRO_CAD_Train_60.xlsx")

#******************************************  SGD   ********************************************#
Result_SGD_HETRO_LIST <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
        result_HETRO_SGD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                       "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
        
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
                                result_HETRO_SGD[count2,] <-c(predictor_order[i],neurons[j],
                                                              result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                                              activation_func[k],learning_rate[l])
                                
                                Result_SGD_HETRO_LIST[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                                       activation_func[k], result_usd_PO3[[j]])
                                count <- count +1
                                count2 <- count2 + 1
                        }
                        
                }
        }
        
}

# Writing result to xlsx file
write.xlsx(result_HETRO_SGD,"result_HETRO_SGD_Train_60.xlsx")

####################################################################################################################################################


# Train 70%
train_per <- 0.7
data_set <- data_spliting(url,train_per)



train_dataset <- list()                                 
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

#************************************* USD **************************************************#      

Result_USD_HOMO_LIST_70 <- list()
count <- 1
count2 <- 1
### Changes in Neurons and Learning Functins and Learning Rate

for (i in 1:length(predictor_order)) {
  result_HOMO_USD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                     neurons = neurons[j], predictor_order[i], 
                                     activation_func[k],learning_rate[l])
        
        result_HOMO_USD[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_USD_HOMO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                              activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
library(xlsx)
write.xlsx(result_HOMO_USD,"result_HOMO_USD_Train_70.xlsx")

#************************************* GBP  **************************************************#

Result_GBP_HOMO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HOMO_GBP <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                     neurons = neurons[j], predictor_order[i], 
                                     activation_func[k],learning_rate[l])
        
        result_HOMO_GBP[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_GBP_HOMO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                              activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_GBP,"result_HOMO_GBP_Train_70.xlsx")

#************************************* EUR  **************************************************#

Result_EUR_HOMO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HOMO_EUR <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_HOMO_EUR[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_EUR_HOMO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                              activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_EUR,"result_HOMO_EUR_Train_70.xlsx")

#****************************************** CHF ***********************************************#

Result_CHF_HOMO_LIST_70 <- list()
count <- 1
count2 <- 1

for (i in 1:length(predictor_order)) {
  result_CHF_EURO <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_CHF_EURO[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_EUR_HOMO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                              activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_CHF,"result_HOMO_CHF_Train_70.xlsx")

#****************************************** AUD ***********************************************#

Result_AUD_HOMO_LIST_70 <- list()
count <- 1
count2 <- 1

for (i in 1:length(predictor_order)) {
  result_HOMO_AUD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_HOMO_AUD[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_AUD_HOMO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                              activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
      # Writing to xlsx file
      #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
      #write.xlsx(result_HOMO_usd_PO3, write)
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_AUD,"result_HOMO_AUD_Train_70.xlsx")


#****************************************** CAD ***********************************************#

Result_CAD_HOMO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HOMO_CAD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_HOMO_CAD[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_CAD_HOMO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                              activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
      # Writing to xlsx file
      #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
      #write.xlsx(result_HOMO_usd_PO3, write)
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_CAD,"result_HOMO_CAD_Train_70.xlsx")

#******************************************  SGD   ********************************************#

Result_SGD_HOMO_LIST_70 <- list()
count <- 1
count2 <- 1

for (i in 1:length(predictor_order)) {
  result_HOMO_SGD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_HOMO_SGD[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_SGD_HOMO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                              activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_SGD,"result_HOMO_SGD_Train_70.xlsx")

#************************************************************ HETROGENEOUS *************************************************************************#

source("HETRO.R")

#************************************* USD *****************************************************#   
Result_USD_HETRO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_USD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_USD[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_USD_HETRO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                               activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_USD,"result_HETRO_USD_Train_70.xlsx")

#************************************* GBP ****************************************************# 
Result_GBP_HETRO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_GBP <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_GBP[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_GBP_HETRO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                               activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}


# Writing result to xlsx file
write.xlsx(result_HETRO_GBP,"result_HETRO_GBP_Train_70.xlsx")

#************************************* EUR ***************************************************#
Result_EUR_HETRO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_EUR <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_EUR[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_EUR_HETRO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                               activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_EUR,"result_HETRO_EUR_Train_70.xlsx")

#****************************************** CHF ***********************************************#
Result_CHF_HETRO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_CHF <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_CHF[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_CHF_HETRO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                               activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_CHF,"result_HETRO_CHF_Train_70.xlsx")

#****************************************** AUD  **********************************************#
Result_AUD_HETRO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_AUD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_AUD[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_AUD_HETRO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                               activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_AUD,"result_HETRO_AUD_Train_70.xlsx")

#****************************************** CAD **********************************************#
Result_CAD_HETRO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_CAD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_CAD[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_CAD[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_CAD_HETRO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                               activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_CAD,"result_HETRO_CAD_Train_70.xlsx")

#******************************************  SGD   ********************************************#
Result_SGD_HETRO_LIST_70 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_SGD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        result_HETRO_SGD[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_SGD_HETRO_LIST_70[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                               activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_SGD,"result_HETRO_SGD_Train_70.xlsx")


#################################################################################################################################################

# Train 80%
train_per <- 0.8
data_set <- data_spliting(url,train_per)



train_dataset <- list()                                 
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

#************************************* USD **************************************************#      

Result_USD_HOMO_LIST_80 <- list()
count <- 1
count2 <- 1
### Changes in Neurons and Learning Functins and Learning Rate

for (i in 1:length(predictor_order)) {
  result_HOMO_USD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                     neurons = neurons[j], predictor_order[i], 
                                     activation_func[k],learning_rate[l])
        
        result_HOMO_USD[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_USD_HOMO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                 activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
library(xlsx)
write.xlsx(result_HOMO_USD,"result_HOMO_USD_Train_80.xlsx")

#************************************* GBP  **************************************************#

Result_GBP_HOMO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HOMO_GBP <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
                                     neurons = neurons[j], predictor_order[i], 
                                     activation_func[k],learning_rate[l])
        
        result_HOMO_GBP[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_GBP_HOMO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                 activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_GBP,"result_HOMO_GBP_Train_80.xlsx")

#************************************* EUR  **************************************************#

Result_EUR_HOMO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HOMO_EUR <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_HOMO_EUR[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_EUR_HOMO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                 activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_EUR,"result_HOMO_EUR_Train_80.xlsx")

#****************************************** CHF ***********************************************#

Result_CHF_HOMO_LIST_80 <- list()
count <- 1
count2 <- 1

for (i in 1:length(predictor_order)) {
  result_CHF_EURO <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_CHF_EURO[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_EUR_HOMO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                 activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_CHF,"result_HOMO_CHF_Train_80.xlsx")

#****************************************** AUD ***********************************************#

Result_AUD_HOMO_LIST_80 <- list()
count <- 1
count2 <- 1

for (i in 1:length(predictor_order)) {
  result_HOMO_AUD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_HOMO_AUD[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_AUD_HOMO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                 activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
      # Writing to xlsx file
      #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
      #write.xlsx(result_HOMO_usd_PO3, write)
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_AUD,"result_HOMO_AUD_Train_80.xlsx")


#****************************************** CAD ***********************************************#

Result_CAD_HOMO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HOMO_CAD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_HOMO_CAD[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_CAD_HOMO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                 activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
      # Writing to xlsx file
      #write <- paste0("result_HOMO_usd_PO_",i,"_LF_",activation_func[k],"_LR_",learning_rate[l],".xlsx")
      #write.xlsx(result_HOMO_usd_PO3, write)
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_CAD,"result_HOMO_CAD_Train_80.xlsx")

#******************************************  SGD   ********************************************#

Result_SGD_HOMO_LIST_80 <- list()
count <- 1
count2 <- 1

for (i in 1:length(predictor_order)) {
  result_HOMO_SGD <- data.frame("Predictor_Order"=numeric(),"Neurons"=numeric(),"RMSE"=numeric(),
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
        result_HOMO_SGD[count2,] <-c(predictor_order[i],neurons[j],
                                     result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                     activation_func[k],learning_rate[l])
        
        Result_SGD_HOMO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                 activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 +1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HOMO_SGD,"result_HOMO_SGD_Train_80.xlsx")

#************************************************************ HETROGENEOUS *************************************************************************#

source("HETRO.R")

#************************************* USD *****************************************************#   
Result_USD_HETRO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_USD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_USD[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_USD_HETRO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                  activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_USD,"result_HETRO_USD_Train_80.xlsx")

#************************************* GBP ****************************************************# 
Result_GBP_HETRO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_GBP <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_GBP[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_GBP_HETRO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                  activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}


# Writing result to xlsx file
write.xlsx(result_HETRO_GBP,"result_HETRO_GBP_Train_80.xlsx")

#************************************* EUR ***************************************************#
Result_EUR_HETRO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_EUR <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_EUR[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_EUR_HETRO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                  activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_EUR,"result_HETRO_EUR_Train_80.xlsx")

#****************************************** CHF ***********************************************#
Result_CHF_HETRO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_CHF <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_CHF[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_CHF_HETRO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                  activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_CHF,"result_HETRO_CHF_Train_80.xlsx")

#****************************************** AUD  **********************************************#
Result_AUD_HETRO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_AUD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_PO3[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_AUD[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_AUD_HETRO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                  activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_AUD,"result_HETRO_AUD_Train_80.xlsx")

#****************************************** CAD **********************************************#
Result_CAD_HETRO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_CAD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        
        result_usd_CAD[[j]] <-  HETRO(train_dataset_PO3,test_dataset_PO3, usd_non_normalize_PO3, 
                                      neurons = neurons[j], predictor_order[i], activation_func[k],learning_rate[l])
        result_HETRO_CAD[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_CAD_HETRO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                  activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_CAD,"result_HETRO_CAD_Train_80.xlsx")

#******************************************  SGD   ********************************************#
Result_SGD_HETRO_LIST_80 <- list()
count <- 1
count2 <- 1
for (i in 1:length(predictor_order)) {
  result_HETRO_SGD <- data.frame("Predictor Order"= numeric(),"Neurons"= numeric(),"RMSE"=numeric(),
                                 "MAE"=numeric(),"Activation Func"= character(),"Learning Rate"=numeric(), stringsAsFactors=F)
  
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
        result_HETRO_SGD[count2,] <-c(predictor_order[i],neurons[j],
                                      result_usd_PO3[[j]][4],result_usd_PO3[[j]][5],
                                      activation_func[k],learning_rate[l])
        
        Result_SGD_HETRO_LIST_80[[count]] <- list(predictor_order[i],neurons[j],learning_rate[l],
                                                  activation_func[k], result_usd_PO3[[j]])
        count <- count +1
        count2 <- count2 + 1
      }
      
    }
  }
  
}

# Writing result to xlsx file
write.xlsx(result_HETRO_SGD,"result_HETRO_SGD_Train_80.xlsx")
