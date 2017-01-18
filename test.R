# sample run

train_dataset <- list()                                 
test_dataset <- list() 
predictor_order <- 3
test_date <- list()
usd_non_normalize <- list()
result_usd <- list()
actual_usd <- list()
learning_rate <- 0.1
activation_func <- c("logistic")

# Train 60%
train_per <- 0.6
data_set <- data_spliting(url,train_per)

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

#************************************************************ HETROGENEOUS *************************************************************************#

source("HETRO.R")


train_dataset <- list()                                 
test_dataset <- list() 
predictor_order <- 3
test_date <- list()
usd_non_normalize <- list()
result_usd <- list()
actual_usd <- list()
learning_rate <- 0.1
activation_func <- c("logistic")


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

#############################################################################################################

# sample run

train_dataset <- list()                                 
test_dataset <- list() 
predictor_order <- 3
test_date <- list()
usd_non_normalize <- list()
result_usd <- list()
actual_usd <- list()
learning_rate <- 0.1
activation_func <- c("logistic")

# Train 60%
train_per <- 0.7
data_set <- data_spliting(url,train_per)


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


#######################################################################################################################

# sample run

train_dataset <- list()                                 
test_dataset <- list() 
predictor_order <- 3
test_date <- list()
usd_non_normalize <- list()
result_usd <- list()
actual_usd <- list()
learning_rate <- 0.1
activation_func <- c("logistic")

# Train 60%
train_per <- 0.8
data_set <- data_spliting(url,train_per)

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



