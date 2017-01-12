
#***************************************** MAIN *************************************************#

# Loading required funtions and pre-processing the data set
source("data_spliting.R")

# Spliting the data
data_set <- data_spliting()                             # Usage of Data_set
                                                        # data_set[[language_type]][[Predictor_order]][1:4]
data_usd_pre_ord_5 <- data_set[[1]][[3]]                #****Lanuage Type **** #   #**** Predictor_ordr****#
data_usd_pre_ord_4 <- data_set[[1]][[2]]                #   1. USD    5. PON   #   #  1. PO_3   5. PO_7
                                                        #   2. AUS    6. SIN   #   #  2. PO_4   6. PO_8
                                                        #   3. CAN    7, SWI   #   #  3. PO_5   7. PO_9
                                                        #   4. EURO            #   #  4. PO_6   8. PO_10
                                                        
## USD , Predictior Order 4 
train_dataset_4 <- data_usd_pre_ord_4[[1]]
test_dataset_4 <- data_usd_pre_ord_4[[2]]
usd_non_normalize_4 <- data_usd_pre_ord_4[[4]] 
test_date_4 <- data_usd_pre_ord_4[[3]]
actual_4 <- denormalized(test_dataset_4[,5],usd_non_normalize_4)
## HOMOGENEOUS MODEL
source("HOMO.R")

result_4 <-  HOMO(train_dataset_4,test_dataset_4,usd_non_normalize_4,4,0.1)
hist(result_4[[2]])
result_towrite_4<- list(test_date_4,actual_4, result_4[[1]],result_4[[2]])
result_towrite_4 <- as.data.frame(result_towrite_4)
names(result_towrite_4)<- c("Date","Actual_Value","Predicted_Value","Error")

# Writing to xlsx file
library(xlsx)
write.xlsx(result_towrite_4, "reslut_USD_PO4.xlsx")


