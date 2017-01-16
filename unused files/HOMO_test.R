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