data_try_4 <-data_set[[1]][[2]]
train_dataset <- data_try_4[[1]]
test_dataset <- data_try_4[[2]]
test_date <- data_try_4[[3]]
usd_non_normalize<- data_try_4[[4]]
actual_data<- test_dataset[,5]
predictor_order <- 4
neurons <- 3

library("RSNNS")
train_input <- as.matrix(train_dataset[,1:4])
train_output <- as.matrix(train_dataset[,5])
test_input <- as.matrix(test_dataset[,1:4])

model <- ml