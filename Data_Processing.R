## Read  USD dataset and process the dataset

## Function Parameters
## Datasource_url: "/home/wut/Desktop/Link to Data/FYP Program/Raw Data/alldata.csv"
## Predictor order : 1-10
## Currency : "USD"/"GBP"/"EUR"/"CHF"/"AUD"/"CAD"/"SGD"


## Return Outcomes
## Date for predicted day, Test Dataset, Train Dataset, Valadition Data Set


#Training variables
Data_Processing <- function(url, predictor_order, ex_currency,train_per){ 
        require("dplyr")
        alldata = tbl_df(read.csv(url))
        if(ex_currency=="USD"){usd_df = select(alldata, DATE, USD)}
        if(ex_currency=="GBP"){usd_df = select(alldata, DATE, GBP)}
        if(ex_currency=="EUR"){usd_df = select(alldata, DATE, EUR)}
        if(ex_currency=="CHF"){usd_df = select(alldata, DATE, CHF)}
        if(ex_currency=="AUD"){usd_df = select(alldata, DATE, AUD)}
        if(ex_currency=="CAD"){usd_df = select(alldata, DATE, CAD)}
        if(ex_currency=="SGD"){usd_df = select(alldata, DATE, SGD)}
        usd_non_normalize = usd_df[,2]
        usd_value = tbl_df(normalized(usd_non_normalize))
        names(usd_value) = "USD"
        result =  createTimeSlices(usd_value$USD, predictor_order, 1, fixedWindow = T)
        train_data = training_data(result, usd_value)
        vector_train = as.vector(train_data$USD,"any")
        matrix_train <- matrix(vector_train, nrow = predictor_order, ncol = length(vector_train)/predictor_order)
        
        #Class Variable
        test_data =  testing_data(result,usd_value)
        
        
        #input_train 60 % && output_train 60 %
        train_until = ceiling(length(matrix_train)*train_per/predictor_order)
        training_input = matrix_train[1:predictor_order, 1:train_until]
        train_end = ceiling(length(test_data$USD)*train_per)
        training_output = test_data[1:train_end,]
        
        new_input = t(training_input)
        train_input = as.data.frame(new_input)
        train_dataset <- cbind(train_input, training_output)
        
        
        #input_test 
        test_until <- train_until+1
        test_end <- train_end+1
        matrix_end <- length(matrix_train)/predictor_order
        testing_input = matrix_train[1:predictor_order, test_until: matrix_end]
        testing_output = test_data[test_end:length(test_data$USD),]
        
        new_input = t(testing_input)
        test_input = as.data.frame(new_input)
        test_dataset = cbind(test_input, testing_output)
      
        ## Seperating TEST and VALIDATE dataset
        test_data_end <- ceiling(length(test_dataset$USD)*0.5)
        test_data = test_dataset[1:test_data_end, ]
        validate_start = test_data_end+1
        validate_data = test_dataset[validate_start:length(test_dataset$USD),]
        
        
        #Nameing columns
        if (predictor_order==3){
                names(train_dataset) <- c("firstDay","secondDay", "thirdDay", "oneDayAhead") 
                names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "oneDayAhead")
        }
        
        if(predictor_order==4){
                names(train_dataset) <- c("firstDay","secondDay", "thirdDay","fourthDay", "oneDayAhead") 
                names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "fourthDay","oneDayAhead")
        }
        
        if(predictor_order==5){
                names(train_dataset) <- c("firstDay","secondDay", "thirdDay","fourthDay","FifthDay", "oneDayAhead") 
                names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "fourthDay","Fifthday","oneDayAhead")
        }
        
        if(predictor_order==6){
                names(train_dataset) <- c("firstDay","secondDay", "thirdDay","fourthDay","FifthDay","SixthDay", "oneDayAhead") 
                names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "fourthDay","Fifthday","SixthDay","oneDayAhead")
        }
        
        if(predictor_order==7){
                names(train_dataset) <- c("firstDay","secondDay", "thirdDay","fourthDay","FifthDay","SixthDay","SeventhDay","oneDayAhead") 
                names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "fourthDay","Fifthday","SixthDay","SeventhDay","oneDayAhead")
        }
        
        if(predictor_order==8){
                names(train_dataset) <- c("firstDay","secondDay", "thirdDay","fourthDay","FifthDay","SixthDay","SeventhDay","eighthDay","oneDayAhead") 
                names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "fourthDay","Fifthday","SixthDay","SeventhDay","eighthDay","oneDayAhead")
        }
        if(predictor_order==9){
                names(train_dataset) <- c("firstDay","secondDay", "thirdDay","fourthDay","FifthDay","SixthDay","SeventhDay","eighthDay","ninethDay","oneDayAhead") 
                names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "fourthDay","Fifthday","SixthDay","SeventhDay","eighthDay","ninethDay","oneDayAhead")
        }
        if(predictor_order==10){
                names(train_dataset) <- c("firstDay","secondDay", "thirdDay","fourthDay","FifthDay","SixthDay","SeventhDay","eighthDay","ninethDay","tenthDay","oneDayAhead") 
                names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "fourthDay","Fifthday","SixthDay","SeventhDay","eighthDay","ninethDay","tenthDay","oneDayAhead")
        }
        
        
        
        #Predicted Date
        Date = createTimeSlices(usd_df$DATE, predictor_order, 1, fixedWindow = T)
        date = testing_data(Date,usd_df)
        testing_date = date[test_end:length(usd_df$DATE),]$DATE
        testing_date_list <- list(lapply(testing_date, as.character))
       
        i <- 1
        testing_date <- data_frame("Date"= character())
        for (i in 1:length(testing_date_list[[1]])) {
                testing_date[i,] <- c(testing_date_list[[1]][[i]])
        }
       
        test_data_end <- ceiling(length(test_dataset$firstDay)*0.5)
        test_date = testing_date[1:test_data_end,]
        validate_start = test_data_end+1
        validate_date = testing_date[validate_start:length(testing_date$Date),]
       
        output <- list(train_dataset,test_data,validate_data,test_date,validate_date, usd_non_normalize)
        
        
}