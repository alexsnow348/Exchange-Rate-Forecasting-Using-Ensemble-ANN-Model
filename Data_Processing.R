
## Read  USD dataset and process the dataset
#Training variables
alldata = tbl_df(read.csv("alldata.csv"))
usd = filter(alldata, currency=="U.S. Dollar")
newdata = tbl_df(usd)
usd_df = select(newdata, Date, Value)
usd_non_normalize = select(usd_df, Value)
usd_value = as.tbl(normalized(usd_non_normalize))
names(usd_value) = "USD"
result =  createTimeSlices(usd_value$USD, 3, 1, fixedWindow = T)
train_data = training_data(result, usd_value)
vector_train = as.vector(train_data$USD,"any")
matrix_train <- matrix(vector_train, nrow = 3 , ncol = length(vector_train)/3)


#Predicted Date
Date = createTimeSlices(usd$Date, 3, 1, fixedWindow = T)
test_date = testing_data(Date,usd)


#Class Variable
test_data =  testing_data(result,usd_value)
actual_date_and_value = select(usd, Date, Value)

#input_train 60 % && output_train 60 %
training_input = matrix_train[1:3, 1:701]
training_output = test_data[1:701,]

new_input = t(training_input)
train_input = as.data.frame(new_input)
train_dataset <- cbind(train_input, training_output)
names(train_dataset) <- c("firstDay","secondDay", "thirdDay", "oneDayAhead")

#input_test 40% && output_test 40%
testing_input = matrix_train[1:3, 702:1168]
testing_output = test_data[702:1168,]

new_input = t(testing_input)
test_input = as.data.frame(new_input)
test_dataset = cbind(test_input, testing_output)
names(test_dataset) <- c("firstDay","secondDay", "thirdDay", "oneDayAhead")



