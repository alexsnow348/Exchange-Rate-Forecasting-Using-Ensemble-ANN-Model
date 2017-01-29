# LIBRARY
library(nnet)
library(neuralnet)
library(grid)
library(MASS)
library(caret)
library(NeuralNetTools)
library(dplyr)

### FUNCTIONS

## Data Slicing Function based on predictor_order
data_slice <- function(dataframe, predictor_order, day_ahead = 1){
        result =  createTimeSlices(dataframe, initialWindow = predictor_order,
                                   horizon = day_ahead, fixedWindow = T)
        return(result)      
};

## Inputs Datasets
training_data <- function(slice_info, dataframe) { # slice_info is data_slice object
        bind = data.frame()
        for(i in slice_info$train){
                for(j in i){
                        b = j
                        bind <- rbind(bind,dataframe[b,])
                }
        }  
        return(bind)
};

## Outputs Varitable 
testing_data <- function(slice_info, dataframe) {
        bind <- data.frame()
        for(i in slice_info$test){
                for(j in i){
                        bind <- rbind(bind,dataframe[j,])
                }
        }  
        return(bind)
};

## Normalized the data into 0-1

normalized <- function(x){
        normalized_x <-  (x-min(x))/(max(x)-min(x))
        return(normalized_x)
};

denormalized <- function(x,non_normalize){
        denormalized_x <- x * (max(non_normalize)-min(non_normalize))+min(non_normalize)
};

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
        sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
        mean(abs(error))
}