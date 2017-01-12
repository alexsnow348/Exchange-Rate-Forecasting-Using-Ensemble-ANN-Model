
# Loading required funtions and pre-processing the data set
data_spliting <- function(){
        
        source("requried_functions_N_libraries.R")
        source("Data_Processing.R")
        
        # Data Source
        
        url <- "/home/wut/Desktop/Link to Data/FYP Program/Raw Data/alldata.csv"
        
        # Select Predictor Order (Range: 3 <-> 10 )
        predictor_order <- seq(3,10,1)
        
        ## Currency : 1. "U.S. Dollar" / 2.  "Australian Dollar"/ 3. "Canadian Dollar"/ 4. "Euro"
        ##            5. "Pound Sterling"/ 6, "Singapore Dollar"/7. "Swiss Franc"
        currency <- c("U.S. Dollar", "Australian Dollar", "Canadian Dollar", "Euro","Pound Sterling", "Singapore Dollar","Swiss Franc")
        
        # To hold the dataframe for each prediction_order
        dataset_list <- list()
        
        for (i in 1:7){
                if (i == 1) {
                        data_frame_3 <- Data_Processing(url,predictor_order[1],currency[1])
                        data_frame_4 <- Data_Processing(url,predictor_order[2],currency[1])
                        data_frame_5 <- Data_Processing(url,predictor_order[3],currency[1])
                        data_frame_6 <- Data_Processing(url,predictor_order[4],currency[1])
                        data_frame_7 <- Data_Processing(url,predictor_order[5],currency[1])
                        data_frame_8 <- Data_Processing(url,predictor_order[6],currency[1])
                        data_frame_9 <- Data_Processing(url,predictor_order[7],currency[1])
                        data_frame_10 <- Data_Processing(url,predictor_order[8],currency[1])
                        dataset_list_usd  <- c(data_frame_3,data_frame_4,data_frame_5,data_frame_6,data_frame_7,data_frame_8,data_frame_9,data_frame_10)
                        dataset_list[[i]] <- dataset_list_usd
                }
                
                if (i == 2) {
                        data_frame_3 <- Data_Processing(url,predictor_order[1],currency[2])
                        data_frame_4 <- Data_Processing(url,predictor_order[2],currency[2])
                        data_frame_5 <- Data_Processing(url,predictor_order[3],currency[2])
                        data_frame_6 <- Data_Processing(url,predictor_order[4],currency[2])
                        data_frame_7 <- Data_Processing(url,predictor_order[5],currency[2])
                        data_frame_8 <- Data_Processing(url,predictor_order[6],currency[2])
                        data_frame_9 <- Data_Processing(url,predictor_order[7],currency[2])
                        data_frame_10 <- Data_Processing(url,predictor_order[8],currency[2])
                        dataset_list_aus  <- c(data_frame_3,data_frame_4,data_frame_5,data_frame_6,data_frame_7,data_frame_8,data_frame_9,data_frame_10)
                        dataset_list[[i]] <- dataset_list_aus
                }
                
                if (i == 3) {
                        data_frame_3 <- Data_Processing(url,predictor_order[1],currency[3])
                        data_frame_4 <- Data_Processing(url,predictor_order[2],currency[3])
                        data_frame_5 <- Data_Processing(url,predictor_order[3],currency[3])
                        data_frame_6 <- Data_Processing(url,predictor_order[4],currency[3])
                        data_frame_7 <- Data_Processing(url,predictor_order[5],currency[3])
                        data_frame_8 <- Data_Processing(url,predictor_order[6],currency[3])
                        data_frame_9 <- Data_Processing(url,predictor_order[7],currency[3])
                        data_frame_10 <- Data_Processing(url,predictor_order[8],currency[3])
                        dataset_list_can  <- c(data_frame_3,data_frame_4,data_frame_5,data_frame_6,data_frame_7,data_frame_8,data_frame_9,data_frame_10)
                        dataset_list[[i]] <- dataset_list_can
                }
                if (i == 4) {
                        data_frame_3 <- Data_Processing(url,predictor_order[1],currency[4])
                        data_frame_4 <- Data_Processing(url,predictor_order[2],currency[4])
                        data_frame_5 <- Data_Processing(url,predictor_order[3],currency[4])
                        data_frame_6 <- Data_Processing(url,predictor_order[4],currency[4])
                        data_frame_7 <- Data_Processing(url,predictor_order[5],currency[4])
                        data_frame_8 <- Data_Processing(url,predictor_order[6],currency[4])
                        data_frame_9 <- Data_Processing(url,predictor_order[7],currency[4])
                        data_frame_10 <- Data_Processing(url,predictor_order[8],currency[4])
                        dataset_list_euro  <- c(data_frame_3,data_frame_4,data_frame_5,data_frame_6,data_frame_7,data_frame_8,data_frame_9,data_frame_10)
                        dataset_list[[i]] <- dataset_list_euro
                }
                if (i == 5) {
                        data_frame_3 <- Data_Processing(url,predictor_order[1],currency[5])
                        data_frame_4 <- Data_Processing(url,predictor_order[2],currency[5])
                        data_frame_5 <- Data_Processing(url,predictor_order[3],currency[5])
                        data_frame_6 <- Data_Processing(url,predictor_order[4],currency[5])
                        data_frame_7 <- Data_Processing(url,predictor_order[5],currency[5])
                        data_frame_8 <- Data_Processing(url,predictor_order[6],currency[5])
                        data_frame_9 <- Data_Processing(url,predictor_order[7],currency[5])
                        data_frame_10 <- Data_Processing(url,predictor_order[8],currency[5])
                        dataset_list_pou  <- c(data_frame_3,data_frame_4,data_frame_5,data_frame_6,data_frame_7,data_frame_8,data_frame_9,data_frame_10)
                        dataset_list[[i]] <- dataset_list_pou
                }
                if (i == 6) {
                        data_frame_3 <- Data_Processing(url,predictor_order[1],currency[6])
                        data_frame_4 <- Data_Processing(url,predictor_order[2],currency[6])
                        data_frame_5 <- Data_Processing(url,predictor_order[3],currency[6])
                        data_frame_6 <- Data_Processing(url,predictor_order[4],currency[6])
                        data_frame_7 <- Data_Processing(url,predictor_order[5],currency[6])
                        data_frame_8 <- Data_Processing(url,predictor_order[6],currency[6])
                        data_frame_9 <- Data_Processing(url,predictor_order[7],currency[6])
                        data_frame_10 <- Data_Processing(url,predictor_order[8],currency[6])
                        dataset_list_sin  <- c(data_frame_3,data_frame_4,data_frame_5,data_frame_6,data_frame_7,data_frame_8,data_frame_9,data_frame_10)
                        dataset_list[[i]] <- dataset_list_sin
                }
                if (i == 7) {
                        data_frame_3 <- Data_Processing(url,predictor_order[1],currency[7])
                        data_frame_4 <- Data_Processing(url,predictor_order[2],currency[7])
                        data_frame_5 <- Data_Processing(url,predictor_order[3],currency[7])
                        data_frame_6 <- Data_Processing(url,predictor_order[4],currency[7])
                        data_frame_7 <- Data_Processing(url,predictor_order[5],currency[7])
                        data_frame_8 <- Data_Processing(url,predictor_order[6],currency[7])
                        data_frame_9 <- Data_Processing(url,predictor_order[7],currency[7])
                        data_frame_10 <- Data_Processing(url,predictor_order[8],currency[7])
                        dataset_list_swi  <- c(data_frame_3,data_frame_4,data_frame_5,data_frame_6,data_frame_7,data_frame_8,data_frame_9,data_frame_10)
                        dataset_list[[i]] <- dataset_list_swi
                }
                
                return(dataset_list)
        }
}








