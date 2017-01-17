
# Loading required funtions and pre-processing the data set

data_spliting <- function(url){
        
        source("functions.R")
        source("Data_Processing.R")
        
        # Select Predictor Order (Range: 3 <-> 10 )
        predictor_order <- seq(3,10,1)
        
        ## Currency : 1. "U.S. Dollar" / 2.  "Australian Dollar"/ 3. "Canadian Dollar"/ 4. "Euro"
        ##            5. "Pound Sterling"/ 6, "Singapore Dollar"/7. "Swiss Franc"
        currency <- c("USD","GBP","EUR","CHF","AUD","CAD","SGD")
        
        # To hold the dataframe for each prediction_order
        dataset_list <- list()
        data_frame_usd <- list()
        data_frame_aus <- list()
        data_frame_can <- list()
        data_frame_euro <- list()
        data_frame_pon <- list()
        data_frame_sin <- list()
        data_frame_swi <- list()
        for(i in 1:7){
                
                if (i == 1) {
                        for(j in 1:8){
                                data_frame <- Data_Processing(url,predictor_order[j],currency[1])
                                data_frame_usd[[j]] <- data_frame
                        }
                        
                        dataset_list[[i]]<- data_frame_usd
                }
                
                if (i == 2) {
                        for(j in 1:8){
                                data_frame <- Data_Processing(url,predictor_order[j],currency[2])
                                data_frame_aus[[j]] <- data_frame
                        }
                        
                        dataset_list[[i]]<- data_frame_aus
                }
                
                if (i == 3) {
                        for(j in 1:8){
                                data_frame <- Data_Processing(url,predictor_order[j],currency[3])
                                data_frame_can[[j]] <- data_frame
                        }
                        
                        dataset_list[[i]]<- data_frame_can
                }
                
                if (i == 4) {
                        for(j in 1:8){
                                data_frame <- Data_Processing(url,predictor_order[j],currency[4])
                                data_frame_euro[[j]] <- data_frame
                        }
                        
                        dataset_list[[i]]<- data_frame_euro
                }
                
                if (i == 5) {
                        for(j in 1:8){
                                data_frame <- Data_Processing(url,predictor_order[j],currency[5])
                                data_frame_pon[[j]] <- data_frame
                        }
                        
                        dataset_list[[i]]<- data_frame_pon
                }
                
                if (i == 6) {
                        for(j in 1:8){
                                data_frame <- Data_Processing(url,predictor_order[j],currency[6])
                                data_frame_sin[[j]] <- data_frame
                        }
                        
                        dataset_list[[i]]<- data_frame_sin
                }
                if (i == 7) {
                        for(j in 1:8){
                                data_frame <- Data_Processing(url,predictor_order[j],currency[7])
                                data_frame_swi[[j]] <- data_frame
                        }
                        
                        dataset_list[[i]]<- data_frame_swi
                }
        }
        
        return(dataset_list)
}