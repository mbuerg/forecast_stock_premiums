library(data.table)
library(ranger)
#library(roxygen2)
library(docstring)
#library(rstudioapi)

#premiums <- fread(path_data_premium_omitted)


rolling_window_rf_walk_forward <- function(data
                                           , window_size){
  #' Rolling Window Random Forest with walk-forward validation
  #' 
  #' @description This function rolls over time series data and fits a 
  #' random forest on the window. After every window the fit is tested on the
  #' very next observation. The window continues to roll until there is no more
  #' test data left.
  #' 
  #' After this process was finished the MSFE is calculated that is the 
  #' MSE of the errors calculated on the very next observations after every
  #' window.
  #' 
  #' @param data dataframe with response and all features
  #' @param window_size size of the rolling window
  #' 
  #' @usage rolling_window_rf_walk_forward(data, window_size)
  #' 
  #' @return Returns every error of the individual test errors, the msfe,
  #' the last ranger fit and all predictions made on the test data
  #' 
  #' @details In order to use this function properly the response variable y has
  #' to be shifted according to the forecast horizon you are interested in.
  #' eg if you are interested in forecasting 3 steps ahead you have to make
  #' sure your data can be used to estimate the model 
  #' y_{t+3} = F(x_{t}) + epsilon_{t+3} with x_{t} being features at time t
  #' 
  #' @examples
  #' rolling_window_rf_walk_forward(premium_data, 10000) creates a 
  #' rolling window of size 10000
  
  
  #set.seed(123)
  
  error <- c()
  predictions <- c()
  
  for(i in 1:(nrow(data) - window_size)){
    
    #set.seed(i)
    
    window <- data[i:(i+window_size)-1, ]
    
    rf_fit <- ranger(prem ~.
                     , data=window
                     #, num.trees=200
                     #, replace=FALSE
                     #, sample.fraction=0.632
                     #, mtry=mtry_split_amount
                     #, seed=i
                     )
    
    prediction <- predict(rf_fit ,
                          data=data[(i+window_size), ])
    
    residual <- data[(i+window_size), "prem"] - prediction$predictions
    
    error <- c(error, residual)
    predictions <- c(predictions, prediction$predictions)
    
  }
  msfe <- mean(error^2)
  
  return(list(error, msfe, rf_fit, predictions))
}

#docstring(rolling_window_rf_walk_forward)
