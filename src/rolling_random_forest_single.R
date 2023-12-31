library(docstring)

rolling_window_rf_walk_forward_single <- function(data
                                           , window_size
                                           , response
                                           , mtry_split_amount
                                           , trees = 500
                                           , s = 0.632
                                           ){
  #' Rolling Window Random Forest with walk-forward validation (time series)
  #' 
  #' @description This function rolls over panel data for a single
  #' object of interest (eg one stock) and fits a random forest (RF) on the window. 
  #' After every window the fit is tested on the very next observation. 
  #' The window continues to roll until there is no more test data left.
  #' 
  #' After this process was finished the MSFE is calculated that is the 
  #' MSE of the errors calculated on the very next observations after every
  #' window.
  #' 
  #' @param data dataframe with response and all features
  #' @param window_size size of the rolling window
  #' @param response is the name of the dependent variable in the data
  #' @param mtry_split_amount specifies the mtry parameter that gives the amount
  #' of features used for each split in each tree for the RF
  #' @param trees number of trees in the RF
  #' @param s sample size of bootstrap
  #' 
  #' @usage rolling_window_rf_walk_forward_ts(data, window_size)
  #' 
  #' @return Returns every error of the individual test errors, the msfe,
  #' the last ranger fit and all predictions made on the test data
  #' 
  #' @details In order to use this function properly the response variable y has
  #' to be shifted according to the forecast horizon you are interested in.
  #' eg if you are interested in forecasting 3 steps ahead you have to make
  #' sure your data can be used to estimate the model 
  #' y_{t+3} = F(x_{t}) + epsilon_{t+3} with x_{t} being features at time t.
  #' Omit the observations that have NA in the response due to the shifting.
  #' 
  #' Furthermore if your data consists of more than one object of interest
  #' eg more than one stock, individual or similar you have to filter the 
  #' data on the object of interest first before you pass the data into 
  #' the function.
  #' 
  #' @examples
  #' rolling_window_rf_walk_forward_ts(premium_data, 10000)
  
  
  
  error <- c()
  predictions <- c()
  colnames(data)[which(names(data) == response)] <- "response"
  
  for(i in 1:(nrow(data) - window_size)){
    
    window <- data[i:(i+window_size-1), ]
    
    rf_fit <- ranger::ranger(response ~.
                     , data=window
                     , num.trees=trees
                     , sample.fraction=s
                     , mtry=mtry_split_amount
                     #, seed=i
                     )
    
    prediction <- predict(rf_fit ,
                          data=data[(i+window_size), ])
    
    residual <- data[(i+window_size), response] - prediction$predictions
    
    error <- c(error, residual)
    predictions <- c(predictions, prediction$predictions)
    
  }
  error <- unname(unlist(error))
  msfe <- mean(error^2)
  
  return(list(error, msfe, rf_fit, predictions))
}