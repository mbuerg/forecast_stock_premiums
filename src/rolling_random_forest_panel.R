library(docstring)

rolling_window_rf_walk_forward_panel <- function(data
                                              #, window_size
                                              , response
                                              #, mtry_split_amount
                                              #, trees = 500
                                              #, s = 0.632
){
  #' Rolling Window Random Forest with walk-forward validation (paneldata)
  #' 
  #' @description This function rolls over panel data and fits a 
  #' random forest (RF) on the window. The data is assumed to have a year feature.
  #' The window will roll over the years. This means the window will contain
  #' all data in the year t, then roll further to year t+1 and so on.
  #' 
  #' The very next year serves to test the fit of the window by calculating 
  #' the Mean Squared Forecast Error(MSFE) within the next year. This MSFE is
  #' saved and the window rolls to the year that was just used for testing and so
  #' on.
  #' 
  #' For example this function can be used to train a RF on rolling window with
  #' the data being yearly stock premiums for several stocks per year. For Fore
  #' casting single stocks see rolling_random_forest_ts function.
  #' 
  #'  
  #' @param data dataframe with response and all features
  #' @param window_size yearly size of the rolling window
  #' @param response is the name of the dependent variable in the data
  #' @param mtry_split_amount specifies the mtry parameter that gives the amount
  #' of features used for each split in each tree for the RF
  #' @param trees number of trees in the RF
  #' @param s sample size of bootstrap
  #' 
  #' @usage rolling_window_rf_walk_forward_panel(data, response)
  #' 
  #' @return Returns every error of the individual windows, 
  #' the msfe of the entire model and the last ranger fit
  #' 
  #' @details In order to use this function properly the response variable y has
  #' to be shifted according to the forecast horizon you are interested in.
  #' eg if you are interested in forecasting 3 steps ahead you have to make
  #' sure your data can be used to estimate the model 
  #' y_{t+3} = F(x_{t}) + epsilon_{t+3} with x_{t} being features at time t.
  #' Omit the observations that have NA in the response due to the shifting.
  #' 
  #' @examples
  #' rolling_window_rf_walk_forward_panel(premiums_data, "prem")
  
  
  # rename response variable to 'response' to make function flexible
  # msfe_for_windows are the msfe that is calculated for every window
  # There might be gaps in the data such that eg there is data for 2000 and 2002
  # but not for 2001. So the index for year cant be incremented with just 1
  # Thus years will be ordered and incremented by the next year in the coming
  # for loop
  colnames(data)[which(names(data) == response)] <- "response"
  msfe_for_windows <- c()
  years_ordered <- unique(data["YEAR"])[order(unname(unlist(unique(data["YEAR"])))), ]
  
  i = 1
  # ignore the very last year since this is the last part of the test set and
  # will not be used for training
  for(y in head(years_ordered, -1)){
    
    window <- data[data["YEAR"] == y, ]
    next_year <- years_ordered[(i+1)]
    next_window <- data[data["YEAR"] == next_year,]
    
    rf_fit <- ranger::ranger(response ~.
                             , data=window
                             #, num.trees=trees
                             #, sample.fraction=s
                             #, mtry=mtry_split_amount
                             #, seed=i
    )
    
    prediction <- predict(rf_fit ,
                          data=next_window)
    
    residuals <- next_window["response"] - prediction$predictions
    
    msfe_for_window <- mean(residuals$response^2)
    
    msfe_for_windows <- c(msfe_for_windows, msfe_for_window)
    i = i+1
  }
  
  
  msfe_model <- mean(msfe_for_windows^2)
  
  return(list(msfe_for_windows, msfe_model, rf_fit))
}
