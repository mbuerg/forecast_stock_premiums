# In diesem Skript wird eine Funktion gebaut, die mit einem rolling window
# immer wieder einen Random Forest fittet und die forecasting errors notiert.
# Die Gesamtforecastperformance ist dann der MSE aller Fehler

library(data.table)
library(ranger)

premiums <- fread(path_data_premium_omitted)


# Definiere Funktion mit rolling window und Anwendung eines Random Forest
# speichere den error ab, bevor das Fenster weiterrollt

rolling_random_forest <- function(data, window_size, h, response){
  # data ist das sample der Zeitreihen
  # window_size ist die Länge des Fensters
  # h ist der forecast horizon
  # reponse ist die abhängige Variable
  # gebe Fehler, mse und den letzten random forest aus. Den letzten Random Forest
  # kann man dann zur Vorhersage verwenden
  
  # benenne response Variable entsprechend um
  names(data)[names(data) == response] <- "response"
  
  error = c() # Hier drin sollen forecast errors gesammelt werden
  
  # Anzahl der windows ist T - window_size - h + 1
  amount_of_windows <- data[, .N] - window_size - h + 1
  
  for(i in 1:amount_of_windows){
    # lasse windows laufen bis zum letzten und berichte die errors
    
      window = data[i:((window_size+i)-1)]
      
      random_forest_fit <- ranger(response ~.,
                                  data = window)
      
      prediction <- predict(random_forest_fit,
                            data = data[window_size+i+h-1, ])
      
      rf_error <- data[window_size+i+h-1, response] - prediction$predictions
      
      error <- append(error, rf_error) 
  }
  mse <- mean(error^2)
  return(list(error, mse, random_forest_fit))
}
