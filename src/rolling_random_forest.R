# In diesem Skript wird eine Funktion gebaut, die mit einem rolling window
# immer wieder einen Random Forest fittet und die OOBs notiert.
# Die Gesamtforecastperformance ist dann der MSE aller Fehler
# Es wird das In-Sample vs Out-of-Sample Prinzip verwendet, sodass eine 
# k fold cross validation nicht nötig ist.

library(data.table)
library(ranger)

premiums <- fread(path_data_premium_omitted)

# Festlegen der window size (70/30 split?)
#mal gucken, wie die Literatur das macht, also medeiros paper und so, wie die 
#trennen, ob die 70/30 nehmen?


# Definiere Funktion mit rolling window und Anwendung eines Random Forest
# speichere den error ab, bevor das Fenster weiterrollt

rolling_random_forest <- function(data, window_size){
  # data ist das sample der Zeitreihen
  # window_size ist die Länge des Fensters
  # gebe Fehler, mse und den letzten random forest aus. Den letzten Random Forest
  # kann man dann zur Vorhersage verwenden
  
  error = c() # Hier drin sollen forecast errors gesammelt werden
  
  # Anzahl der windows ist T - window_size + 1
  amount_of_windows <- data[, .N] - window_size + 1
  
  for(i in 1:amount_of_windows){
    # lasse windows laufen bis zum vorletzten und berichte die errors
    # im letzten window fitte lediglich einen random forest
    if(i < amount_of_windows){
      window = data[i:((window_size+i)-1)]
      random_forest_fit <- ranger(prem ~.,
                                  data = window)
      prediction <- predict(random_forest_fit,
                            data = data[window_size+i, 2:4])
      rf_error <- data[window_size+i, prem] - prediction$predictions
      error <- append(error, rf_error) 
      
    } else{
      last_window <- data_length - window_size + 1
      last_random_fit <- ranger(prem ~.,
                                data = data[last_window:.N])
    }
  }
  mse <- mean(error^2)
  return(list(error, mse, last_random_fit))
}


# danach zum predicten letzten RF nehmen und predict(letzter rf, letzte Zeile)
# nicht nur Punktprediction machen, sondern auch density forecast!

test_response <- rnorm(100, 3, 1.3)
test_feature_1 <- rnorm(100, 4, 2.1)
test_feature_2 <- sample(1:20, 100, replace=TRUE)
test_feature_3 <- rpois(100, 1.3)

test_df <- data.frame(round(test_response, 2), 
                      round(test_feature_1, 2),
                      test_feature_2,
                      test_feature_3)
names(test_df) <- c("prem", "feature_1",
                    "feature_2",
                    "feature_3")
setDT(test_df)
a <- rolling_random_forest(data=test_df, window_size=5)

a[[1]]

test_rf <- ranger(prem ~., data = test_df)
test_pred <- predict(test_rf, data = test_df)
test_pred$predictions
