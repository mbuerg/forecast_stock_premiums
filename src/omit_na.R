# Entfernen der NAs
library(data.table)

# Es gibt Features mit mehr als eine Millionen (mehr als 30%) NAs. Auch prem
# hat NAs.

data_premium = fread(path_data_premium_with_na)

summary(data_premium)

# NAs omitten
data_premium_omitted <- na.omit(data_premium)

fwrite(data_premium_omitted, path_data_premium_omitted)

