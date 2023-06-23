# In diesem Skript werden die Daten gebaut, die für einen forecast der Aktienprämien
# notwendig sind. Dazu braucht man die Features, die Aktienreturns und die tb3ms 
# (risikoloser Bond). Die resultierenden Daten werden abgespeichert.

library(data.table)

# Data Wrangling

# importiere Daten
data <- fread(path_data_raw)
ret <- fread(path_ret_raw, sep =",")
bond <- fread(path_bond_raw)

# mache Datum zu characters, um sie später in Monate und Jahre zu trennen
data <- data[, DATE:=as.character(DATE)]
data <- data[, DATE:=substring(DATE, 1, 6)]
data <- data[, MONTH:=as.integer(substring(DATE, 5))]
data <- data[, YEAR:=as.integer(substring(DATE, 1, 4))]
data <- data[, DATE:= as.integer(DATE)]

ret <- ret[, DATE:=substring(date, 1, 6)]
ret <- ret[, date:=NULL]

# ret-dt von wide zu long Format machen
ret_long <- melt(ret, 
                 id.vars = c("DATE"), 
                 variable.name = "permno")


ret_long <- ret_long[, permno:=as.integer(substring(permno, 5,))]
ret_long <- ret_long[, DATE:= as.integer(DATE)]

setnames(ret_long, "value", "ret")

# joine data und ret_long
# beachte, dass alle features bereits gelagged sind. Man kann also einfach 
# über das Datum joinen für einen forecast horizon h = 1.

data_ret <- data[ret_long, on=.(DATE, permno), nomatch=NULL]

# tb3ms wranglen
bond <- bond[, DATE:=as.character(DATE)]
bond <- bond[, DATE:=paste(substring(DATE, 1, 4), substring(DATE, 6, 7), sep="")]
bond <- bond[, DATE:=as.integer(DATE)]


# joine data_ret mit bond
data_ret_bond <- bond[data_ret, on=.(DATE)]

# Aktienprämien berechnen und unnötige Features raus
data_premium <- data_ret_bond[, prem:=ret-TB3MS]
data_premium[, TB3MS:=NULL]
data_premium[, ret:=NULL]
data_premium[, DATE:=NULL]

#str(data_premium, list.len=ncol(data_premium))

rm(list = c("bond", "data", "data_ret", "ret", "ret_long", "data_ret_bond"))

#summary(data_premium)

# speichere daten
fwrite(data_premium, path_data_premium_with_na)



