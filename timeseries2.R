install.packages("htmlTable")
library("htmlTable")
htmlTable(stat_tbl)  
stat_tbl
stationarity_test <- stat_tbl[ , !names(stat_tbl) %in% "reading"]
htmlTable(stationarity_test)
