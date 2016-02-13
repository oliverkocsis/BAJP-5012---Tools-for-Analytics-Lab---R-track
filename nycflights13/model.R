library(data.table)
library(nycflights13)

rm(flights)
flights <- merge(flights, airports[,1:2], by.x = "origin", by.y = "faa")
flights <- merge(flights, airports[,1:2,], by.x = "dest", by.y = "faa")
flights <- merge(flights, airlines[,1:2,], by = "carrier")
setDT(flights)
colnames(flights) <- c(colnames(flights)[1:16], "origin.name", "dest.name", "carrier.name")
flights[, date := as.Date(paste(flights$year, flights$month, flights$day, sep = "-"))]