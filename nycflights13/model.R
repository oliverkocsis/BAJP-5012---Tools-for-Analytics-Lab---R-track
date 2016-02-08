library(data.table)

flights <- data.table(flights)
flights[, date := as.Date(paste(flights$year, flights$month, flights$day, sep = "-"))]
