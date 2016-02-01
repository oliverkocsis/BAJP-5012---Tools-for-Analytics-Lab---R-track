# ggplot2 exercises
# number of carburetors
# horsepower
# barplot of number of carburetors per transmission
# boxplot of horsepower by the number of carburetors
# horsepower and weight by the number of carburetors
# horsepower and weight by the number of carburetors with a trend line

library(dplyr)
library(ggplot2)

str(mtcars)
# number of carburetors
mtcars %>% ggplot(aes(x = carb)) + geom_bar() + theme_minimal()
# horsepower
mtcars %>% ggplot(aes(x = hp)) + geom_bar(binwidth = 50) + theme_minimal()
# barplot of number of carburetors per transmission
mtcars$am <- factor(mtcars$am)
mtcars %>% ggplot(aes(x = carb)) + geom_bar() + theme_minimal() + facet_grid(~am)
mtcars %>% ggplot(aes(x = carb, fill = am)) + geom_bar() + theme_minimal()
mtcars %>% ggplot(aes(x = carb, fill = am)) + geom_bar(position = "fill") + theme_minimal()
# boxplot of horsepower by the number of carburetors
mtcars$carb <- factor(mtcars$carb)
mtcars %>% ggplot(aes(x = carb, y = hp)) + geom_boxplot() + theme_minimal()
# horsepower and weight by the number of carburetors
mtcars %>% ggplot(aes(x = hp, y = wt)) + geom_point(aes(color = carb), size = 3) + theme_minimal()
# horsepower and weight by the number of carburetors with a trend line
mtcars %>% ggplot(aes(x = hp, y = wt)) + geom_point(aes(color = carb), size = 3) + geom_smooth(method = "lm") + theme_minimal()

# FTSE Index from HTML table
library(XML)
ftse <- readHTMLTable(readLines("https://en.wikipedia.org/wiki/FTSE_100_Index"), which = 2, header = TRUE, stringsAsFactors = FALSE)
colnames(ftse)[4] <- "Market"
ftse$Market <- as.numeric(ftse$Market)
ftse$Employees <- as.numeric(gsub(",", "", ftse$Employees))
min(ftse$Employees)
summary(ftse)
aggregate(Employees ~ Sector, FUN = mean, data = ftse )
subset(ftse, Employees < 1000 & Market < 5)

# data.table
library(data.table)
library(hflights)
dt <- data.table(hflights)
subset(dt, Dest == "LAX")
dt[Dest == "LAX", list(Dest, DepTime, ArrTime)]
dt[,sum(Cancelled)]
dt[,.N, by = Dest]
dt[,list(cancelled = sum(Cancelled)), by = Dest]
dt[,.(Cancelled = sum(Cancelled), .N, sum(ArrDelay, na.rm = TRUE)), by = Dest] # "." characters means list
dt[ArrDelay >= 0,.(Cancelled = sum(Cancelled), .N, sum(ArrDelay, na.rm = TRUE)), by = Dest]
dta <- dt[ArrDelay >= 0][,.(Cancelled = sum(Cancelled), .N, ArrDelay = sum(ArrDelay, na.rm = TRUE)), by = Dest] # Chaining
dta[order(N)]
setorder(dta, N)
dta
setorder(dta, Dest, -N)
dtb <- dt[,list(cancelled = sum(Cancelled)), by = .(Origin, Dest)]
dtb
setorder(dtb, Origin, Dest)
dtb

# nycflights13
library(nycflights13)
str(flights)
flights <- data.table(flights)
flights[, date := as.Date(paste(year,month,day, sep = "-"))] # ":=" new column
flights$weekday <- weekdays(flights$date) # do not use "<-" as it copies object within memory (in case of few GBs data it does matter)
flights[, weekday := weekdays(date)]
Sys.getlocale()
str(airports)
flights <- merge(nycflights13::flights,nycflights13::airports, by.x = "dest", by.y = "faa")
str(flights)
flights <- merge(nycflights13::flights,nycflights13::airports, by.x = "dest", by.y = "faa", all.x = TRUE)
str(flights)

# Further data.table exercises on the nycflights13 dataset to practice for the !!!exam!!!:
library(data.table)
library(nycflights13)
library(ggplot2)
# count the number of flights to LAX
str(flights)
str(airports)
flights <- data.table(nycflights13::flights)
airports <- data.table(nycflights13::airports)
flights[, .N]
# count the number of flights to LAX
flights[dest == "LAX", .N]
# count the number of flights to LAX from JFK
flights[origin == "JFK" & dest == "LAX", .N]
# compute the average delay (in minutes) for flights from JFK to LAX
flights[origin == "JFK" & dest == "LAX", .(avg_arr_delay = mean(arr_delay, na.rm = TRUE))]
# which destination has the lowest average delay from JFK?
average_delays <- flights[origin == "JFK", .(avg_arr_delay = mean(arr_delay, na.rm = TRUE)), by = .(dest)]
average_delays[order(avg_arr_delay)][1]
average_delays[which.min(avg_arr_delay)]
# plot the average delay to all destinations from JFK
ggplot(average_delays, aes(x = dest, y = avg_arr_delay)) + geom_point()
ggplot(average_delays, aes(x = dest, y = avg_arr_delay)) + geom_bar(stat = "identity")
setorder(average_delays, avg_arr_delay)
average_delays[, dest_factor := factor(dest, levels = average_delays$dest)]
plot <- ggplot(average_delays, aes(x = dest_factor, y = avg_arr_delay))
plot + geom_bar(stat = "identity")
plot + geom_bar(stat = "identity") + coord_flip() # y must be numeric
plot + geom_bar(stat = "identity") + ggtitle("Average delays of destinations from JFK") # y must be numeric
# plot the distribution of all flight delays to all destinations from JFK
ggplot(flights[origin == "JFK"], aes(x = dest, y = arr_delay)) + geom_point()

library(jsonlite)
distance <- function(origin, dest) {
  json <- fromJSON(paste("https://maps.googleapis.com/maps/api/distancematrix/json?origins=", origin, "&destinations=" ,dest, "&key=AIzaSyDDQXzayXlGiNCLgRHyX18L2MqZ2fudXwo", sep = ""))
}

distance("JFK", "LAX")

# compute a new variable in flights showing the week of day
# plot the number of flights per weekday
# create a heatmap on the number of flights per weekday and hour of the day (see geom_tile)
# merge the airports dataset to flights on the FAA airport code
# order the weather dataset by year, month, day and hour
# plot the average temperature at noon in EWR for each month based on the weather dataset
# aggregate the weather dataset and store as daily_temperatures to show the daily average temperatures based on the EWR records
# merge the daily_temperatures dataset to flights on the date
# do the above two steps on daily + hourly temperature averages
