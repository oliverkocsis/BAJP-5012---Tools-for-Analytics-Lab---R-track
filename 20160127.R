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
