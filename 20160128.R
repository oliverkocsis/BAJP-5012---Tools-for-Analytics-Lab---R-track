library(data.table)
library(dplyr)
library(ggplot2)

# data.table exercises with the following dataset:
set.seed(42)
tx <- data.table(
  item   = sample(letters[1:3], 10, replace = TRUE),
  time   = as.POSIXct(as.Date('2016-01-01')) - runif(10) * 36*60^2,
  amount = rpois(10, 25))
prices <- data.table(
  item  = letters[1:3],
  date  = as.Date('2016-01-01') - 1:2,
  price = as.vector(outer(c(100, 200, 300), c(1, 1.2))))
items <- data.table(
  item   = letters[1:3],
  color  = c('red', 'white', 'red'),
  weight = c(2, 4, 2.5))

# filter for transactions with "b" items
tx[item == "b"]
# filter for transactions with less than 25 items
tx[amount < 25]
# filter for transactions with less then 25 "b" items
tx[item == "b" & amount < 25]
# count the number of transactions for each items
tx[, .N, by = item]
# count the number of transactions for each day
tx[, date := as.Date(time)]
tx[, .N, date]
# count the overall number of items sold on each day
tx[, .(amount=sum(amount)), by = date]
# merge option a
merge(tx, items, by = "item")
merge(tx, prices, by = c("item","date"))
# merge option b
setkey(tx, item)
setkey(items, item)
items[tx]
tx[items]
setkey(tx, item, date) # The order matters not the name!
setkey(prices, item, date)
prices[tx]
tx[prices]
tx <- tx[items]
tx <- tx[prices]
# sum of weight per date
tx[,.(weight = sum(weight)), by = date]
tx[,.N, by = color]
tx[,.(amount = sum(amount)), by = color]
tx[,.N, by = .(color, date)]

# reshape
library(reshape2)
ggplot(data=tx[,.N, by = .(color, date)], aes(x = date, y = N)) + geom_bar(stat = "identity", position = "dodge", aes(fill = color))
ft <- tx[,.N, by = .(color, date)]
ft
table(tx$color,tx$date)

ft <- dcast(ft, color ~ date, value.var = "N")
ft
melt(ft, id.vars = "color")

# Height - Weight
df <- read.csv("http://bit.ly/BudapestBI-R-csv")
str(df)
t.test(heightIn ~ sex, data = df)
aov(heightIn ~ sex, data = df)
summary(aov(heightIn ~ sex, data = df))
ggplot(data=df, aes(x = sex, y = heightIn)) + geom_boxplot()
ggplot(data=df, aes(x = sex, y = heightIn)) + geom_violin()

str(df)
setDT(df) # df <- data.table(df)
df[, high := as.character(heightIn > mean(heightIn))]
str(df)
ft <- dcast(df, sex ~ high)
ft
ft[,-1] # should work check git hub 
ft$sex <- NULL # instead of -1
chisq.test(ft)

# bickel 
library(openxlsx)
df <- read.xlsx("bickel.xlsx")
setDT(df)
str(df)
df[,rejected := applicants - admissions]
df[,percent := admissions / applicants]
temp <- df[,.(percent = sum(admissions) / sum(applicants)) , by = .(gender, deparment)]
dcast(temp, gender ~ deparment, value.var = "percent")
ggplot(data=temp, aes(x = deparment, y = percent)) + geom_bar(stat = "identity", position = "dodge", aes(fill = gender))

dp <- df[,.(applicants = sum(applicants), admissions = sum(admissions)), by = deparment]
dp[,percent := admissions / applicants]
setkey(dp, deparment)
gn <- dcast(df, deparment ~ gender, value.var = "applicants")
setDT(gn)
setkey(gn, deparment)
dp <- dp[gn]
dp[, fm := f / m]
dp
# So there is no gender bias