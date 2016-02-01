library(h2o)
library(hflights)

# h2o.init()
write.csv(hflights, "hflights.csv", row.names = FALSE)
hex = h2o.importFile("hflights.csv", destination_frame = "hflights")
str(hex)
summary(hex)
hex.data.frame <- as.data.frame(hex)
str(hex.data.frame)
summary(hex.data.frame)
hex[,"FlightNum"] = as.factor(hex[,"FlightNum"])


hsplit = h2o.splitFrame(hex, ratios = 0.75)
htrain = hsplit[1]
htest = hsplit[2]

h2o.randomForest(training_frame = htrain, validation_frame = htest, x = c("Month"))
