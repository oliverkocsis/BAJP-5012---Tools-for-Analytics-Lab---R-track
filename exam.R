library(data.table)
library(ggplot2)
## Transform the mtcars dataset to data.table and store as a new object
dt <- data.table(mtcars)
dt[, car := rownames(mtcars)]

## Count the number of cars with less than 4 gears
dt[gear < 4,.N]

## Count the number of cars with more than 4 gears and less than 100 horsepower
dt[gear > 4 & hp < 100,.N]

## What's the average weight of cars with 4 cylinders?
dt[cyl == 4,.(avg_wt = mean(wt))]

## Which car has the best fuel consumption?
dt[which.max(mpg)]

## Plot the distribution of the number of carburetors
ggplot(dt, aes(x = carb)) + geom_bar()

## Plot the distribution of the number of carburetors grouped by gears
ggplot(dt, aes(x = carb)) + geom_bar() + facet_grid( ~ gear)

## Plot the average weight grouped by the number of carburetors
ggplot(dt[,.(avg_wt = mean(wt)), by = carb], aes(x = carb, y = avg_wt)) + geom_bar(stat = "identity") 

## Plot the weight and horsepower of cars
plot <- ggplot(dt, aes(x = wt, y = hp)) + geom_point() 
plot

## Add a linear trend line to the above plot
plot + geom_smooth(method = "lm", se = FALSE)

## Add a 3rd degree polynomial model to the above plot
plot + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE)

## Fit a linear model on the weight of cars to predict fuel consumption
fit <- lm(mpg ~ wt, data = dt)
summary(fit)

## What's the estimated fuel consumption of a car with wt = 5?
predict(fit, newdata = data.frame(wt = c(5)))
ggplot(dt, aes(x = wt, y = mpg)) + geom_point() + geom_smooth(method = "lm", se = FALSE) # in order to validate the result

## Install the ISLR package and use its Auto for the below exercises
# install.packages("ISLR")
library(ISLR)
?Auto
# Origin of car (1. American, 2. European, 3. Japanese)
str(Auto)
## Build and visualize a decision tree to tell if a car was made in America, Europe or Japan
library(rpart)
set.seed(2016)
auto <- Auto
auto$origin <- as.factor(auto$origin) # classification
l <- dim(auto)[1]
i <- sample(1:l, as.integer(l * 0.75))
train <- auto[i, -9]
test <- auto[-i, -9]
ct <- rpart(origin ~ ., data = train)
summary(ct)
plot(ct)
text(ct)
table(test$origin, predict(ct, newdata = test, type = "class")) # Confusion matrix
## Apply k-means or hierarchical clustering on the dataset to split the observations into 3 groups
# k-means
# Not using year, origin, and name
kc <- kmeans(auto[,1:6], centers = 3) 
summary(kc)
table(kc$cluster, auto$origin)
# hierarchical
dm <- dist(auto[,1:6])
hc <- hclust(dm)
plot(hc)
rect.hclust(hc, k = 3, border = 'red')
cn <- cutree(hc, k = 3)
table(cn, auto$origin)

## Bonus exercise: train a reasonable k-NN or other ML model classifying cars as American VS other origin (target for AUC > 0.95)
# install.packages("gbm")
library(gbm)
auto <- Auto[,1:6]
auto$american <- Auto$origin == 1
str(auto)
summary(auto)
l <- dim(auto)[1]
i <- sample(1:l, as.integer(l * 0.75))
train <- auto[i,]
test <- auto[-i,]
md <- gbm(american ~ ., data = train, distribution = "bernoulli", n.trees = 100, interaction.depth = 10, shrinkage = 0.05, cv.folds = 3)
gbm.perf(md, plot.it = TRUE)
md
p <- predict(md, test, n.trees = 100) 
conf <- table(ifelse(p > 0,1,0), test$american)
conf
# Accuracy
(conf[2,2] + conf[1,1]) / (conf[1,1] + conf[1,2] + conf[2,2] + conf[2,1])
