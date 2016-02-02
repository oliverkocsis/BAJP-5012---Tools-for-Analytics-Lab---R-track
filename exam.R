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
?Auto # Origin of car (1. American, 2. European, 3. Japanese)
str(Auto)
## Build and visualize a decision tree to tell if a car was made in America, Europe or Japan
library(rpart)
set.seed(2016)
auto <- Auto
auto$origin <- as.factor(auto$origin) # classification
l <- dim(Auto)[1]
i <- sample(1:l, as.integer(l * 0.75))
train = auto[i, -9]
test = auto[-i, -9]
ct <- rpart(origin ~ ., data = train)
summary(ct)
plot(ct)
text(ct)

## Apply k-means or hierarchical clustering on the dataset to split the observations into 3 groups
## Bonus exercise: train a reasonable k-NN or other ML model classifying cars as American VS other origin (target for AUC > 0.95)