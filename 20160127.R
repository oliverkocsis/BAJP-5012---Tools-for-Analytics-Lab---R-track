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
