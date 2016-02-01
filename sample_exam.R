library(data.table)
library(ggplot2)
# Load the content of the https://bit.ly/mtcars-csv CSV file and save as df (check the variable names in the manual of mtcars)
df <- read.csv("https://bit.ly/mtcars-csv")
# Transform df to a data.table object
setDT(df)
str(df)
# Count the number of cars with 4 gears
df[gear == 4,.N]
# Count the number of cars with 4 gears and less than 100 horsepower
df[gear == 4 & hp < 100,.N]
# What's the overall weight of cars with 4 cylinders?
df[cyl == 4,.(wt = sum(wt))]
# Which car is the heaviest?
df[which.max(wt)]
# Plot the distribution of weights
ggplot(df, aes(x = wt)) + geom_histogram()
# Plot the distribution of gears
ggplot(df, aes(x = gear)) + geom_histogram()
# Plot the distribution of weights per gears
ggplot(df, aes(x = wt)) + geom_histogram() + facet_grid( ~ gear)
# Plot the average weight per gears
ggplot(df[,.(wt = mean(wt)), by = gear], aes(x = gear, y = wt)) + geom_point() 
# Which car has the best fuel consumption?
df[which.max(mpg)]
# Plot the weight and horsepower of cars
ggplot(df, aes(x = hp, y = wt)) + geom_point()
# Add a linear trend line to the above plot
ggplot(df, aes(x = hp, y = wt)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# Add a 3rd degree polynomial model to the above plot
ggplot(df, aes(x = hp, y = wt)) + geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE)
# Fit a linear model on hp to predict weight
fit <- lm(formula = wt ~ hp, data = df)
summary(fit)
# Estimate the weight based on the above model for Lotus Europa
df[car == "Lotus Europa"]
predict(fit, newdata = df[car == "Lotus Europa"])
# Compute a new variable in the dataset for the ratio of wt and hp
df[, ratio := wt / hp]
summary(df)
# Plot the distribution of this new variable on a boxplot
boxplot(df$ratio)
ggplot(df, aes(x = ratio)) + geom_boxplot()
# Create an aggregated dataset on mtcars including the average hp and wt grouped by the number of gears
df[, .(avg_hp = mean(hp), avg_wt = mean(wt)), by = gear]
# Merge the average hp and wt per gears from the above dataset to the original df object based on the number of gears
merge(df, df[, .(avg_hp = mean(hp), avg_wt = mean(wt)), by = gear], by = "gear")
# Compute a new variable for fuel consumption using the "liters per 100 kilometers" unit based on mpg
df[,lpk := 235.214583 / mpg]
# Which car has the best fuel consumption?
df[which.min(lpk)]
# Compute wt2 to store the weight in kilograms based on wt
df[,wt2 := wt * 1000]
# Apply k-means clustering on the dataset to split the observations into 3 groups
kmeans(df[,-1, with = FALSE], centers = 3)
# Perform hierarchical clustering on the dataset and plot the dendogram
dm <- dist(df[,-1, with = FALSE])
hc <- hclust(dm)
plot(hc)
# Build a decision tree to tell if a car has automatic or manual transmission
library(rpart)
df <- read.csv("https://bit.ly/mtcars-csv") # RPart does not like data.table
df$am <- as.factor(df$am)

set.seed(2016)
l <- dim(df)[1]
i <- sample(1:l, as.integer(l * 0.75))
train = df[i, -1, with = FALSE]
test = df[-i, -1, with = FALSE]
ct <- rpart(am ~ ., data = train)
summary(ct)
# Visualize the above decision tree
plot(ct)
text(ct)
# Create a confusion matrix for the above model
table(test$am, predict(ct, newdata = test, type = "class"))
# Use the k-NN algorithm to fit a similar model and decide on the best number of neighbors to use
library(class)
ratio = data.frame(k = 0, error = 0)
for (k in 1:as.integer(l / 2)) {
  res <- knn(train[,-10, with = FALSE], test[,-10, with = FALSE], train$am, k = k)
  conf <- table(test$am, res)
  TP <- conf[2,2] / (conf[2,2] + conf[2,1])
  TN <- conf[1,1] / (conf[1,1] + conf[1,2])
  ratio <- rbind(ratio, c(k = k, error = mean(c(TP, TN))))
}
ratio
