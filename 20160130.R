df <- read.csv("http://bit.ly/BudapestBI-R-csv")
str(df)
plot(df$heightIn, df$weightLb)
fit <- lm(data = df, weightLb ~ heightIn)
summary(fit)
abline(fit)
segments(df$heightIn, df$weightLb, df$heightIn, predict(fit))

predict(fit, newdata = data.frame(heightIn = c(70,25))) # it provides some minus values
plot(df$heightIn, df$weightLb, xlim = c(0,100), ylim = c(0,300))
abline(fit)

plot(df$heightIn, df$weightLb, xlim = c(0,100), ylim = c(0,300))
fit_square  <- lm(data = df, weightLb ~ poly(heightIn, 2))
abline(fit_square) #  does not work

library(ggplot2)
ggplot(df, aes(x = heightIn, y = weightLb)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(df, aes(x = heightIn, y = weightLb)) + geom_point() + geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2))

df <- read.csv("http://bit.ly/math_and_shoes")
str(df)
fit <- lm(data = df, math  ~ size)
summary(fit)
ggplot(data = df, aes(x = size, y = math)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

plot(df)
cor(df)
cor(residuals(lm(data = df, math ~ x)),residuals(lm(data = df, size ~ x)))

# iris and clustering
str(iris)
plot(iris)
fit <- lm(data = iris, Sepal.Width ~ Sepal.Length)
summary(fit)
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()  + geom_smooth(method = "lm", se = FALSE)
fit <- lm(data = iris, Sepal.Width ~ Sepal.Length + Species)
summary(fit)
str(dist(iris)) # NAs by factors
dm <- dist(iris[,1:4])
hc <- hclust(dm)
str(hc)
plot(hc)
rect.hclust(hc, k = 3)
cn <- cutree(hc, k = 3)

i <- data.table(iris)
i[, cluster := cn]
i[,.N, by = .(Species, cluster)]
library(reshape2)
dcast(i[,.N, by = .(Species, cluster)], Species ~ cluster, value.var = "N")
?scale
scale(iris$Sepal.Length)

km <- kmeans(iris[,1:4], 3)
str(km)
table(km$cluster, iris$Species)
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species)
plot(iris$Sepal.Length, iris$Sepal.Width, col = cn)
plot(iris$Sepal.Length, iris$Sepal.Width, col = km$cluster)

library(class)
str(iris)
set.seed(42)
iris <- data.table(datasets::iris)
i <- sample(1:150, 100)
train <- data.table(iris[i])
test <- data.table(iris[-i])
result <- knn(train = train[,1:4, with = FALSE], test = test[,1:4, with = FALSE], cl = train$Species, k = 5)
table(test$Species, result)


library(NbClust)
NbClust(iris[, 1:4, with = FALSE], method = "complete")

library(rpart)
str(iris)
ct <- rpart(Species ~ ., data = train)
plot(ct)
text(ct)
library(partykit)
library(rpart.plot)
rpart.plot(ct)
plot(as.party(ct))
str(ct)
p <- predict(ct, newdata = test, type = 'class')
table(p, test$Species)


df <- read.csv("http://bit.ly/BudapestBI-R-csv")
setDT(df)
str(df)
i <- sample(1:237, 175)
train <- df[i]
test <- df[-i]
k <- knn(train = train[,4:5, with = FALSE], test = test[,4:5, with = FALSE], cl = train$sex, k = )
table(test$sex, k)

t <- rpart(sex ~ heightIn + weightLb, data = train)
p <- predict(t, newdata = test, type = 'class')
table(p, test$sex)
plot(df$heightIn, df$weightLb, col = df$sex )


iris <- data.table(datasets::iris)
pc <- prcomp(iris[,1:4, with = FALSE], scale. = TRUE)
plot(pc$x[,1:2], col = iris$Species)
# what is pc 1 and pc2? 
pc # linear combitaniots of original features

str(eurodist)
cmdscale(eurodist)

m <- cmdscale(eurodist)
plot(m)
plot(m, type = 'n')
text(m[,1], -m[,2], labels(eurodist))
