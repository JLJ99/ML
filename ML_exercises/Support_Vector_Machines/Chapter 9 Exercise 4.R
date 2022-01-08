library(e1071)
set.seed(1)
x <- rnorm(100)
y <- 4 * x^2 + 1 + rnorm(100)
class <- sample(100, 50)
y[class] <- y[class] + 3
y[-class] <- y[-class] - 3
plot(x[class], y[class], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30))
points(x[-class], y[-class], col = "blue")
z <- rep(-1, 100)
z[class] <- 1
data <- data.frame(x = x, y = y, z = as.factor(z))

set.seed(5)
train <- sample(1:nrow(data), 0.5*nrow(data))
data.train <- data[train,]
data.test <- data[-(train),]

svm.linear <- svm(z~., data = data.train, kernel = "linear", cost = 10)
pred.linear <- predict(svm.linear, newdata = data.test)

acc.linear <- sum(pred.linear == data.test$z)/50

svm.poly <- svm(z~., data = data.train, kernel = "polynomial",degree = 2, cost = 10)
pred.poly <- predict(svm.poly, newdata = data.test)
acc.poly <- sum(pred.poly == data.test$z)/length(data.test$z)

svm.radial <- svm(z~., data = data.train, kernel = "radial",gamma = 1, cost = 10)
pred.radial <- predict(svm.radial, newdata = data.test)
acc.radial <- sum(pred.radial == data.test$z)/length(data.test$z)