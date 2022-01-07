library(e1071)

## 1
set.seed(2077)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1,] <- x[y == 1,] + 1
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., kernel ="linear", cost = 0.1, scale = FALSE, data = dat)
summary(svmfit)
plot(svmfit,dat)
nSV <- 16
MC1 <- 1

## 2
set.seed(2077)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1,] <- x[y == 1,] + 1
dat <- data.frame(x = x, y = as.factor(y))
tune.out <- tune(svm, y~., data = dat, kernel = "linear", scale = TRUE, ranges =list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
best.parameters <- tune.out$best.parameters
best.performance <- tune.out$best.performance

## 3
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1,] <- x[y == 1,] + 1
dat <- data.frame(x = x, y = as.factor(y))

xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1,] <- xtest[ytest == 1,] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

svmfit <- svm(y~.,data = dat, kernel = "linear", scale = FALSE, cost = 0.01)
ypred <- predict(svmfit, testdat)
cf.test <- table(predict = ypred, truth = testdat$y)

## 4
MC1 <- 1