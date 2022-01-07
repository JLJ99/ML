library(e1071)

## 1
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
train <- sample(200, 100)

svmfit <- svm(y~., kernel = "polynomial", degree = 3, cost = 1, data = dat[train,])

## 2
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
train <- sample(200, 100)
svmfit1 <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 0.01)
svmfit2 <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
svmfit3 <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
train.acc1 <-mean(predict(svmfit1, dat[train,]) == dat[train,"y"])
train.acc2 <-mean(predict(svmfit2, dat[train,]) == dat[train,"y"])
train.acc3 <-mean(predict(svmfit3, dat[train,]) == dat[train,"y"])
test.acc1 <-mean(predict(svmfit1, dat[-(train),]) == dat[-(train),"y"])
test.acc2 <-mean(predict(svmfit2, dat[-(train),]) == dat[-(train),"y"])
test.acc3 <-mean(predict(svmfit3, dat[-(train),]) == dat[-(train),"y"])
MC1 <- 3

## 3
make.circle.data = function(n) {
  x1 <- runif(n, min = -1, max = 1)
  x2 <- x1
  y1 <- sqrt(1 - x1^2)
  y2 <- (-1)*y1
  x1 <- c(x1,x2)
  x2 <- c(y1,y2)
  return(data.frame(x1, x2))
}

set.seed(2077)
df1 <- make.circle.data(30)
df2 <- make.circle.data(30)
df1 <- df1 * .5 + rnorm(30, 0, 0.05)
df2 <- df2 + rnorm(30, 0, 0.05)
df <- rbind(df1, df2)
z <- rep(c(-1, 1), each=60)

dat <- data.frame(x=df, y = as.factor(z))
names(dat) <- c('x.1', 'x.2', 'y')

train_idx <- sample(1:nrow(df), nrow(df)*0.8)
train <- dat[train_idx,]
test <- dat[-(train_idx),]
lin.tune.out <- tune(svm,y~., dat = train,kernel = "linear",ranges = list(cost = c(0.01, 0.1, 1, 10, 100)))
poly.tune.out <- tune(svm,y~., dat = train,kernel = "polynomial",ranges = list(cost = c(0.01, 0.1, 1, 10, 100),degree = c(2,3,4,5)))
rad.tune.out <- tune(svm,y~., dat = train,kernel = "radial",ranges = list(cost = c(0.01, 0.1, 1, 10, 100),gamma = c(0.5,1,2,3,4)))
lin.pred <- predict(lin.tune.out$best.model, test)
poly.pred <- predict(poly.tune.out$best.model, test)
rad.pred <- predict(rad.tune.out$best.model, test)
plot(lin.tune.out$best.model,test)
plot(poly.tune.out$best.model,test)
plot(rad.tune.out$best.model,test)
MC1 <- 3
