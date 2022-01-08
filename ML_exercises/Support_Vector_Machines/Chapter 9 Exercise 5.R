library(e1071)
set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)
data <- data.frame(x1 = x1, x2 = x2, y = y)

## 1
plot(data$x1,data$x2, col=ifelse(y>0.5,"red","blue"))

## 2
glm.fit1 <- glm(y~x1+x2, data = data, family = binomial)

## 3
y_predicted <- predict(glm.fit1)

plot(data$x1,data$x2, col=ifelse(y_predicted>0.5,"red","blue"))

MC1 <- 1

## 4
glm.fit2 <- glm(y~x1+x2+poly(x1,2)+poly(x2,2)+x1:x2, family = binomial,data = data)

y2_predicted <- predict(glm.fit2)

plot(data$x1,data$x2, col=ifelse(y2_predicted>0.5,"red","blue"))

MC2 <- 2

## 5
svm.fit1 <- svm(y~., data = data, kernel = "linear",cost = 0.01,scale = FALSE)

y_svm_predicted <- predict(svm.fit1, data)

plot(data$x1,data$x2, col=ifelse(y_svm_predicted>0.5,"red","blue"))

MC3 <-2

##6

svm.fit2 <- svm(y~., data = data, kernel = "radial",cost = 1,gamma =1,scale = FALSE)

y_svm2_predicted <- predict(svm.fit2)

plot(data$x1,data$x2, col=ifelse(y_svm2_predicted>0.5,"red","blue"))

MC4 <-1

## 10
MC5 <-3