library(e1071)

## 1 
MC1 <- 2

## 2
set.seed(1)
train_idx <- sample(1:nrow(iris), nrow(iris)*0.7)
train <- iris[train_idx,]
test <- iris[-(train_idx),]
tune.out <- tune(svm, data = train, Species ~., scale = TRUE, kernel ="linear",ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
bestmod <- tune.out$best.model
score <- mean(predict(bestmod, test) == test$Species)

