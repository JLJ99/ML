library(ISLR2)
library(glmnet)

#set seed
set.seed(42)

#sample data
college.train.rows <- sample( 1:nrow(College), nrow(College)*0.6)
college.train_1 <- College[college.train.rows, ]
college.train<-as.data.frame(college.train_1)
college.test.rows <- (-college.train.rows)
college.test_1 <- College[college.test.rows,]
college.test <- as.data.frame(college.test_1)



#basic models
lm.fit <- lm(Apps ~ . , data = college.train)
pred.lm <- predict(lm.fit, newdata = college.test)
lm.error <- mean((pred.lm - college.test$Apps)^2)
#data as matrix
train.x <- model.matrix(Apps~., data = college.train)
test.x <- model.matrix(Apps ~. , data = college.test)
train.y <- college.train$Apps
test.y <- college.test$Apps

# lasso
grid <- 10 ^ seq(4, -2, length = 100)
set.seed(42)

cv.lasso <- cv.glmnet(train.x,train.y, lambda = grid, nfolds = 5, alpha = 1)
bestlam.lasso <- cv.lasso$lambda.min

head(cv.lasso$cvm)

pred.lasso <- predict(cv.lasso, s= bestlam.lasso, newx = test.x)
coef.lasso <- predict(cv.lasso, s = bestlam.lasso, newx = test.x, type = "coef")
lasso.error <- mean((pred.lasso-test.y)^2)


