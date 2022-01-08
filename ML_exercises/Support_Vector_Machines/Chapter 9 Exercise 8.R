library(ISLR2)
library(e1071)
attach(OJ)

## 1
set.seed(1)
train <- sample(nrow(OJ),800)
OJ.train <- OJ[train,]
OJ.test <- OJ[-(train),]

## 2
svm.lin <- svm(Purchase~., cost = 10, kernel ="linear", data = OJ.train)
nSV.lin <- sum(svm.lin$nSV)

## 3
pred_train <- predict(svm.lin, OJ.train)
train.err.lin <- (sum(pred_train != OJ.train$Purchase))/800
pred_test <- predict(svm.lin, newdata = OJ.test)
test.err.lin <- (sum(pred_test != OJ.test$Purchase))/270

## 4
set.seed(2)
tune.out.lin <- tune(svm, Purchase~.,kernel ='linear',data = OJ.train, ranges = list(cost = c(0.01,0.1,1)))
best.param.lin <- tune.out.lin$best.parameters

## 5
svm.lin.opt <- svm(Purchase~., cost = 1, kernel ="linear", data = OJ.train)
pred_train.opt <- predict(svm.lin.opt, OJ.train)
train.err.lin.opt <- (sum(pred_train.opt != OJ.train$Purchase))/800
pred_test.opt <- predict(svm.lin.opt, newdata = OJ.test)
test.err.lin.opt <- (sum(pred_test.opt != OJ.test$Purchase))/270

## 6
svm.rad <- svm(Purchase~., cost = 10, kernel ="radial", data = OJ.train)
nSV.rad <- sum(svm.rad$nSV)

pred_train.rad <- predict(svm.rad, OJ.train)
train.err.rad <- (sum(pred_train.rad != OJ.train$Purchase))/800
pred_test.rad <- predict(svm.rad, newdata = OJ.test)
test.err.rad <- (sum(pred_test.rad != OJ.test$Purchase))/270

set.seed(2)
tune.out.rad <- tune(svm, Purchase~.,kernel ='radial',data = OJ.train, ranges = list(cost = c(0.01,0.1,1),gamma =c(0.01,0.1,1)))
best.param.rad <- tune.out.rad$best.parameters

svm.rad.opt <- svm(Purchase~., cost = 1,gamma = 0.01, kernel ="radial", data = OJ.train)
pred_train.opt.rad <- predict(svm.rad.opt, OJ.train)
train.err.rad.opt <- (sum(pred_train.opt.rad != OJ.train$Purchase))/800
pred_test.opt.rad <- predict(svm.rad.opt, newdata = OJ.test)
test.err.rad.opt <- (sum(pred_test.opt.rad != OJ.test$Purchase))/270

## 7
svm.poly <- svm(Purchase~., cost = 10, degree =2,kernel ="polynomial", data = OJ.train)
nSV.poly <- sum(svm.poly$nSV)

pred_train.poly <- predict(svm.poly, OJ.train)
train.err.poly <- (sum(pred_train.poly != OJ.train$Purchase))/800
pred_test.poly <- predict(svm.poly, newdata = OJ.test)
test.err.poly <- (sum(pred_test.poly != OJ.test$Purchase))/270

set.seed(2)
tune.out.poly <- tune(svm, Purchase~.,kernel ='polynomial',data = OJ.train, ranges = list(cost = c(0.01,0.1,1),degree =c(2,3,4)))
best.param.poly <- tune.out.poly$best.parameters

svm.poly.opt <- svm(Purchase~., cost = 1,gamma = 0.01, kernel ="polynomial", data = OJ.train)
pred_train.opt.poly <- predict(svm.poly.opt, OJ.train)
train.err.poly.opt <- (sum(pred_train.opt.poly != OJ.train$Purchase))/800
pred_test.opt.poly <- predict(svm.poly.opt, newdata = OJ.test)
test.err.poly.opt <- (sum(pred_test.opt.poly != OJ.test$Purchase))/270

## 8
MC1 <-1