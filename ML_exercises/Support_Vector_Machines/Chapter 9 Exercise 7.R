library(ISLR2)
library(e1071)
## 1
Auto$mpglevel <- as.factor(ifelse(Auto$mpg > median(Auto$mpg),1,0))
Auto$mpg <- NULL

## 2
set.seed(1)
tune.out.lin <- tune(svm,mpglevel~.,kernel ="linear", ranges = list(cost = c(0.01,0.1,1)),data = Auto)
best.param.lin <- tune.out.lin$best.parameters
best.perform.lin <- tune.out.lin$best.performance  

## 3
tune.out.rad <- tune(svm,mpglevel~.,kernel ="radial", ranges = list(cost = c(0.01,0.1,1),gamma =c(0.01,0.1,1) ),data = Auto)
best.param.rad <- tune.out.rad$best.parameters
best.perform.rad <- tune.out.rad$best.performance  

## 4
tune.out.poly <- tune(svm,mpglevel~.,kernel ="polynomial", ranges = list(cost = c(0.01,0.1,1),degree=c(2,3,4) ),data = Auto)
best.param.poly <- tune.out.poly$best.parameters
best.perform.poly <- tune.out.poly$best.performance  
## 5
MC1 <- 3