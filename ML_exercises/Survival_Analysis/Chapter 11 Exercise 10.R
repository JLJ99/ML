library(ISLR2)
library(survival)


## 1 
fit.km1 <- survfit(Surv(time,status)~1,data = BrainCancer)

## 2
fit.cox <- coxph(Surv(time,status)~.,data = BrainCancer)

fit.cox

MC1 <- 3
MC2 <- 3

## 3
unique(BrainCancer$ki)

