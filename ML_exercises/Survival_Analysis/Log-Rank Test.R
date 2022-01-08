library(ISLR2)
library(survival)

logrank.pub <- survdiff(Surv(time, status)~posres)
logrank.pub
MC1 <- 2