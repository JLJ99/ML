library(ISLR2)
library(survival)

fit.all <- coxph(Surv(time, status)~posres+multi+clinend+sampsize+budget+impact)
fit.all
MC1 <- 1
MC2 <- 4