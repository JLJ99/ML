rm(list = ls())
library(survival)
library(ISLR2)

# This example makes use of the data in Table 11.4.
df <- data.frame(observation=rep(c(26.5, 37.2, 57.3, 90.8, 20.2, 89.8)),
                 censoring=rep(c(1,1,1,0,0,0)),
                 covariate=rep(c(0.1, 11, -0.3, 2.8, 1.8, 0.4)), stringsAsFactors=T)


# Question 1
df$group <- ifelse(df$covariate < 2,"Group 1","Group 2")
fit.km <- survfit(Surv(df$observation,df$censoring)~df$group)
plot(fit.km,col =c(2,4))

MC1 <- 3
# Question 2
fit.cox <-coxph(Surv(df$observation,df$censoring)~df$group)
fit.cox
MC2 <-4


# Question 3
logrank.test <- survdiff(Surv(df$observation,df$censoring)~df$group)
logrank.test$chisq
MC3 <- 2