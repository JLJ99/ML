library(ISLR2)
library(survival)

fit.pub <- survfit(Surv(time, status)~ 1)
plot(fit.pub, xlab = "Months", ylab = "Probability of Not Being Published")

fit.posres <- survfit(Surv(time, status)~posres)
plot(fit.posres, xlab = "Months", ylab = "Probability of Not Being Published", col = 3:4)
legend("topright", c("Negative Result", "Positive Result"), col = 3:4, lty = 1)
