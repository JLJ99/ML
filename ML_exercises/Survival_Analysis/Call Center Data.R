library(ISLR2)
library(survival)
library(coxed)

set.seed(4)
N <- 2000
Operators <- sample(5:15, N, replace = T)
Center <- sample(c("A", "B", "C"), N, replace = T)
Time <- sample(c("Morn.", "After.", "Even."), N, replace = T)
X <- model.matrix(~ Operators + Center + Time)[, -1]

true.beta <- c(0.04, -0.3, 0, 0.2, -0.2)
h.fn <- function(x) return(0.00001 * x)

queuing <- sim.survdata(N = N, T = 1000, X = X, beta = true.beta, hazard.fun = h.fn)

# Kaplan-Meier survival curves
fit.Center <- survfit(Surv(y,failed)~Center, data = queuing$data)
plot(fit.Center, xlab = "Seconds", ylab = "Probability of Still Being on Hold", col = c(2, 4, 5))
legend("topright", c("Call Center A", "Call Center B", "Call Center C"), col = c(2, 4, 5), lty = 1)

fit.Time <- survfit(Surv(y,failed)~Time, data = queuing$data)
plot(fit.Time, xlab = "Seconds", ylab = "Probability of Still Being on Hold", col = c(2, 4, 5))
legend("topright", c("Morning", "Afternoon", "Evening"), col = c(5, 2, 4), lty = 1)

MC1 <- 1
# log-rank test
log.center <- survdiff(Surv(y,failed)~Center, data = queuing$data)

log.time <- survdiff(Surv(y,failed)~Time, data = queuing$data)

MC2 <- 1
# Cox’s proportional hazards model
cox <- coxph(Surv(y,failed)~.,data = queuing$data)
MC3<-1
