## 1
MC1 <- 3

## 2
set.seed(1)
x <- matrix(rnorm(10 * 100), 10, 100)
x[, 1:50] <- x[, 1:50] + 1 # mean 1 instead of 0.5
p.values <- rep(0, 100)
for (i in 1:100)
  p.values[i] <- t.test(x[, i], mu = 0)$p.value
decision <- rep("Do not reject H0", 100)
decision[p.values <= .05] <- "Reject H0"
table(decision, c(rep("H0 is False", 50), rep("H0 is True", 50)))

MC1 <- 2