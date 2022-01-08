library(ISLR2)
attach(Khan)
## 1
set.seed(1)
x <- rbind(xtrain, xtest)
y <- c(as.numeric(ytrain), as.numeric(ytest))

x <- as.matrix(x)
x1 <- x[which(y == 2),]
x2 <- x[which(y == 4),]
n1 <- nrow(x1)
n2 <- nrow(x2)

t.out <- t.test(x1[, 20], x2[, 20], var.equal = TRUE)
TT <- t.out$statistic
tt.pvalue <- t.out$p.value

set.seed(1)
B <- 10000
Tbs <- rep(NA, B)
for (b in 1:B) {
   dat <- sample(c(x1[, 20], x2[, 20]))
   Tbs[b] <- t.test(dat[1:n1], dat[(n1 + 1):(n1 + n2)], var.equal = TRUE)$statistic
 }
rs.pvalue <- mean((abs(Tbs) >= abs(TT)))
