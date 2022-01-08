set.seed(1)
n <- 20
m <- 100
X <- matrix(rnorm(n * m), ncol = m)

p.values <- vector()
for(i in 1:100){
  t_test<- t.test(X[, i], mu = 0)
  p.values[i] <- t_test$p.value
}

rejected.null.hypotheses <- length(which(p.values < 0.05))

p.values.fwer <- p.adjust(p.values, method = "bonferroni")
rejected.null.hypotheses.FWER <- length(which(p.values.fwer<0.05))

p.values.fdr <- p.adjust(p.values, method = "BH")
rejected.null.hypotheses.FDR <- length(which(p.values.fdr<0.05))




best_manager <- head(order(abs(colMeans(X)),decreasing = TRUE),10)
X_best <- subset(X,select = best_manager)

p.values_top <- vector()
for(i in 1:10){
  t_test<- t.test(X_best[, i], mu = 0)
  p.values_top[i] <- t_test$p.value
}

p.values.fwer.best <- p.adjust(p.values_top, method = "bonferroni")
top.managers.FWER <- which(p.values.fwer.best<0.05)

p.values.fdr.best <- p.adjust(p.values_top, method = "BH")
top.managers.FDR <- which(p.values.fdr.best<0.05)
MC1 <- 2
