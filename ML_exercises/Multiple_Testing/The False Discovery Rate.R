library(ISLR2)

## 1
fund.pvalues <- rep(0, 2000)
for (i in 1:2000)
  fund.pvalues[i] <- t.test(Fund[, i], mu = 0)$p.value

q.values.BH <- p.adjust(fund.pvalues, method = "BH")

rejections <- length(q.values.BH[q.values.BH<0.1])


