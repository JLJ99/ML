library(ISLR2)

## 1
fund.mini <- Fund[, 1:5]
fund.pvalue <- rep(0, 5)
for (i in 1:5)
  fund.pvalue[i] <- t.test(fund.mini[, i], mu = 0)$p.value

p.adjust(fund.pvalue, method = "holm")

MC1 <- 3

## 2
fund.mini <- Fund[, 1:5]

MC1 <- 1

## 3 
MC1 <- 2