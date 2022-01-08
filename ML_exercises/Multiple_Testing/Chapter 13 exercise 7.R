library(ISLR2)
# question 1
lm.fit1 <- lm(Sales~Advertising,data =Carseats)# Advertising
lm.fit2 <- lm(Sales~Age,data =Carseats) # Age
lm.fit3 <- lm(Sales~CompPrice,data =Carseats) # CompPrice
lm.fit4 <- lm(Sales~Education,data =Carseats) # Education
lm.fit5 <- lm(Sales~Income,data =Carseats) # Income
lm.fit6 <- lm(Sales~Population,data =Carseats) # Population
lm.fit7 <- lm(Sales~Price,data =Carseats) # Price

p.values <- rep(0,7)
p.values[1] <- summary(lm.fit1)$coefficients[2,4]
p.values[2] <- summary(lm.fit2)$coefficients[2,4]
p.values[3] <- summary(lm.fit3)$coefficients[2,4]
p.values[4] <- summary(lm.fit4)$coefficients[2,4]
p.values[5] <- summary(lm.fit5)$coefficients[2,4]
p.values[6] <- summary(lm.fit6)$coefficients[2,4]
p.values[7] <- summary(lm.fit7)$coefficients[2,4]

rejected.null.hypotheses <- which(p.values<0.05)

MC1 <- 2
p.values.fwer <- p.adjust(p.values, method = "holm")
rejected.null.hypotheses.FWER <- which(p.values.fwer<0.05)

p.values.fdr <- p.adjust(p.values, method = "BH")
rejected.null.hypotheses.FDR <- which(p.values.fdr<0.2)
