library(ISLR2)

lm.fit1 <- lm(Apps ~Accept)
summary(lm.fit1)
plot(lm.fit1)
Accepter <- data.frame(Accept = 2000)
pred <- predict(lm.fit1,newdata = Accepter)
pred
conf.int.95 <- predict(lm.fit1, newdata = Accepter, interval = "confidence")
conf.int.95
pred.int.95 <- predict(lm.fit1, newdata = Accepter, interval = "prediction")
pred.int.95  

MC1 <- 3
MC2 <- 4

abline(lm.fit1)

lm.fit2 <- lm(Apps ~ poly(Accept, 2))
lm.fit3 <- lm(Apps ~ poly(Accept,3))

college.anova <- anova(lm.fit1, lm.fit2, lm.fit3)
MC3 <- 3