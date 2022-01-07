library(ISLR2)
library(boot)


# 1
set.seed(4)
deltas.poly <- vector()
for(i in 1:10){
  glm.fit <-glm(wage~poly(age,i),data = Wage)
  deltas <- cv.glm(Wage, glm.fit, K=10)
  deltas.poly[i] <- deltas$delta[1]
}
plot(deltas.poly)
d.min.poly <- 4

fit1 <- lm(wage~age, data=Wage)
fit2 <- lm(wage~poly(age, 2), data=Wage)
fit3 <- lm(wage~poly(age, 3), data=Wage)
fit4 <- lm(wage~poly(age, 4), data=Wage)
fit5 <- lm(wage~poly(age, 5), data=Wage)

anova.poly <- anova(fit1,fit2,fit3,fit4,fit5)
anova.poly

MC1 <- 3
MC2 <- 2

# 2
set.seed(1)
deltas.cut <- vector()
for(i in 2:10){
  Wage$age.cut <- cut(Wage$age,i)
  lm.fit <- glm(wage~ age.cut, data=Wage)
  deltas <- cv.glm(Wage, lm.fit, K=10)
  deltas.cut[i] <- deltas$delta[1]
}
plot(deltas.cut)
d.min.cut <- 8

plot(Wage$age,Wage$wage)
age.grid <- seq(range(Wage$age)[1],range(Wage$age)[2])
fit8 <- glm(wage~cut(age,8),data = Wage)
preds <- predict(fit8, newdata = data.frame(age = age.grid))
lines(age.grid,preds, col = "red")
