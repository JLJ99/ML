library(MASS)
library(boot)

## 1
fit.poly <- glm(nox~poly(dis,3),data = Boston)

summary(fit.poly)
MC1 <- 3


## 2
rss <- vector()
for(i in 1:10){
  fit <- glm(nox~poly(dis,i),data = Boston)
  rss[i] <- sum((fit$residuals)^2)
}
plot(rss)

## 3
set.seed(1)
deltas.poly <- vector()
for(i in 1:10){
  fit <- glm(nox~poly(dis,i),data = Boston)
  deltas <- cv.glm(Boston, fit,K=10)
  deltas.poly[i] <- deltas$delta[1]
}
plot(1:10,deltas.poly)
d.min.poly <- 4