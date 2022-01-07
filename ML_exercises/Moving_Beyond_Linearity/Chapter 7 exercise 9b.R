library(MASS)
library(boot)
library(splines)

## 1
fit.bs <- lm(nox~bs(dis,df = 4, knots = c(4,7,11)),data = Boston)

plot(Boston$dis,Boston$nox)
dis.grid <- seq(range(Boston$dis)[1],range(Boston$dis)[2],0.1)
preds <- predict(fit.bs, newdata = data.frame(dis =dis.grid))
lines(dis.grid,preds,col = "red")

## 2 
rss <- rep(NA,16)
for(i in 3:16){
  fit <- lm(nox~bs(dis,df = i),data = Boston)
  rss[i] <- sum((fit$residuals)^2)   
}

## 3
set.seed(1)
deltas.bs <- rep(NA,16)
for(i in 3:16){
  fit <- glm(nox~bs(dis,df = i),data = Boston)
  deltas <- cv.glm(Boston,fit,K=10)
  deltas.bs[i] <- deltas$delta[1]  
}
plot(deltas.bs[-c(1,2)])

df.min.bs <- 6