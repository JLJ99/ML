library(ISLR2)
library(boot)
library(splines)

# 1
set.seed(1)
deltas.cut <- rep(NA,10)
for(i in 2:10){
  Auto$displacement.cut <- cut(Auto$displacement,i)
  fit <- glm(mpg~displacement.cut,data = Auto)
  deltas <- cv.glm(Auto,fit,K=10)
  deltas.cut[i] <- deltas$delta[1]
}
plot(deltas.cut)
d.min.cut <- 9

# 2
set.seed(1)
deltas.ns <- rep(NA,10)
for(i in 3:10){
  fit <- glm(mpg~ns(displacement,i),data = Auto)
  deltas <- cv.glm(Auto,fit,K=10)
  deltas.ns[i] <- deltas$delta[1]
}
plot(deltas.ns)
df.min.ns <- 10

plot(Auto$displacement,Auto$mpg)
displacement.grid <-seq(range(Auto$displacement)[1],range(Auto$displacement)[2],0.1) 

fit.ns <- glm(mpg~ns(displacement,10),data = Auto)
preds<- predict(fit,newdata = list(displacement = displacement.grid))
lines(preds)
