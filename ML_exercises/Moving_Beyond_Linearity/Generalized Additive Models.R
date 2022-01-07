library(MASS)

## 1
gam1 <- gam(medv ~ bs(rm,3)+ ns(crim, 4), data = Boston)
gam2 <- gam(medv ~ s(rm,3)+ s(crim,4), data = Boston)

plot.Gam(gam1)
plot.Gam((gam2))
plot(gam1)
plot(gam2)
MC1 <- 2

## 2
gam1 <- gam(medv ~ s(crim,4), data = Boston)
gam2 <- gam(medv ~ rm+s(crim,4), data = Boston)
gam3 <- gam(medv~ s(rm,3)+s(crim,4), data = Boston)

anova(gam1,gam2,gam3)
MC1 <- 3

## 3
gam1 <- gam(medv ~ lo(lstat, span = 0.5)+ s(rm,3), data = Boston)
preds <- predict(gam1, newdata = Boston)
preds

## 4
gam1 <- gam(I(medv > 30) ~ s(dis, df = 4)+ lstat,family = binomial(), data = Boston)
preds <- predict(gam1, newdata = Boston, type = "response")