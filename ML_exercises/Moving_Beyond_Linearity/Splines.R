library(MASS)

## 1
fit <- lm(medv ~bs(lstat, knots = c(10,20)), data = Boston)
lstat.grid <- seq(0,50, by = 1)
preds <- predict(fit, newdata = list(lstat = lstat.grid), se = TRUE )
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

MC1 <- 2

## 2
fit <- lm(medv ~ns(lstat, df = 5), data = Boston)
lstat.grid <- seq(0,50, by = 1)
preds <- predict(fit, newdata = list(lstat = lstat.grid), se = TRUE )
MC1 <- 2

## 3
fit <- smooth.spline(rm, medv, cv = TRUE)
df.cv <- fit$df

## 4 
fit <- loess(medv ~ rm,span = .4, data = Boston)
rm.grid = seq(4, 8, by = 0.1)
preds <- predict(fit, newdata = data.frame(rm = rm.grid))
preds
fit
plot(preds)
fit2 <- loess(medv ~rm, data = Boston,span = 0.2)
preds2 <- predict(fit2, newdata = data.frame(rm = rm.grid))
plot(preds2)
MC1 <- 1