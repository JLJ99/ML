library(ISLR2)
library(MASS)

## 1
fit <- lm(wage ~ poly(year,3, raw = TRUE))
yearlims <- range(year)
year.grid <- seq(from = yearlims[1], to = yearlims[2])
preds <- predict(fit, newdata = list(year = year.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

## 2
fit.1 <- lm(medv ~crim, data = Boston)
fit.2 <- lm(medv ~poly(crim,2), data = Boston)
fit.3 <- lm(medv ~poly(crim,3), data = Boston)
fit.4 <- lm(medv ~poly(crim,4), data = Boston)
fit.5 <- lm(medv ~poly(crim,5), data = Boston)

anova.medv <- anova(fit.1,fit.2,fit.3,fit.4,fit.5)
anova.medv
MC1 <- 3

## 3
fit.1 <- lm(medv ~crim + rm + lstat, data = Boston)
fit.2 <- lm(medv ~poly(crim,2)+ rm + lstat, data = Boston)
fit.3 <- lm(medv ~poly(crim,3)+ rm + lstat, data = Boston)

anova.medv <- anova(fit.1,fit.2,fit.3)
anova.medv
MC1 <- 1

## 4

fit <- glm(I(medv>30) ~ poly(dis,3), data = Boston, family = binomial)
dislim <- range(1,12)
dis.grid <- seq(from = dislim[1], to = dislim[2], by = 0.1)
preds <- predict(fit, newdata = list(dis = dis.grid), se = TRUE )
pfit <- exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+ exp(se.bands.logit))

## 5
fit <- glm(I(medv>30) ~ poly(dis,3), data = Boston, family = binomial)
dislim <- range(1,12)
dis.grid <- seq(from = dislim[1], to = dislim[2], by = 0.1)
preds <- predict(fit, newdata = list(dis = dis.grid), se = TRUE , type ="response")
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

preds
MC1 <- 1

## 6
breakpoints <- c(1,3,6,9,12)
fit <- lm(medv ~cut(dis,breakpoints), data = Boston)
MC1 <- 3

