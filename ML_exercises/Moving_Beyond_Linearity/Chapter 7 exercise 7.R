library(ISLR2)
library(gam)

MC1 <- 2
MC2 <- 2
fit1 <- gam(wage~lo(year, span = 0.7)+ s(age,5)+education,data = Wage)
fit2 <- gam(wage~lo(year, span =0.7)+ s(age,5)+education +jobclass,data = Wage)
fit3 <-gam(wage~lo(year,  span =0.7)+ s(age,5)+education+maritl,data = Wage)
fit4 <- gam(wage~lo(year, span = 0.7)+ s(age,5)+education+jobclass+maritl,data = Wage)
anova.wage <- anova(fit1,fit2,fit3,fit4,test = "F")
anova.wage

MC3 <- 4
