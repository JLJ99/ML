library(ISLR2)
library(leaps)
library(gam)
attach(College)

# 1
set.seed(1)
train <- sample(1:nrow(College),nrow(College)*0.5)
College.train <- College[train,]
College.test <- College[-(train),]

subset.fit <- regsubsets(Outstate~.,data = College, nvmax = 17, subset = train, method = "forward" )
summary <-  summary(subset.fit)
plot(summary$adjr2)
plot(summary$cp)
MC1 <- 3

subset.coef <- coef(subset.fit, id = 4)

# 2
gam.fit <- gam(Outstate~Private+ s(Room.Board,2)+s(PhD,2)+s(Expend,5), data = College, subset = train)
par(mfrow = c(2, 2))
plot(gam.fit)

# 3
gam.preds <- predict(gam.fit, newdata = College.test)

mse <- mean((College.test$Outstate - gam.preds)^2)
