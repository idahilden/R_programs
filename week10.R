# Week 10
# Exercise 5, 10, 11 and 12 from Section 7 

# Exercise 10
library(ISLR)
library(leaps)
attach(College)
dim(College)
names(College)

# (a)
set.seed(345)
index <- sample(length(Outstate), length(Outstate)/2)
train <- College[index,]
test <- College[-index,]

regfit.fwd <- regsubsets(Outstate ~ ., data = train, nvmax = 18, method = "forward")

fwd.summary <- summary(regfit.fwd)
fwd.summary$rsq
which.max(fwd.summary$rsq)
par(mfrow=c(1,1))
plot(fwd.summary$rsq, xlab = "Number of Variables", ylab = "RSq", type="l")
points(17, fwd.summary$rsq[17], col="red", cex=2, pch=20)
points(6, fwd.summary$rsq[6], col="red", cex=2, pch=20)
# almost a straight line between the two points
# Hence there is little evidence of improvement from a 6 variable model to a 17 variable model
par(mfrow=c(2,2))
plot(fwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
which.min(fwd.summary$rss)
points(6, fwd.summary$rss[6], col="red", cex=2, pch=20)
plot(fwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type="l")
which.max(fwd.summary$adjr2)
points(6, fwd.summary$adjr2[6], col="red", cex=2, pch=20)
plot(fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(fwd.summary$cp)
points(6, fwd.summary$cp[6], col="red", cex=2, pch=20)
plot(fwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(fwd.summary$bic)
points(6, fwd.summary$bic[6], col="red", cex=2, pch=20)

# In rsqr, rss, asj rsq, cp and bic there is little evidence that the model will improve 
# with more than 6 variables. Hence we will choose a model with 6 variables when
# forward stepwise selection is used on the training data.

reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coef(reg.fit, 6)

# (b)
#install.packages('gam')
library(gam)
gam.mod <- gam(Outstate ~ Private + s(Room.Board, df=2) + s(PhD, df=2) + s(perc.alumni, df=2) + 
                 s(Expend, df=5) + s(Grad.Rate, df=2), data = train)

par(mfrow = c(2, 3))
plot(gam.mod, se = T, col = "blue")

# (c)
gam.pred <- predict(gam.mod, test)
gam.err <- mean((test$Outstate - gam.pred)^2)
gam.err
gam.tss = mean((test$Outstate - mean(test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss

# Rsqr is now 0.77

# (d)
summary(gam.mod)
# All variables showes evidence of statistical significance with p-value < 0.05

# Exercise 11
#(a)
set.seed(4812)
x1 <- rnorm(100)
x2 <- rnorm(100)
eps <- rnorm(100, sd=0.1)
y <- -2.1 + 1.3*x1 + 0.54*x2 + eps
#(b)
beta0 <- rep(NA, 1000)
beta1 <- rep(NA, 1000)
beta2 <- rep(NA, 1000)
beta1[1] <- 10
#(c)
#y - B1*x1 = B0 + B2*x2 + e
a <- y-beta1*x1
beta2 <- lm(a ~ x2)$coef[2]
print(beta2)

# (d)
#y - B2*x2 = B0 + B1*x1 + e
a <- y-beta2*x2
beta1 <- lm(a ~ x1)$coef[2]
print(beta1)

# (e)
for (i in 1:1000) {
  a = y - beta1[i] * x1
  beta2[i] = lm(a ~ x2)$coef[2]
  a = y - beta2[i] * x2
  lm.fit = lm(a ~ x1)
  if (i < 1000) {
    beta1[i + 1] = lm.fit$coef[2]
  }
  beta0[i] = lm.fit$coef[1]
}
plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas", ylim = c(-2.2, 
                                                                             1.6), col = "green")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")

# (f)

lm.fit <- lm(y ~ x1 + x2)

plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas", ylim = c(-2.2, 
                                                                             1.6), col = "green")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")
abline(h = lm.fit$coef[1], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
abline(h = lm.fit$coef[2], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
abline(h = lm.fit$coef[3], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))








