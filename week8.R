# week 8

# Exercise 9 section 7
set.seed(15)
library(MASS)
attach(Boston)
names(Boston)

# (a)
fit <- lm(nox ~ poly(dis, 3), data=Boston)
summary(fit)
# R2 = 71.4 <- quite good. All variables show statistical significance with p-value < 0.05.
dislims <- range(dis)
dis.grid <- seq(from=dislims[1], to=dislims[2], by=0.1)
preds <- predict(fit, newdata = list(dis=dis.grid), se=TRUE)

plot(dis, nox, xlim=dislims, cex=.5, col="darkgrey")
lines(dis.grid, preds$fit, lwd=2, col="blue")
title("Cubic polynomial")
# the model fits the data quite well and is a smooth fit.

# (b)
rss <- rep(NA, 10)
for (i in 1:10) {
  fit <- lm(nox ~ poly(dis, i), data = Boston)
  rss[i] <- sum(fit$residuals^2)
}
rss
plot(1:10, rss, xlab = "Degree", ylab = "Residual sum of squares", type = "l", pch = 20, 
     lwd = 2)
# The rss decreases as the degree of polynomial increases.
# The most dramatic decrease is from 1 till 3, then monotinically decrease from 3 to 8
# a slight decrease from 8 to 10, but not significant difference.

# (c)
library(boot)
k=10
set.seed(50)
cv.errors <- rep(NA, 10)
for (i in 1:k) {
  glm.fit <- glm(nox ~ poly(dis, i), data = Boston)
  cv.errors[i] <- cv.glm(Boston, glm.fit, K=10)$delta[2]
}
cv.errors
plot(1:10, cv.errors, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, 
     lwd = 2)
# The cross validation error decreases from 1 to 4, and increase from 4 to 8 then it 
# decreases again. Here we would choose polynomial with degree 4.

# (d)
# We see that dis has limits of about 1 and 13 respectively. We split this range 
# in roughly equal 4 intervals and establish knots at [4,7,11]. Note: bs function 
# in R expects either df or knots argument. If both are specified, knots are 
# ignored.
library(splines)

fit.splines <- lm(nox ~ bs(dis, df=4, knots = c(4,7,11)), data = Boston)
pred <- predict(fit.splines, list(dis=dis.grid))

plot(dis, nox, col="grey")
lines(dis.grid, pred, col="red", lwd=2)
title("Regression Splines")

# (e)
rss <- rep(NA, 20)

for (i in 3:20) {
  fit <- lm(nox ~ bs(dis, df=i), data = Boston)
  pred <- predict(fit, list(dis=dis.grid))
  rss[i] <-  sum(fit$residuals^2)
}
rss[-c(1,2)]

# (f)
library(boot)
k=20
set.seed(55)
cv.errors <- rep(NA, 20)
for (i in 1:k) {
  glm.fit <- glm(nox ~ bs(dis, df=i), data = Boston)
  cv.errors[i] <- cv.glm(Boston, glm.fit, K=20)$delta[2]
}
cv.errors[-c(1,2)]
plot(3:20, cv.errors[-c(1,2)], xlab = "Degrees of freedom", 
     ylab = "Cross-Validation Errors", type = "l")
title("Degrees of freedom for a regression spline")
# Very jumpy cv-error. Lowest around 12, so we choose 12 as the optimum degrees of freesom
# for a regression spline with this data.



