# Week 5
# Exercise 8
# (a)
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)
# n=100, p=2

# (b)
plot(x,y)
# x intervall (-2, 2)
# y intervall (-12, 2)
# inverted U-shape of the points

# (c)
library(boot)
xy <- data.frame(x, y)
head(xy)
fit <- glm(y ~ x)
cv.err <- cv.glm(xy, fit)
cv.err$delta

set.seed(2)
cv.error <- rep(0, 4)
for (i in 1:4) {
  fit <- glm(y ~ poly(x, i))
  cv.error[i] <- cv.glm(xy, fit)$delta[1]
}
cv.error
## 9.3024399 0.9842903 0.9955898 1.0077718

# (d)
set.seed(3)
cv.error <- rep(0, 4)
for (i in 1:4) {
  fit <- glm(y ~ poly(x, i))
  cv.error[i] <- cv.glm(xy, fit)$delta[1]
}
cv.error
# same result, this is because we are using LOOCV, 
# which uses all of the data and are not dependent and are not 
# dependent on how data is split like K-fold-CV is

# (e)
# i = 2 has the smallest cross-validation error
# This is expected because its the same shape as the true data

# (f)
summary(glm(y~poly(x,1)))
summary(glm(y~poly(x,2)))
summary(glm(y~poly(x,3)))
summary(glm(y~poly(x,4)))
# we see that the cubic and quadratic terms are not significant in model 3 and 4. 
# In all the model 2 all variables are significant
# in the first model the linear term is not significant
# this is in agreement with the cv.errors.
