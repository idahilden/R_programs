# Week 6
# Exercise 8 ((a)-(d))

#(a)
x <- rnorm(100)
epsilon <- rnorm(100)

#(b)
beta0 <- 1
beta1 <- 2
beta2 <- 3
beta3 <- 4

y <- beta0 + beta1*x + beta2*x^2 + beta3*x^3 + epsilon
y

# (c)
xy <- data.frame(x, y)
xy
dim(xy)
library(leaps)
regfit.full <- regsubsets(y ~ poly(x, 10, raw = T), data = xy, nvmax = 10)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type = "l")
which.min(reg.summary$rss)
points(3, reg.summary$rss[3], col="red", cex=2, pch=20)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type="l")
which.max(reg.summary$adjr2)
# I will choose 3 as the number of variables, because the plots shows little 
# difference in the RSS and adjusted Rsqr after 3
points(3, reg.summary$adjr2[3], col="red", cex=2, pch=20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(3, reg.summary$cp[3], col="red", cex=2, pch=20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(3, reg.summary$bic[3], col="red", cex=2, pch=20)

# (d)
# Forward stepwise selection
regfit.fwd <- regsubsets(y ~ poly(x, 10, raw = T), data = xy, 
                         nvmax = 10, method = "forward")
fwd.summary <- summary(regfit.fwd)
fwd.summary$rsq
par(mfrow=c(2,2))
plot(fwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
which.min(fwd.summary$rss)
points(3, fwd.summary$rss[3], col="red", cex=2, pch=20)
plot(fwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type="l")
which.max(fwd.summary$adjr2)
points(3, fwd.summary$adjr2[3], col="red", cex=2, pch=20)
plot(fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(fwd.summary$cp)
points(3, fwd.summary$cp[3], col="red", cex=2, pch=20)
plot(fwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(fwd.summary$bic)
points(3, fwd.summary$bic[3], col="red", cex=2, pch=20)

# Backward stepwise selection
regfit.bwd <- regsubsets(y ~ poly(x, 10, raw=T), data = xy,
                         nvmax = 10, method = "backward")
bwd.summary <- summary(regfit.bwd)
bwd.summary$rsq
par(mfrow=c(2,2))
plot(bwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
which.min(bwd.summary$rss)
points(4, bwd.summary$rss[4], col="red", cex=2, pch=20)
plot(bwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type="l")
which.max(bwd.summary$adjr2)
points(4, bwd.summary$adjr2[4], col="red", cex=2, pch=20)
plot(bwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(bwd.summary$cp)
points(4, bwd.summary$cp[4], col="red", cex=2, pch=20)
plot(bwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(bwd.summary$bic)
points(4, bwd.summary$bic[4], col="red", cex=2, pch=20)

# for forward stepwise selection we would still choose p=3
# for backward stepwise selection we would choose p=4
# eventhough all the models sugest that we should choose a larger p,
# there is little evidence from the plots that there are much improvements
# in the models with a larger p than what we have chosen.
