auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = T)
attach(auto)

# 2.1
fuel1 <- factor(fuel, levels=c("gas","diesel"))
fit3 <- lm(city.distance~ engine.size + I(engine.size^2)+
             I(engine.size^3)+ fuel1)
print(summary(fit3))
# R2 = 0.59, all variables har p-value < 0.05 except the cubic and quadratic term

plot(engine.size, city.distance, type="n",
     ylab="City distance",
     xlab="Engine size",  xlim=c(1,5.5))
d <- fuel=="gas"
points(engine.size[d], (city.distance[d]), col=c("grey"))
points(engine.size[!d], (city.distance[!d]), col=c("dark green"))

x <- (seq(min(engine.size), max(engine.size), length=200))
x <- seq(1,5.5,  length=200)
beta<- coef(fit3)
lines(x, beta[1]+ beta[2]*x+beta[3]*x^2+beta[4]*x^3, col=c("grey"))
lines(x,  beta[1]+ beta[2]*x+beta[3]*x^2+beta[4]*x^3+beta[5],
      col=c("dark green"))

plot(fit3, which=1, sub.caption="", add.smooth=FALSE )
plot(fit3, which=2, sub.caption="", add.smooth=FALSE )

# Removing the cubic term
fuel1 <- factor(fuel, levels=c("gas","diesel"))
fit4 <- lm(city.distance ~ engine.size + I(engine.size^2) + fuel1)
print(summary(fit4))
# R2 = 0.59, all p-values < 0.05
plot(engine.size, city.distance, type="n",
     ylab="City distance",
     xlab="Engine size",  xlim=c(1,5.5))
d <- fuel=="gas"
points(engine.size[d], (city.distance[d]), col=c("grey"))
points(engine.size[!d], (city.distance[!d]), col=c("dark green"))
#
x <- (seq(min(engine.size), max(engine.size), length=200))
x <- seq(1,5.5,  length=200)
beta<- coef(fit4)
lines(x, beta[1]+ beta[2]*x+beta[3]*x^2, col=c("grey"))
lines(x,  beta[1]+ beta[2]*x+beta[3]*x^2+beta[4], col=c("dark green"))

plot(fit4, which=1, sub.caption="", add.smooth=FALSE )
plot(fit4, which=2, sub.caption="", add.smooth=FALSE )

# 2.2
# Predicting values for fit3, for galsoline cars with engine size 1-7
x <- seq(1, 7, 0.1)
x
idx = fuel1 == 'gas'
plot(engine.size[idx], city.distance[idx], xlim = c(1, 7), ylim = c(0, 22))
beta <- coef(fit3)
lines(x, beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3, col = 2, lty = 1, lwd = 2)

#2.3
y = 1/city.distance
fit_17 <- lm(y ~ engine.size  + fuel1)
summary(fit_17) #R-squared: 0.64

r2_17 <-  1 - sum((city.distance - 1/fitted(fit_17))^2) / ((length(city.distance)-1)*var(city.distance))
r2_17 #R-squared: 0.56
# We are performing a non-linear transformation of our targets, to we expect R^2 to be different.
# To compare the models we need to do this in the same space (of our responce), meaning that (2.14) is still a better fit.


