# Exercise 2.7
car.data <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = T)
names(car.data)
attach(car.data)

#  (a): Using variables from chapter: engine.size, fuel, cylinders, curb.weight, city.distance
fuel1 <- factor(fuel, levels=c("gas","diesel"))
cylinders2 <- factor(n.cylinders==2)

mod <- lm(log(highway.distance) ~ log(engine.size) + fuel1 + cylinders2 + log(curb.weight))
print(summary(mod))
# R2 = 0.86

#  (b): Using variables listed in Appendix B.2
names(car.data)

mod2 <- lm(log(highway.distance) ~ log(engine.size) + fuel1 + cylinders2 + 
             log(curb.weight) + factor(brand) + factor(aspiration) + factor(bodystyle) +
             factor(drive.wheels) + factor(engine.location) + wheel.base + 
             length + width + compression.ratio + log(HP) + peak.rot)
print(summary(mod2))
# R2 = 0.92


