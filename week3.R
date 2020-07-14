# Exercise 9

library(ISLR)
auto <- Auto
attach(auto)
names(auto)

# (a)
# Scatterplot
plot(auto)

# (b)
# Correlation matrix
cor(auto[-9])

# (c)
# multiple linear regression with mpg as response and all other variables, except name, as predictors
fit <- lm(mpg ~ . -name, data = auto)
summary(fit)
# R2 = 0.82 <- fair realtionship between predictors and response
# p-values < 0.05 for displacement, weight, year and origin <- shows statistical significance
# acceleration, horsepower and cylinders has relativly large p-value <- shows less statistical significance

#(d)
# diagnostic plots
par(mfrow=c(2,2))
plot(fit)
# Nr 1: residuals (Anscombe plot)
        # Ideally, the residual plot will show no fitted pattern. 
        # That is, the red line should be approximately horizontal at zero.
        # In this case, there is a slight u-shape, which means we might have problem with certain points.
        # point 323, 227 and 326 seems to be unusial large outliners.
# Nr 2: q-q
        # The part of the graph that conforms least to expectations lies 
        # in the upper tail of the distribution, the portion above 2. 
        # Specifically, these observed residuals are of much larger absolute value 
        # than the expected ones, indicating a heavy tail with respect to the normal curve.
        # from (-3, 2) is quite satisfactory (the points are on the normal line).
# Nr 3: scale-location
        # This plot shows if residuals are spread equally along the ranges of predictors. 
        # It’s good if you see a horizontal line with equally spread points.
        # which is quite good in this case.
# Nr 4: Leverage (Cook's distance)
        # observation nr 14 is far away from the other points <- outliner, shows unusually high leverage.
        # The other residuals appear clustered to the left.

# (e)
# interaction effects
fit2 <- summary(lm(mpg ~ . -name + displacement:horsepower + weight:displacement, data = auto))
fit3 <- summary(lm(mpg ~ . -name + displacement*horsepower + weight*displacement, data = auto))
fit2
fit3
# used the correlation matrix to fit the variables that has the most correlation
# acceleration is the variable with the least correlation to the other variables,
# hence, it becomes less significant in fit2 and fit3 models.
# fit2: R2 = 0.86, p-values < 0.05, except acceleration and cylinders
# fit 3 R2 = 0.86, p-values < 0.05, except acceleration and cylinders

# (f)
# non-linear transformations of the predictors
# log transformation
fit.log <- lm(mpg ~ log(cylinders) + log(displacement) + log(horsepower) + log(weight) + 
                log(acceleration) + log(year) + log(origin), data = auto)
summary(fit.log)
# R2 = 0.85 <- quite good
# all p-values < 0.05, except for log(cylinders) and log(displacement)
# square root transformation
fit.sqrt <- lm(mpg ~ sqrt(cylinders) + sqrt(displacement) + sqrt(horsepower) + sqrt(weight) +
                 sqrt(acceleration) + sqrt(year) + sqrt(origin), data = auto)
summary(fit.sqrt)
# R2 = 0.83 <- quite good
# all p-values < 0.05, except for cylinders, displacement and acceleration
# not as good results as the log transformation, but still decent
# square transformation 
fit.sq <- lm(mpg ~ cylinders + I(cylinders^2) + displacement + I(displacement^2) + horsepower + I(horsepower^2) 
             + weight + I(weight^2) + acceleration + I(acceleration^2) + year + I(year^2) + origin + I(origin^2), data=auto)
summary(fit.sq)
# R2 = 0.88 <- best so far
# all variables significant (p-value < 0.05) except cylinders(and the square term), displacement(and the square term),
# origin(and the square term). 

# Exercise 10

carseats <- Carseats
attach(carseats)

# (a)

mod <- lm(Sales ~ Price + Urban + US, data = carseats)
summary(mod)

# (b)

# Price: negavtive; the more sales the lower price
# The linear regression suggests a relationship between price and sales given the 
# low p-value of the t-statistic. The coefficient states a negative relationship 
# between Price and Sales: as Price increases, Sales decreases.

# urbanYes: The linear regression suggests that there isn’t a relationship between 
# the location of the store and the number of sales based on the high p-value of the 
# t-statistic.

# USYes: positive; more sales in US
# The linear regression suggests there is a relationship between whether the store 
# is in the US or not and the amount of sales. The coefficient states a positive 
# relationship between USYes and Sales: if the store is in the US, the sales will 
# increase by approximately 1201 units.
















