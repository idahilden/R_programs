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
