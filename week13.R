# Week 13 exercises 3 and 9 section 8
#         exercises 3 and 7 section 9

# Exercise 3


par(mfrow=c(1,1))
p = seq(0, 1, 0.01)
gini = p * (1 - p) * 2
entropy = -(p * log(p) + (1 - p) * log(1 - p))
class.err = 1 - pmax(p, 1 - p)
matplot(p, cbind(gini, entropy, class.err), col = c("red", "green", "blue"))



# Exercise 9

library(ISLR)
attach(OJ)
dim(OJ)
names(OJ)

# (a)
set.seed(876)
index <- sample(dim(OJ)[1], 800)
train <- OJ[index,]
test <- OJ[-index,]

# (b)
library(tree)
tree.oj <- tree(Purchase ~., data = train)
summary(tree.oj)
## The tree only uses three variables: LoyalCH, PriceDiff and ListPriceDiff. 
# It has 6 terminal nodes. Training error rate (misclassification error) for the tree is 0.1588.

# (c)
tree.oj
# spilt criterion LoyalCH < 0.48
# if LoyalCH is smaller than 0.48 we are then in the "yes" branch (left)
# here the split criterion is LoyalCH < 0.05
# if "yes" then we move to the right branch which has terminal node MM marked "4)"
# customers who has less then 48% loyalty to CH and also less then 5% loyalty to CH will
# end up choosing MM 98.5% of the time.

# (d)
plot(tree.oj)
text(tree.oj, pretty=0)
# LoyalCH is the most important variable of the tree, in fact top 3 nodes contain LoyalCH. If 
# LoyalCH<0.48, the tree predicts MM. If LoyalCH>0.76, the tree predicts CH.
# For intermediate values of LoyalCH, the decision also depends on the value of PriceDiff and ListPriceDiff.

# (e)
set.seed(345)
tree.pred <- predict(tree.oj, test, type = "class")
table(test$Purchase, tree.pred)
correct <- (124+86)/270
test.error <- 1-correct
# 77.8% of the test observations were correctly classified.
# Test error rate: 22.2 %

# (f)
set.seed(493282)
cv.oj <- cv.tree(tree.oj, FUN=prune.misclass)
cv.oj$size
# [1] 6 5 2 1
cv.oj$dev
# [1] 139 139 159 316
cv.oj$k
#[1]       -Inf   0.000000   6.333333 169.0000

# (g)
par(mfrow=c(1,1))
plot(cv.oj$size, cv.oj$dev, type = "b")

# (h)
# a tree with 5 or 6 nodes corresponds to the lowest cross-validation error rate.

# (i)
prune.oj <- prune.misclass(tree.oj, best = 5)
plot(prune.oj)
text(prune.oj, pretty = 0)

# (j)
summary(prune.oj)
# Training error rate: 0.1588 <- same training error as the unpruned tree.

# (k)
set.seed(3221)
tree.pred2 <- predict(prune.oj, test, type = "class")
table(test$Purchase, tree.pred2)
correct <- (124+86)/270
test.error <- 1-correct
# Same test error as before.

detach(OJ)

# Exercise 3

# (a)
x1 <- c(3,2,4,1,2,4,4)
x2 <- c(4,2,4,4,1,3,1)
y <- c("Red", "Red", "Red", "Red", "Blue", "Blue", "Blue")
plot(x1,x2, col=y)

# (b)
# ((2,2) + (2,1))/2 = (2,1.5)
# ((4,4) + (4,3))/2 = (4,3.5)
# a = 0.5
# b = (3.5 - 1.5)/(4-2) = 1
abline(-0.5, 1)

#(c) 0.5 + x1 + x2 > 0, b0=0.5, b1=1, b2=1

#(d)
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)

# (e)
arrows(2, 1, 2, 1.5)
arrows(2, 2, 2, 1.5)
arrows(4, 4, 4, 3.5)
arrows(4, 3, 4, 3.5)

# (g)
abline(-0.8, 1)
# 0.8 + x1 + x2 > 0

# (h)
points(c(4), c(2), col = c("red"))

# Exercise 7
library(ISLR)
attach(Auto)
dim(Auto)
names(Auto)

# (a)
med <- median(mpg)
new.var <- ifelse(mpg > med, 1, 0)
Auto$mpg.level <- as.factor(new.var)

# (b)
library(e1071)
set.seed(325)
svm.fit <- tune(svm, mpg.level ~ ., data = Auto, kernel = "linear", 
               ranges=list(cost=c(0.01, 0.1, 1, 5,10, 100)))
summary(svm.fit)
# We see that cost=1 results in the lowest cross-validation error rate. 

# (c)
svm.fit.rad <- tune(svm, mpg.level ~., data = Auto, kernel = "radial",
                 ranges=list(cost=c(0.1, 1, 5, 10), gamma=c(0.01, 0.1, 1, 5, 10, 100)))
summary(svm.fit.rad)
# cost = 5 and gamma=0.1 results in the lowest cross-validation error rate.
svm.fit.poly <- tune(svm, mpg.level ~., data = Auto, kernel = "polynomial",
                     ranges=list(cost=c(0.1, 1, 5, 10), degree=c(2, 3, 4)))
summary(svm.fit.poly)
# cost = 10 and degree = 2 results in the lowest cross-validation error rate.

# (d)
svm.linear = svm(mpg.level ~ ., data = Auto, kernel = "linear", cost = 1)
svm.radial = svm(mpg.level ~ ., data = Auto, kernel = "radial", cost = 5, gamma = 0.1)
svm.poly = svm(mpg.level ~ ., data = Auto, kernel = "polynomial", cost = 10, degree = 2)

plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }

  plotpairs(svm.linear)
  
  plotpairs(svm.poly)
  
  plotpairs(svm.radial)
}

detach(Auto)







