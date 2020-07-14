# week 7

# Exercise 10
# (a)
set.seed(5)
n <- 1000
p <- 20
x <- matrix(rnorm(n*p), n, p)
beta <- rnorm(p)
beta[3]=0
beta[10]=0
beta[12]=0
beta[19]=0
epsilon <- rnorm(p)
y <- x %*% beta+epsilon

# (b)
training.sample <- sample(seq(n), 100, replace = FALSE)
y.train <- y[training.sample, ]
y.test <- y[-training.sample, ]
x.train <- x[training.sample, ]
x.test <- x[-training.sample, ]


# (c)
# perform best subset selection on the training set
# plot the training set MSE associated with the best model of each size
library(leaps)

regfit.full <- regsubsets(y ~ ., data = data.frame(x=x.train, y=y.train), 
                          nvmax = p)
x.cols <- colnames(x, do.NULL = FALSE, prefix = "x.")
val.errors <- rep(NA, p)
for (i in 1:p){
  coefi <- coef(regfit.full, id=i)
  pred <- as.matrix(x.train[, x.cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x.cols]
  val.errors[i] <- mean((y.train - pred)^2)
}
val.errors

plot(val.errors, ylab = "Training MSE", pch = 19, type = "b")

# (d)
val.errors.test <- rep(NA, p)
for (i in 1:p) {
  coefi <- coef(regfit.full, id=i)
  pred <- as.matrix(x.test[, x.cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x.cols]
  val.errors.test[i] <- mean((y.test - pred)^2)
}
val.errors.test
plot(val.errors.test, ylab="Test MSE", pch=19, type = "b")

# (e)
which.min(val.errors.test)

# (f)
betaj <- coef(regfit.full, id=12)
# should have caught x.1, x.7, x.16 and x.20

# (g)
val.errors.beta <- rep(NA, p)
a <- rep(NA, p)
b <- rep(NA, p)
for (i in 1:p) {
  coefi <- coef(regfit.full, id=i)
  a[i] <- length(coefi)-1
  b[i] <- sqrt(sum((beta[x.cols %in% names(coefi)] - coefi[names(coefi) %in% x.cols])^2) +
                 sum(beta[!(x.cols %in% names(coefi))])^2)
}

plot(x = a, y = b, xlab = "number of coefficients", 
     ylab = "error between estimated and true coefficients")
which.min(b)

# Exersice 11
library(MASS)
attach(Boston)
names(Boston)
summary(Boston)
dim(Boston)

# Best subset selection
library(leaps)
regfit.full <- regsubsets(medv ~., Boston)
summary(regfit.full)
# this indicates that the best two-variable model contains only lstat and rm
regfit.full <- regsubsets(medv ~., Boston, nvmax = 13)
reg.summary <- summary(regfit.full)
reg.summary$rsq
which.max(reg.summary$rsq)
# the model with the largest Rsqr is model nr13
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab="RSS", type = "l")
which.min(reg.summary$rss)
points(13, reg.summary$rss[13], col="red", cex=2, pch=20)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab="cp", type = "l")
which.min(reg.summary$cp)
points(11, reg.summary$cp[11], col="red", cex=2, pch=20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab="BIC", type = "l")
which.min(reg.summary$bic)
points(11, reg.summary$bic[11], col="red", cex=2, pch=20)
# we would choose the model with 11 variables, as there little evidence of improvement
# to the RSS of the model after 11 variables model.

# Forward stepwise selection
regfit.fwd <- regsubsets(medv ~., data = Boston, nvmax=13, method = "forward")
fwd.summary <- summary(regfit.fwd)
fwd.summary$rsq
which.max(fwd.summary$rsq)
par(mfrow=c(1,1))
plot(fwd.summary$rsq, xlab = "Number of Variables", ylab = "RSq", type="l")
points(13, fwd.summary$rsq[13], col="red", cex=2, pch=20)
points(11, fwd.summary$rsq[11], col="red", cex=2, pch=20)
# almost a straight line between the two points
# Hence there is little evidence of improvement from a 11 variable model to a 13 variable model
par(mfrow=c(2,2))
plot(fwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
which.min(fwd.summary$rss)
points(13, fwd.summary$rss[13], col="red", cex=2, pch=20)
plot(fwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type="l")
which.max(fwd.summary$adjr2)
points(11, fwd.summary$adjr2[11], col="red", cex=2, pch=20)
plot(fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(fwd.summary$cp)
points(11, fwd.summary$cp[11], col="red", cex=2, pch=20)
plot(fwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(fwd.summary$bic)
points(11, fwd.summary$bic[11], col="red", cex=2, pch=20)
# chooses the same 11-variable model

# Backwards stepwise selection
regfit.bwd <- regsubsets(medv ~., data=Boston, nvmax = 13, method = "backward")
bwd.summary <- summary(regfit.bwd)
bwd.summary$rsq
which.max(bwd.summary$rsq)
par(mfrow=c(2,2))
plot(bwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
which.min(bwd.summary$rss)
points(13, bwd.summary$rss[13], col="red", cex=2, pch=20)
plot(bwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(bwd.summary$adjr2)
points(11, bwd.summary$adjr2[11], col="red", cex=2, pch=20)
plot(bwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(bwd.summary$cp)
points(11, bwd.summary$cp[11], col="red", cex=2, pch=20)
plot(bwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(bwd.summary$bic)
points(11, bwd.summary$bic[11], col="red", cex=2, pch=20)
# we also choose the 11-variable model with backwards selection

# validation set approach 
set.seed(6)
train <- sample(c(TRUE, FALSE), nrow(Boston), rep=TRUE)
test <- (!train)
regfit.best <- regsubsets(medv ~., data = Boston[train,], nvmax = 13)
test.mat <- model.matrix(medv ~., data = Boston[test,])
val.errors <- rep(NA, 13)
for (i in 1:13) {
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Boston$medv[test]-pred)^2)
}
val.errors
which.min(val.errors)
# The model with the lowest test MSE is the model that contains 10 variables


# creating our own prediction function (there is no predict() method for regsubsets())
predict.regsubsets = function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# Cross-validation
k=10
set.seed(8)
folds <- sample(1:k, nrow(Boston), replace = TRUE)
cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))
for (j in 1:k) {
  best.fit <- regsubsets(medv ~., data = Boston[folds!=j,], nvmax = 13)
  for (i in 1:13) {
    pred <- predict(best.fit, Boston[folds==j,], id=i)
    cv.errors[j, i] <- mean((Boston$medv[folds==j]-pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type = "b")

# Would choose a model with 11 variables
# but there is not much improvement in the MSE after a model with 7 variables
# Infact the MSE starts to increase after 7 and decreases again.
# The 7 variable model might also be a better choise because of less complexity
# and a model which might be easier to interperate.

# Ridge regression
x <- model.matrix(medv ~., Boston)[,-1]
y <- Boston$medv
library(glmnet)
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
set.seed(9)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s=bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)
# best lambda = 0.67 with a MSE=24.26, performed on the full model
# ridge regression does not perform variable selection

# Lasso 
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(10)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)
# best lambda = 0.04, MSE = 31.05 
out <- glmnet(x,y,alpha = 1,lambda = grid)
lasso.coef <- predict(out, type="coefficients", s=beatlam)[1:14]
lasso.coef
lasso.coef[lasso.coef!=0]
# Hence the model used in the lasso uses 11 variables

# PCR
library(pls)
set.seed(11)
pcr.fit <- pcr(medv ~., data=Boston, scale=TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
# Smallest cross-validation error occurs when M=13
set.seed(12)
pcr.fit <- pcr(medv ~., data=Boston, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")
# Smallest cross-validation error when M=10
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 10)
mean((pcr.pred - y.test)^2)
# MSE = 27.89
pcr.fit <- pcr(y ~ x, scale=TRUE, ncomp=10)
summary(pcr.fit)

# Partial least squares
set.seed(13)
pls.fit <- plsr(medv ~ ., data=Boston, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
# Lowest cross-validation error occurs when M=2
pls.pred <- predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)
# MSE = 26.64
pls.fit <- plsr(medv ~., data=Boston, scale=TRUE, ncomp=2)
summary(pls.fit)
