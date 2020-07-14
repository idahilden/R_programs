# week 11

# Exercise 10 section 4
library(ISLR)
attach(Weekly)

#(a)
dim(Weekly)
names(Weekly)
summary(Weekly)
cor(Weekly[-9])
# good correlation between Volume and Year variables
# the correlation between the Lag variables and today's return is close to zero
attach(Weekly)
plot(Volume)
# Volume is increasing over time, the average number of shares traded weekly
#from 1990 to 2010

# (b)
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Weekly, family = binomial)
summary(glm.fit)
# AIC: 1500.4
# The Lag2 variable is the only response variable which is statistically significant
# with a p-value<0.05

correct <- list() 
# (c)
glm.probs <- predict(glm.fit, type="response")
contrasts(Direction)
glm.pred <- rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
confusion.matrix <- table(glm.pred, Direction)
confusion.matrix
correct$full <- mean(glm.pred==Direction)
# The logistic regression correctly predicted the movement of the market 56.1% of the time.
training.error <- 1 - correct.pred
training.error
# The training error rate is 43.89% , which is not that good, as training errors
# usually are overly optimistic. 

# (d)
train <- (Year < 2009)
Weekly.2009 <- Weekly[!train,]
dim(Weekly.2009)
Direction.2009 <- Direction[!train]
glm.fit <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Weekly.2009, type = "response")
glm.pred <- rep("Down", 104)
glm.pred[glm.probs>.5]="Up"
confusion.matrix <- table(glm.pred, Direction.2009)
confusion.matrix
correct$glm <- mean(glm.pred==Direction.2009)
# The logistic regression correctly predicted the movement of the market 62.5% of the time.
training.error <- 1 - correct.pred
training.error
# The training error rate is 37.5%

# (e)
# LDA
library(MASS)
lda.fit <- lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.probs <- predict(lda.fit, Weekly.2009)
table(lda.probs$class, Direction.2009)
correct$lda<- mean(lda.probs$class==Direction.2009)

# The LDA model make correct predictions 62.5% of the time.
# 9/104 correctly when the market goes down
# 56/104 correctly when the market goes up

# (f)
# QDA
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.class <- predict(qda.fit, Weekly.2009)$class
table(qda.class, Direction.2009)
correct$qda <- mean(qda.class==Direction.2009)
# QDA model makes correct predctions 58.7% of the time.
# But it always chooses "Up"

# (g)
library(class)
train.x <- as.matrix(Lag2[train])
test.x <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]
set.seed(5432)
knn.pred <- knn(train.x, test.x, train.Direction, k=1)
table(knn.pred, Direction.2009)
correct$knn <- mean(knn.pred==Direction.2009)

# (h)
print(correct)

# THe model that predicts correctly most of the time is the GLM model 
# with only one predictor and the LDA model with 62.5% of the predictions 
# correct. We will therefore choose one of these models.






















