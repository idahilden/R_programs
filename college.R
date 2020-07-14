# Exercise 8 in section 2.4 in ISL book
college <- read.csv("College.csv", header = T)
rownames(college) <- college[,1]
college <- college[,-1]
summary(college)
pairs(college [,2:18])  #this one takes a while to compute
attach(college)
private <- as.factor(Private)
plot(private, Outstate, xlab="Private", ylab="Outstate")   #Boxplot

Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)      # 78 students are at an elite school
plot(Elite, Outstate, xlab="Elite", ylab="Outstate")

par(mfrow=c(2,2))
hist(Grad.Rate)
hist(PhD)
hist(Expend)
hist(Books)
