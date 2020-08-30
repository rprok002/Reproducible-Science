## Problem 1.2
table1= read.csv(file.choose())
x1 <- table1[,1]
x2 <- table1[,2]
plot(x1,x2)
mean(x1)
mean(x2)
var(x1)
var(x2)
cov(x1,x2)
cov(x2,x1)
cor(x1,x2)
vector1 <- c(mean(x1), mean(x2))
meanarray <- array(vector1, dim= c(2,1))
meanarray
print(meanarray)

##Problem 1.9
table2= read.csv(file.choose())
x1b <- table2[,1]
x2b <- table2[,2]
plot(x1b, x2b)
var(x1b)
var(x2b)
cov(x1b,x2b)
x1brotated <- x1b*cos(26)+x2b*sin(26)
x2brotated <- -x1b*sin(26)+x2b*cos(26)
table3 <- data.frame(x1brotated,x2brotated)
print(table3)
var(x1brotated)
var(x2brotated)
