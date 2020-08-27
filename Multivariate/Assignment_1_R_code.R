## Problem 1.2
table1= read.csv(file.choose())
x1 <- table1[,1]
x2 <- table1[,2]
plot(x1,x2)
mean(x1)
mean(x2)
var(x1)
var(x2)
cor(x1,x2)
