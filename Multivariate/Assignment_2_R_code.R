## Problem 1: Exercise 3.2
data1 = read.csv(file.choose())
p1 = data1[,1]
p2 = data1[,2]
plot(p1,p2)
meanp1 = mean(p1)
meanp2 = mean(p2)
data1 = read.csv(file.choose())
p1 = data1[,1]
p2 = data1[,2]
plot(p1,p2)
invcosangle = acos(-0.866)
degreesangle = invcosangle*(180/3.1415)
matrixSn3.2 = matrix(c(2,-3,-3,6), nrow = 2, ncol = 2)
matrixSn3.2
corrcoeff3.2 = matrix(c(1,-0.86,-0.86,1), nrow = 2, ncol = 2)
corrcoeff3.2

## Problem 2: 
meanmatrix = matrix(c(12,14,10), nrow = 3, ncol = 1)
meanmatrix
mean(meanmatrix)
cormatrix = matrix(c(20,-80,140,-80,40,60,140,60,100), nrow = 3, ncol = 3)
cormatrix
cormatrix*(1/3)
