## Problem 1
mean1 = c(0.550,0.625)
mean1
cov1 = matrix(c(0.014,0.012,0.012,0.015), nrow = 2, ncol = 2)
cov1
library(mixtools)
library(matlib)
ellipse(mean1,cov1, alpha = .01, npoints = 200, newplot = TRUE, draw = TRUE, xlab = "x1-bar", ylab = "x2-bar")
abline(v = 0.550)
abline(h = 0.625)
value1 = c(0.56, 0.62)
dev1 = mean1 - value1
matrixdev1 = matrix(dev1, nrow = 2, ncol = 1)
matrixdev1
transposematrixdev1 = t(matrixdev1)
transposematrixdev1
inversecov1 = inv(cov1)
inversecov1
tsquared1 = 41 *transposematrixdev1 %*% inversecov1 %*% matrixdev1
qf(0.99, 2, 39)
(40*2)/39
tcrit1 = 2.051282 * qf(0.99,2,39)
tcrit1
tsquared1 < tcrit1
abline(v = 0.56, h = 0.62)
## Fail to reject null in favor of the alternative. 0.56,0.62 is a likely value 
## for set of numbers in dataset. Found within the confidence region from part a
?ellipse

## Problem 2: Exercise 5.7
table5.7 = read.csv(file.choose())
sweat = table5.7[,2]
sweat
sodium = table5.7[,3]
potassium = table5.7[,4]
table5.7123 = cbind(sweat,sodium,potassium)
table5.7123
matrix5.7 = matrix(c(sweat, sodium, potassium), nrow = 20, ncol = 3)
matrix5.7
xbar2 = matrix(c(mean(sweat), mean(sodium), mean(potassium)), nrow = 3, ncol = 1)
xbar2
cov2 = cov(matrix5.7)
cov2
qf(0.95, 3, 17)
(3*19)/17
tcrit2 = qf(0.95, 3, 17) * (3*19)/17
tcrit2
sqrttcrit = sqrt(tcrit2)
sqrttcrit
sqrtss1n = sqrt(2.879368/20)
sqrtss1n
mu1lower = 4.640 - (sqrttcrit * sqrtss1n)
mu1lower
mu1upper = 4.640 + (sqrttcrit * sqrtss1n)
mu1upper
## 3.398 less than/equal to mu1 less than equal to 5.882
sqrtss2n = sqrt(199.7884/20) 
sqrtss2n
mu2lower = 45.400 - (sqrttcrit * sqrtss2n)
mu2lower
mu2upper = 45.400 + (sqrttcrit * sqrtss2n)
mu2upper
## 35.052 less than/equal to mu2 less than equal to 55.748
sqrtss3n = sqrt(3.627658/20) 
sqrtss3n
mu3lower = 9.965 - (sqrttcrit * sqrtss3n)
mu3lower
mu3upper = 9.965 + (sqrttcrit * sqrtss3n)
mu3upper
## 8.571 less than/equal to mu3 less than equal to 11.359
tcritinternal = (0.05/(2*3))
tcrit0.05 = qt(1-tcritinternal, 17)
bonmu1lower = 4.640 - (tcrit0.05 * sqrtss1n)
bonmu1lower
bonmu1upper = 4.640 + (tcrit0.05 * sqrtss1n)
bonmu1upper
## 3.633 less than/equal to mu1 less than equal to 5.647
bonmu2lower = 45.400 - (tcrit0.05 * sqrtss2n)
bonmu2lower
bonmu2upper = 45.400 + (tcrit0.05 * sqrtss2n)
bonmu2upper
## 37.009 less than/equal to mu2 less than equal to 53.791
bonmu3lower = 9.965 - (tcrit0.05 * sqrtss3n)
bonmu3lower
bonmu3upper = 9.965 + (tcrit0.05 * sqrtss3n)
bonmu3upper
## 8.834 less than/equal to mu3 less than equal to 11.096
##Bonferonni intervals are smaller slightly than other ones
