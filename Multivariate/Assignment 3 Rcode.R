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

## Problem 3: Exercise 5.9 
## part a
sample5.9 = matrix(c(95.52,164.38,55.69,93.39,17.98,31.13), nrow = 6, ncol = 1)
sample5.9
cov5.9 = matrix(c(3266.46,1343.97,731.54,1175.50,162.68,238.37,1343.97,721.91,324.25,537.35,80.17,117.73,731.54,324.25,179.28,281.17,39.15,56.80,1175.50,537.35,281.17,474.98,63.73,94.85,162.68,80.17,39.15,63.73,9.95,13.88,
                  238.37,117.73,56.80,94.85,13.88,21.26), nrow = 6, ncol = 6)
cov5.9
sqrtchisquare5.9 = sqrt(qchisq(0.95,6))
sqrtchisquare5.9
sqrts11n = sqrt(3266.46/61)
sqrts22n = sqrt(721.91/61)
sqrts33n = sqrt(179.28/61)
sqrts44n = sqrt(474.98/61)
sqrts55n = sqrt(9.95/61)
sqrts66n = sqrt(21.26/61)
simmatrix5.9 = matrix(c(sqrtchisquare5.9*sqrts11n,sqrtchisquare5.9*sqrts22n,sqrtchisquare5.9*sqrts33n,sqrtchisquare5.9*sqrts44n,sqrtchisquare5.9*sqrts55n,sqrtchisquare5.9*sqrts66n), nrow = 6, ncol = 1)
simmatrix5.9
lower5.9 = sample5.9 - simmatrix5.9
lower5.9
upper5.9 = sample5.9 + simmatrix5.9
upper5.9
## parb b don't know yet
ellipse(sample5.9b,cov5.9b, alpha = 0.05, newplot = TRUE, xlab = "Weight", ylab = "Girth")
## part c
tcrit5.9internal = (0.05/(2*6))
tcrit5.9internal
tcrit5.9 = qt(1-tcrit5.9internal, 55)
tcrit5.9
bonmatrix5.9 = matrix(c(tcrit5.9*sqrts11n,tcrit5.9*sqrts22n,tcrit5.9*sqrts33n,tcrit5.9*sqrts44n,tcrit5.9*sqrts55n,tcrit5.9*sqrts66n), nrow = 6, ncol = 1)
bonmatrix5.9
bonlower5.9 = sample5.9 - bonmatrix5.9
bonlower5.9
bonupper5.9 = sample5.9 + bonmatrix5.9
bonupper5.9
## part d don't know yet
## part e
widthminuslength = 31.13 - 17.98
tcrit5.9einternal = (0.05/(2*7))
tcrit5.9e = qt(1-tcrit5.9einternal, 60 )
tcrit5.9e
sqrt12n = sqrt((9.95-13.88-13.88+21.26)/61)
sqrt12n
coninterval5.9e = tcrit5.9e * sqrt12n
lower5.9e = widthminuslength - coninterval5.9e
lower5.9e
upper5.9e = widthminuslength + coninterval5.9e
upper5.9e
## interval is 12.488 is less than/equal to width minus length is less than/equal to 13.812