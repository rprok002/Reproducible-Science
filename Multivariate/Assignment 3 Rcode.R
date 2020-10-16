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
