## Problem 1: Exercise 8.1 amd 8.2
## Exercise 8.1
cov8.1 = matrix(c(5,2,2,2), nrow = 2, ncol = 2)
cov8.1
eigen8.1 = eigen(cov8.1)
eigen8.1
## Y1 = e1'X = -0.8944X1 - 0.4472X2
## Y2 = e2'X = 0.4472X1 - 0.8944X2
totalvar8.1 = 5+2
propfirst8.1 = 6/7
propfirst8.1
## first component accounts for 85.7% of variance
##Exercise 8.2
cor8.2 = cov2cor(cov8.1)
cor8.2
eigen8.2 = eigen(cor8.2)
eigen8.2
## Y1 = 0.7071(X1-mu1) + 0.7071(X2-mu2)
## Y2 = -0.7071(X1-mu1) + 0.7071(X2-mu2)
totalvar8.2 = eigen8.2$values[1]+eigen8.2$values[2]
totalvar8.2
propfirst8.2 = eigen8.2$values[1]/totalvar8.2
propfirst8.2
## first component accounts for 81.6% of variance
## standardized shows equivalent eigenvectors, but X1
## still accounts for over 80% of variance, but changed from
## 85% to about 82%
sqrteigen18.2 = sqrt(eigen8.2$values[1])
sqrteigen28.2 = sqrt(eigen8.2$values[2])
pY1z1 = 0.7071/sqrteigen18.2
pY1z1
pY1z2 = 0.7071/sqrteigen28.2
pY1z2
pY2z1 = -0.7071/sqrteigen18.2
pY2z1

## Problem 2: Exercise 8.6
## part a
xbar8.6 = matrix(c(155.60,14.70), nrow = 2, ncol = 1)
xbar8.6
cov8.6 = matrix(c(7476.45,303.62,303.62,26.19), nrow = 2, ncol = 2)
cov8.6
eigen8.6 = eigen(cov8.6)
eigen8.6
Y18.6values = matrix(c(eigen8.6$values[1],eigen8.6$values[2]), nrow = 2, ncol = 1)
Y18.6values
Y18.6vectors = matrix(c(eigen8.6$vectors[1,1],eigen8.6$vectors[2,1]), nrow = 2, ncol = 1)
Y18.6vectors
Y28.6vectors = matrix(c(eigen8.6$vectors[1,2],eigen8.6$vectors[2,2]), nrow = 2, ncol = 1)
Y28.6vectors
Y18.6 = t(xbar8.6) %*% Y18.6vectors
Y18.6
Y28.6 = t(xbar8.6) %*% Y28.6vectors
Y28.6
Y18.6var = eigen8.6$values[1]
Y18.6var
Y28.6var = eigen8.6$values[2]
Y28.6var
