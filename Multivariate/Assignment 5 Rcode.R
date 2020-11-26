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
pY1z1 = 0.7071*sqrteigen18.2
pY1z1
pY1z2 = 0.7071*sqrteigen28.2
pY1z2
pY2z1 = -0.7071*sqrteigen18.2
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
## part b
Y18.6var = eigen8.6$values[1]
Y18.6var
Y28.6var = eigen8.6$values[2]
Y28.6var
totvar8.6 = eigen8.6$values[1] + eigen8.6$values[2]
totvar8.6
propY1var8.6 = eigen8.6$values[1]/totvar8.6
propY1var8.6
## part d
sqrteigen18.6 = sqrt(eigen8.6$values[1])
sqrteigen18.6
sqrtss18.6 = sqrt(7476.45)
sqrtss28.6 = sqrt(26.19)
pY1z18.6 = (eigen8.6$vectors[1,1]*sqrteigen18.6)/sqrtss18.6
pY1z18.6
pY1z28.6 = (eigen8.6$vectors[2,1]*sqrteigen18.6)/sqrtss28.6
pY1z28.6

## Sales dominates the first principal component.


## Problem 3: Exercise 8.7
## part a
xbar8.7=xbar8.6
cor8.7 = cov2cor(cov8.6)
cor8.7
eigen8.7= eigen(cor8.7)
eigen8.7
Y18.7values = matrix(c(eigen8.7$values[1],eigen8.7$values[2]), nrow = 2, ncol = 1)
Y18.7values
Y18.7vectors = matrix(c(eigen8.7$vectors[1,1],eigen8.7$vectors[2,1]), nrow = 2, ncol = 1)
Y18.7vectors
Y28.7vectors = matrix(c(eigen8.7$vectors[1,2],eigen8.7$vectors[2,2]), nrow = 2, ncol = 1)
Y28.7vectors
Y18.7 = t(xbar8.7) %*% Y18.7vectors
Y18.7
Y28.7 = t(xbar8.7) %*% Y28.7vectors
Y28.7
## part b
Y18.7var = eigen8.7$values[1]
Y18.7var
Y28.7var = eigen8.7$values[2]
Y28.7var
totvar8.7 = eigen8.7$values[1] + eigen8.7$values[2]
totvar8.7
propY1var8.7 = eigen8.7$values[1]/totvar8.7
propY1var8.7
## part c
sqrteigencor18.7 = sqrt(eigen8.7$values[1])
sqrteigencor18.7
pY1z18.7 = eigen8.7$values[1]*sqrteigencor18.7
pY1z18.7
pY1z28.7 = eigen8.7$values[2]*sqrteigencor18.7
pY1z28.7
## part d
Y18.6
Y28.6
Y18.7
Y28.7
## Because the ranges are very different for the two 
## variables, standardization is probably the best option

## Problem 4: Exercise 8.10
## part a
data8.10 = read.csv(file.choose())
cov8.10 = cov(data8.10)
cov8.10
eigen8.10 = eigen(cov8.10)
eigen8.10
xbar8.10 = matrix(c(0.0011,0.0007,0.0016,0.0040,0.0040), nrow = 5, ncol = 1)
Y18.10values = matrix(c(eigen8.10$values[1],eigen8.10$values[2], eigen8.10$values[3], eigen8.10$values[4], eigen8.10$values[5]), nrow = 5, ncol = 1)
Y18.10values
Y18.10vectors = matrix(c(eigen8.10$vectors[1,1],eigen8.10$vectors[2,1], eigen8.10$vectors[3,1], eigen8.10$vectors[4,1], eigen8.10$vectors[5,1]), nrow = 5, ncol = 1)
Y18.10vectors
Y28.10vectors = matrix(c(eigen8.10$vectors[1,2],eigen8.10$vectors[2,2], eigen8.10$vectors[3,2], eigen8.10$vectors[4,2], eigen8.10$vectors[5,2]), nrow = 5, ncol = 1)
Y28.10vectors
Y38.10vectors = matrix(c(eigen8.10$vectors[1,3],eigen8.10$vectors[2,3], eigen8.10$vectors[3,3], eigen8.10$vectors[4,3], eigen8.10$vectors[5,3]), nrow = 5, ncol = 1)
Y38.10vectors
Y48.10vectors = matrix(c(eigen8.10$vectors[1,4],eigen8.10$vectors[2,4], eigen8.10$vectors[3,4], eigen8.10$vectors[4,4], eigen8.10$vectors[5,4]), nrow = 5, ncol = 1)
Y48.10vectors
Y58.10vectors = matrix(c(eigen8.10$vectors[1,5],eigen8.10$vectors[2,5], eigen8.10$vectors[3,5], eigen8.10$vectors[4,5], eigen8.10$vectors[5,5]), nrow = 5, ncol = 1)
Y58.10vectors
Y18.10 = t(xbar8.10) %*% Y18.10vectors
Y18.10
Y28.10 = t(xbar8.10) %*% Y28.10vectors
Y28.10
Y38.10 = t(xbar8.10) %*% Y38.10vectors
Y38.10
Y48.10 = t(xbar8.10) %*% Y48.10vectors
Y48.10
Y58.10 = t(xbar8.10) %*% Y58.10vectors
Y58.10
PC8.10 = matrix(c(Y18.10,Y28.10,Y38.10,Y48.10,Y58.10), nrow = 5, ncol = 1)
PC8.10
## part b
totvar8.10 = eigen8.10$values[1] + eigen8.10$values[2] + eigen8.10$values[3] + eigen8.10$values[4] + eigen8.10$values[5]
totvar8.10
propY1var8.10 = eigen8.10$values[1]/totvar8.10
propY1var8.10
propY2var8.10 = eigen8.10$values[2]/totvar8.10
propY2var8.10
propY3var8.10 = eigen8.10$values[3]/totvar8.10
propY3var8.10
propvar8.10 = matrix(c(propY1var8.10,propY2var8.10,propY3var8.10), nrow = 3, ncol = 1)
propvar8.10
## part c
z8.10 = -qnorm((1-.10)/(2*5))
z8.10
den8.10c = 1+(z8.10*sqrt(2/103))
num18.10c = eigen8.10$values[1]/den8.10c
bonlower18.10 = eigen8.10$values[1] - num18.10c 
bonupper18.10 = eigen8.10$values[1] + num18.10c 
bonlower18.10
bonupper18.10
num28.10c = eigen8.10$values[2]/den8.10c
bonlower28.10 = eigen8.10$values[2] - num28.10c 
bonupper28.10 = eigen8.10$values[2] + num28.10c 
bonlower28.10
bonupper28.10
num38.10c = eigen8.10$values[3]/den8.10c
bonlower38.10 = eigen8.10$values[3] - num38.10c 
bonupper38.10 = eigen8.10$values[3] + num38.10c 
bonlower38.10
bonupper38.10
## First three components explains 90% of variance, and other two
## eigenvalues are very close to 0 so may not need
## any other components to be retained 

## Problem 5: Exercise 8.13
## part a
data8.13 = read.csv(file.choose())
cov8.13 = cov(data8.13)
cov8.13
cor8.13 = cov2cor(cov8.13)
cor8.13
## part b 
## different ranges for variables, so using correlation 
eigencor8.13 = eigen(cor8.13)
eigencor8.13
vartot8.13 = eigencor8.13$values[1]+ eigencor8.13$values[2] + eigencor8.13$values[3] + eigencor8.13$values[4] + eigencor8.13$values[5] + eigencor8.13$values[6]
vartot8.13
prop18.13 = eigencor8.13$values[1]/vartot8.13
prop28.13 = eigencor8.13$values[2]/vartot8.13
prop38.13 = eigencor8.13$values[3]/vartot8.13
prop48.13 = eigencor8.13$values[4]/vartot8.13
prop58.13 = eigencor8.13$values[5]/vartot8.13
prop68.13 = eigencor8.13$values[6]/vartot8.13
prop18.13
prop28.13
prop38.13
prop48.13
prop = c(prop18.13,prop28.13,prop38.13,prop48.13,prop58.13,prop68.13)
prop
value = c("# symp", "activity", "sleep", "food", "appetite", "skin")
table8.13b = matrix(c(prop,value), nrow = 6, ncol = 2)
table8.13b
## part c
## need 4 components to account for 90% of variance. Can't
## summarize with single component because the single component
## with the highest variance is only 47%, not enough to explain
##part 4
col18.13 = matrix(eigencor8.13$vectors[,1], nrow = 6, ncol = 1)
col18.13
col28.13 = matrix(eigencor8.13$vectors[,2], nrow = 6, ncol = 1)
col38.13 = matrix(eigencor8.13$vectors[,3], nrow = 6, ncol = 1)
col48.13 = matrix(eigencor8.13$vectors[,4], nrow = 6, ncol = 1)
sqrteigen18.13 = sqrt(eigencor8.13$values[1])
pY1z1 = eigencor8.13$vectors[1,1] * sqrteigen18.13
pY1z2 = eigencor8.13$vectors[2,1] * sqrteigen18.13
pY1z3 = eigencor8.13$vectors[3,1] * sqrteigen18.13
pY1z4 = eigencor8.13$vectors[4,1] * sqrteigen18.13
pY1z5 = eigencor8.13$vectors[5,1] * sqrteigen18.13
pY1z6 = eigencor8.13$vectors[6,1] * sqrteigen18.13
pY1 = matrix(c(pY1z1,pY1z2,pY1z3,pY1z4,pY1z5,pY1z6), nrow = 6, ncol = 1)
pY1
sqrteigen28.13 = sqrt(eigencor8.13$values[2])
pY2z1 = eigencor8.13$vectors[1,2] * sqrteigen28.13
pY2z2 = eigencor8.13$vectors[2,2] * sqrteigen28.13
pY2z3 = eigencor8.13$vectors[3,2] * sqrteigen28.13
pY2z4 = eigencor8.13$vectors[4,2] * sqrteigen28.13
pY2z5 = eigencor8.13$vectors[5,2] * sqrteigen28.13
pY2z6 = eigencor8.13$vectors[6,2] * sqrteigen28.13
pY2 = matrix(c(pY2z1,pY2z2,pY2z3,pY2z4,pY2z5,pY2z6), nrow = 6, ncol = 1)
pY2
sqrteigen38.13 = sqrt(eigencor8.13$values[3])
pY3z1 = eigencor8.13$vectors[1,3] * sqrteigen38.13
pY3z2 = eigencor8.13$vectors[2,3] * sqrteigen38.13
pY3z3 = eigencor8.13$vectors[3,3] * sqrteigen38.13
pY3z4 = eigencor8.13$vectors[4,3] * sqrteigen38.13
pY3z5 = eigencor8.13$vectors[5,3] * sqrteigen38.13
pY3z6 = eigencor8.13$vectors[6,3] * sqrteigen38.13
pY3 = matrix(c(pY3z1,pY3z2,pY3z3,pY3z4,pY3z5,pY3z6), nrow = 6, ncol = 1)
pY3
sqrteigen48.13 = sqrt(eigencor8.13$values[4])
pY4z1 = eigencor8.13$vectors[1,4] * sqrteigen48.13
pY4z2 = eigencor8.13$vectors[2,4] * sqrteigen48.13
pY4z3 = eigencor8.13$vectors[3,4] * sqrteigen48.13
pY4z4 = eigencor8.13$vectors[4,4] * sqrteigen48.13
pY4z5 = eigencor8.13$vectors[5,4] * sqrteigen48.13
pY4z6 = eigencor8.13$vectors[6,4] * sqrteigen48.13
pY4 = matrix(c(pY4z1,pY4z2,pY4z3,pY4z4,pY4z5,pY4z6), nrow = 6, ncol = 1)
pY4
matrix8.13d = matrix(c(pY1,pY2,pY3,pY4), nrow = 6, ncol = 4)
matrix8.13d
## pc1, variables contribute equally. pc2, skin contributes most
## pc3, sleep,skin and activity contribute the most.
## pc4, number of symptoms, sleep and food contribute the most