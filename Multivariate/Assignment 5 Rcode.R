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

## Problem 6: Exercise 8.15
sd18.15 = 32.9909
sd28.15 = 33.5918
sd38.15 = 36.5534
sd48.15 = 37.3517
cor8.15 = matrix(c(1,0.7501,0.6329,0.6363,0.7501,1,0.6925,0.7386,0.6329,0.6925,1,0.6625,0.6363,0.7386,0.6625,1), nrow = 4, ncol = 4)
cor8.15
sd8.15 = c(sd18.15,sd28.15,sd38.15,sd48.15)
sd8.15
sd8.15diag = diag(sd8.15)
sd8.15diag
S8.15 = sd8.15diag %*% cor8.15 %*% sd8.15diag
S8.15
eigencor8.15 = eigen(S8.15)
eigencor8.15
data8.12 = read.csv(file.choose())
cov8.12 = cov(data8.12)
cov8.12
cor8.12 = cor(data8.12)
cor8.12
eigencov8.12 =  eigen(cov8.12)
eigencov8.12
eigencor8.12 = eigen(cor8.12)
eigencor8.12

## Problem 7: Exercise 8.20
## part a
data8.20 = read.csv(file.choose())
data8.20rev = data.frame("100 m (s)" = data8.20[,2], "200 m (s)" = data8.20[,3], "400 m (s)" = data8.20[,4], "800 m (min)" = data8.20[,5], "1500 m (min)" = data8.20[,6], "5000 m (min)" = data8.20[,7], "10,000 m (min)" = data8.20[,8], "marathon" = data8.20[,9])
data8.20rev
cov8.20 = cov(data8.20rev)
cov8.20
cor8.20 = cor(data8.20rev)
cor8.20
eigencov8.20 = eigen(cov8.20)
eigencov8.20
eigencor8.20 = eigen(cor8.20)
eigencor8.20
princomp8.20 = princomp(data8.20rev, cor=T)
princomp8.20
summary(data8.20rev)
## part b
## Y1 = -0.3324z1 -0.3461z2 -0.3391z3 -0.3531z4 - 0.3660z5 - 0.3698z6 - 0.3659z7 - 0.3543z8
## repeat process for second component
corvector18.20 = eigencor8.20$vectors[,1]
corvector18.20
sqrtcorval1 = sqrt(eigencor8.20$values[1])
sqrtcorval1
Y1cor8.20 = -corvector18.20 * sqrtcorval1
Y1cor8.20
corvector28.20 = eigencor8.20$vectors[,2]
corvector28.20
sqrtcorval2 = sqrt(eigencor8.20$values[2])
sqrtcorval2
Y2cor8.20 = -corvector28.20 * sqrtcorval2
Y2cor8.20
sampvar18.20b = eigencor8.20$values[1]/sum(eigencor8.20$values)
sampvar18.20b
sampvar28.20b = eigencor8.20$values[2]/sum(eigencor8.20$values)
sampvar28.20b
cumsampvar8.20b = sampvar18.20b + sampvar28.20b
cumsampvar8.20b
## part c
## athletic excellence based on nation accounts for 84% of sample variance.
## rel. strength of nation at various running distances only accounts for 7%
## of the total variance, so it explains much less of the data than
## the component based purely on the nation. Based on the component, 
## all events contributed relatively equally to the first component. 

## part d
z18.20d = (data8.20rev[,1]-mean(data8.20rev[,1]))/sqrt(var(data8.20rev[,1]))
z28.20d = (data8.20rev[,2]-mean(data8.20rev[,2]))/sqrt(var(data8.20rev[,2]))
z38.20d = (data8.20rev[,3]-mean(data8.20rev[,3]))/sqrt(var(data8.20rev[,3]))
z48.20d = (data8.20rev[,4]-mean(data8.20rev[,4]))/sqrt(var(data8.20rev[,4]))
z58.20d = (data8.20rev[,5]-mean(data8.20rev[,5]))/sqrt(var(data8.20rev[,5]))
z68.20d = (data8.20rev[,6]-mean(data8.20rev[,6]))/sqrt(var(data8.20rev[,6]))
z78.20d = (data8.20rev[,7]-mean(data8.20rev[,7]))/sqrt(var(data8.20rev[,7]))
z88.20d = (data8.20rev[,8]-mean(data8.20rev[,8]))/sqrt(var(data8.20rev[,8]))
zz8.20d = cbind(z18.20d,z28.20d,z38.20d,z48.20d,z58.20d,z68.20d,z78.20d,z88.20d)
corvec18.20matrix = matrix(eigencor8.20$vectors[,1], nrow = 8, ncol = 1)
corvec18.20matrix
Y1hat8.20b = zz8.20d %*% corvec18.20matrix
rank(Y1hat8.20b)
rank(princomp8.20$scores[1:54,1])
## list is: USA, GB, Kenya, France, Australia, Italy, Brazil, Germany, Portugal,
## Canada. Differs from female rankings

## Problem 8 :Exercise 8.23
mean8.23 = matrix(c(95.52,164.38,55.69,93.39,17.98,31.13), nrow = 6, ncol = 1)
mean8.23
cov8.23 = matrix(c(3266.46,1343.97,731.54,1175.50,162.68,238.37,1343.97,721.91,324.25,537.35,80.17,117.73,731.54,324.25,179.28,281.17,39.15,56.80,
                   1175.50,537.35,281.17,474.98,63.73,94.85,162.68,80.17,39.15,63.73,9.95,13.88,238.37,117.73,56.80,94.85,13.88,21.26), nrow = 6, ncol = 6)
cov8.23
eigencov8.23 = eigen(cov8.23)
eigencov8.23
sampvar18.23 = eigencov8.23$values[1]/sum(eigencov8.23$values)
sampvar18.23
sampvar28.23 = eigencov8.23$values[2]/sum(eigencov8.23$values)
sampvar28.23
sampvar38.23 = eigencov8.23$values[3]/sum(eigencov8.23$values)
sampvar38.23
sqrtval8.23 = sqrt(eigencov8.23$values)
sqrtval8.23
covvec18.23 = eigencov8.23$vectors[,1]
var8.23 = c(3266.36,721.91,179.28,474.98,9.95,21.26)
sqrtvar8.23 = sqrt(var8.23)
sqrtval8.23
coeff8.23 = (sqrtval8.23 * covvec18.23)/sqrtval8.23
coeff8.23
## Component 1 accounts for 96% of variation and coefficient is much higher for
## first component than any other, so can be summarized using first component only.