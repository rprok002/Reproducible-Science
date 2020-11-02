## Problem 1: 6.2
table6.2 = read.csv(file.choose())
d16.2 = table6.2[,1] - table6.2[,3]
d16.2
meand16.2 = mean(d16.2)
meand16.2
d26.2 = table6.2[,2] - table6.2[,4]
d26.2
meand26.2 = mean(d26.2)
meand26.2
meanmatrix6.2 = matrix(c(meand16.2,meand26.2), nrow = 2, ncol = 1)
diff6.2 = matrix(c(d16.2,d26.2), nrow = 11, ncol = 2)
diff6.2
sddiff6.2 = cov(diff6.2)
sddiff6.2
sd16.2 = 199.25455
sd26.2 = 418.61818
sqrtsd16.2n = sqrt(sd16.2/11)
sqrtsd26.2n = sqrt(sd26.2/11)
qt6.2 = -qt(((1-.05)/4),10)
qt6.2
sdcrit1 = sqrtsd16.2n * qt6.2
sdcrit2 = sqrtsd26.2n * qt6.2
sdcritmatrix6.2 = matrix(c(sdcrit1,sdcrit2), nrow = 2, ncol = 1)
bonlower6.2 = meanmatrix6.2 - sdcritmatrix6.2
bonlower6.2
bonupper6.2 = meanmatrix6.2 + sdcritmatrix6.2
bonupper6.2
## intervals for simultaneious are -22.46 to 3.74 and -5.71 to 32.25
## Bonferroni are much smaller intervals and don't cover 0

## Problem 2: Exercise 6.5
## part a
mean6.5 = matrix(c(46.1,57.3,50.4), nrow = 3, ncol = 1)
mean6.5
cov6.5 = matrix(c(101.3,63.0,71.0,63.0,80.2,55.6,71.0,55.6,97.4), nrow = 3, ncol = 3)
cov6.5
contrast6.5 = matrix(c(1,-1,0,1,-1,0), nrow = 2, ncol = 3)
contrast6.5
contrastmean6.5 = contrast6.5 %*% mean6.5
contrastmean6.5
tcontrastmean6.5 = t(contrastmean6.5)
tcontrast6.5 = t(contrast6.5)
CSC6.5 = solve(contrast6.5 %*% cov6.5 %*% tcontrast6.5)
CSC
tsquared6.5 = 40 * tcontrastmean6.5 %*% CSC6.5 %*% contrastmean6.5
tsquared6.5
qf6.5 = qf(0.95, 2,38)
qf6.5
m6.5 = ((40-1)*(3-1)/(40-3+1))
tcrit6.5 = m6.5 * qf6.5
tcrit6.5
## Tsquared greater than tcrit, so reject null hypothesis that
## Cu = 0, that there are no treatment effects. Accept alternative 
## that Cu does not equal 0, so there are treatment effects

## part b

## Problem 3: 
## part a: Ho is that there is no difference between electric
## use in July with homeowners with and without air conditioning
## Ha is that there is a difference between electric use in July
## with homeowners with and without air conditioning
xbar13 = matrix(c(204.4,556.6), nrow = 2, ncol = 1)
xbar13
xbar23 = matrix(c(130.0,355.0), nrow = 2, ncol = 1)
xbar23
cov13 = matrix(c(13825.3,23823.4,23823.4,73107.4), nrow = 2, ncol = 2)
cov13
cov23 = matrix(c(8632,19616.7,19616.7,55964.5), nrow = 2, ncol = 2)
cov23
difxbar3 = xbar13 - xbar23
difxbar3
tdifxbar3 = t(difxbar3)
pooledcov3 = ((1/45)*(cov13)) + ((1/55)*(cov23))
pooledcov3
invpooledcov3 = solve(pooledcov3)
tsquareds3 = tdifxbar3 %*% invpooledcov3 %*% difxbar3
tsquareds3
chicrit3 = qchisq(0.99,2)
chicrit3

## tsquared greater than tcrit, so reject null and accept alt
a13 = matrix(c(1,0), nrow = 2, ncol = 1)
sqrtchicrit3 = sqrt(chicrit3)
sqrtpooledcov13 = sqrt(t(a13) %*% pooledcov3 %*% a13)
sqrtpooledcov13
interval13 = sqrtchicrit3 * sqrtpooledcov13
interval13
intervallow13 = 74.4 - 65.385
intervalup13 = 74.4 - 65.385
intervalup13
a23 = matrix(c(0,1), nrow = 2, ncol = 1)
sqrtpooledcov23 = sqrt(t(a23) %*% pooledcov3 %*% a23)
interval23 = sqrtchicrit3 * sqrtpooledcov23
interval23
intervallow23 = 201.6 - 155.997
intervallow23
intervalup23 = 201.6 + 155.997
intervalup23
