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