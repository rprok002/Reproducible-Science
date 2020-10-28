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