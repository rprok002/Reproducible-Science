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

## part b
ctr<-c(74.4, 201.6)
library(mixtools)
s1=matrix(c(13825.3, 23823.4, 23823.4,73107.4),2)
s2=matrix(c(8632, 19616.7, 19616.7,55964.5),2)
Sp=((45-1)/(45+55-2))*s1+((55-1)/(45+55-2))*s2
Sp
A <-Sp # covariance matrix -> cov(dataMatrix)
RR <- chol(A) # Cholesky decomposition
angles <- seq(0, 2*pi, length.out=200) # angles for ellipse
ell <- 1 * cbind(cos(angles), sin(angles)) %*% RR # ellipse scaled with factor 1
ellCtr <- sweep(ell, 2, ctr, "+") # center ellipse to the data centroid
plot(ellCtr, type="l", lwd=2, xlab="mu11-mu21",ylab="mu12-m22", asp=1)# plot ellipse
points(ctr[1], ctr[2], xlab="m11-mu21", pch=4, lwd=2) # plot data centroid
library(car) # verify with car's ellipse() function
ellipse(ctr, shape=A, radius=0.98, col="red", lty=2) 
eigVal <- eigen(A)$values
eigVec <- eigen(A)$vectors
eigScl <- eigVec %*% diag(sqrt(eigVal)) # scale eigenvectors to length = square-root
xMat <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) # normal ellipse
ellRot <- eigVec %*% t(ellBase) # rotated ellipse
matlines(xMat, yMat, lty=1, lwd=2, col="green")
points(ctr[1], ctr[2], pch=4, col="red", lwd=3)
#abline(h=0, untf=FALSE, lty=1)
#abline(v=0, untf=FALSE, lty=1)

## Problem 4: Exercise 6.6
## part a
treatment2 = matrix(c(3,6,3,3,1,2), nrow = 3, ncol = 2)
treatment3 = matrix(c(3,1,1,3,2,5,3,2), nrow = 4, ncol = 2)
cov6.62 = cov(treatment2)
cov6.63 = cov(treatment3)
cov6.62
cov6.63
Sp6.6a = (((3-1)*cov6.62)+((4-1)*cov6.63))/(3+4-2)
Sp6.6a
tstatS = ((1/3)+(1/4))*Sp6.6a
invtstatS = solve(tstatS)
meantreatment2.1= mean(treatment2[,1])
meantreatment2.1
meantreatment2.2 = mean(treatment2[,2])
meantreatment3.1 = mean(treatment3[,1])
meantreatment3.2 = mean(treatment3[,2])
meantreatment2 = matrix(c(meantreatment2.1,meantreatment2.2), nrow = 2, ncol = 1)
meantreatment2
meantreatment3 = matrix(c(meantreatment3.1, meantreatment3.2), nrow = 2, ncol = 1)
meantreatment3
difmean6.6a = meantreatment2-meantreatment3
difmean6.6a
tdifmean6.6a = t(difmean6.6a)
tsquared = tdifmean6.6a %*% invtstatS %*% difmean6.6a
tsquared
x = ((3+4-2)*2)/(3+4-2-1)
x
u = qf(0.99,2,3+4-2-1)
u
tcrit6.6 = x*u
tcrit6.6
## fail to reject null
## part c
relate1S = sqrt(((1/3)*(1/4))*2)
interval16.6 = sqrt(tcrit6.6)*relate1S
interval16.6low = 2 - interval16.6
interval16.6low
interval16.6up = 2 + interval16.6
interval16.6up
relate2S = sqrt(((1/3)*(1/4))*1.6)
interval26.6 = sqrt(tcrit6.6)*relate2S
interval26.6low = -1 - interval26.6
interval26.6low
interval26.6up = -1 + interval26.6
interval26.6up
## both intervals cover 0, which supports failing to reject null