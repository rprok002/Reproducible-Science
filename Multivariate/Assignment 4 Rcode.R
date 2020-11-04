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
sqrt6.5b = sqrt(m6.5*qf6.5)
contrast12 = matrix(c(1,-1,0), nrow = 1, ncol = 3)
lower12 = (contrast12 %*% mean6.5) - ((sqrt6.5b)*sqrt((contrast12 %*% cov6.5 %*% t(contrast12)/40)))
lower12
upper12 = (contrast12 %*% mean6.5) + ((sqrt6.5b)*sqrt((contrast12 %*% cov6.5 %*% t(contrast12)/40)))
upper12
contrast23 = matrix(c(0,1,-1), nrow = 1, ncol = 3)
lower23 = (contrast23 %*% mean6.5) - ((sqrt6.5b)*sqrt((contrast23 %*% cov6.5 %*% t(contrast23)/40)))
lower23
upper23 = (contrast23 %*% mean6.5) + ((sqrt6.5b)*sqrt((contrast23 %*% cov6.5 %*% t(contrast23)/40)))
upper23
contrast13 = matrix(c(1,0,-1), nrow = 1, ncol = 3)
lower13 = (contrast13 %*% mean6.5) - ((sqrt6.5b)*sqrt((contrast13 %*% cov6.5 %*% t(contrast13)/40)))
lower13
upper13 = (contrast13 %*% mean6.5) + ((sqrt6.5b)*sqrt((contrast13 %*% cov6.5 %*% t(contrast13)/40)))
upper13

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

## Problem 5: Exercise 6.13
## part d
Y16.13 = c(6,4,8,4,3,-3,4,-4,-3,-4,3,-4)
Y26.13 = c(8,6,12,6,8,2,3,3,2,-5,-3,-6)
Treat6.13 = c(rep("A",4), rep("B",4), rep("C",4))
Exam6.13 = data.frame(cbind(Y16.13,Y26.13),Treat6.13)
Exam6.13
x6.13manova = manova(cbind(Y16.13,Y26.13)~Treat6.13, data=Exam6.13)
x6.13manova
summary.aov(x6.13manova)
## Treatments effects for both at 0.05 alpha level

## Problem 6: Exercise 6.16
data6.16 = read.csv(file.choose())
matrix6.16 = matrix(c(data6.16[,1],data6.16[,2],data6.16[,3],data6.16[,4]), nrow = 30, ncol = 4)
matrix6.16
mean6.16 = matrix(c(mean(data6.16[,1]),mean(data6.16[,2]),mean(data6.16[,3]),mean(data6.16[,4])), nrow = 4, ncol = 1)
mean6.16
cov6.16 = cov(matrix6.16)
cov6.16
contrast6.16 = matrix(c(1,0,0,-1,1,0,0,-1,1,0,0,-1), nrow = 3, ncol = 4)
contrast6.16
contrastmean6.16 = contrast6.16 %*% mean6.16
contrastmean6.16
tcontrastmean6.16 = t(contrastmean6.16)
tcontrast6.16 = t(contrast6.16)
CSC6.16 = solve(contrast6.16 %*% cov6.16 %*% tcontrast6.16)
CSC6.16
tsquared6.15 = 30 * tcontrastmean6.16 %*% CSC6.16 %*% contrastmean6.16
tsquared6.15
qf6.16 = qf(0.95, 3,27)
m6.16 = ((30-1)*(4-1)/(30-4+1))
tcrit6.16 = m6.16 * qf6.16
tcrit6.16
sqrt6.16 = sqrt(m6.16*qf6.16)
contrast1234 = matrix(c(1,1,-1,-1), nrow = 1, ncol = 4)
contrast1234
lower1234 = (contrast1234 %*% mean6.16) - ((sqrt6.16)*sqrt((contrast1234 %*% cov6.16 %*% t(contrast1234)/30)))
lower1234
upper1234 = (contrast1234 %*% mean6.16) + ((sqrt6.16)*sqrt((contrast1234 %*% cov6.16 %*% t(contrast1234)/30)))
upper1234

## Problem 7: Exercise 6.19
## part a
data6.19 = read.csv(file.choose())
gas6.19 = matrix(c(data6.19[,1],data6.19[,2],data6.19[,3]), nrow = 36, ncol = 3)
gas6.19
dx1 = data6.19[1:23,4]
dx2 = data6.19[1:23,5]
dx3 = data6.19[1:23,6]
diesel6.19 = matrix(c(dx1,dx2,dx3), nrow = 23, ncol = 3)
diesel6.19
gascov6.19 = cov(gas6.19)
gascov6.19
dieselcov6.19 = cov(diesel6.19)
dieselcov6.19
meangas6.19 = matrix(c(mean(data6.19[,1]),mean(data6.19[,2]), mean(data6.19[,3])), nrow = 3, ncol = 1)
meangas6.19
meandiesel6.19 = matrix(c(mean(dx1), mean(dx2), mean(dx3)), nrow = 3, ncol = 1)
meandiesel6.19
difmean6.19 = meangas6.19 - meandiesel6.19
difmean6.19
Sp6.19 = ((((36-1)*gascov6.19)+ ((23-1)*dieselcov6.19)))/(36+23-2)
invSp6.19 = solve(((1/36)+(1/23))*Sp6.19)
tsquared6.19 = t(difmean6.19) %*% invSp6.19 %*% difmean6.19
tsquared6.19
qf6.19 = qf(0.99, 3,36+23-1)
qf6.19
m6.19 = (((36+23-2)*(3))/(36+23-3-1))
tcrit6.19 = m6.19 * qf6.19
tcrit6.19
chsquare6.19 = qchisq(0.99,3)
## Can we assume this is small because one of the sample sizes is less than 30?
## Either way, reject null that there is no difference between means of pops
## part b
a16.19 = matrix(c(1,0,0), nrow = 3, ncol = 1)
a26.19 = matrix(c(0,1,0), nrow = 3, ncol = 1)
a36.19 = matrix(c(0,0,1), nrow = 3, ncol = 1)
linear16.19 = ((t(a16.19) %*% difmean6.19)^2)/(t(a16.19) %*% (((1/36)+(1/23))*Sp6.19) %*% a16.19)
linear16.19
linear26.19 = ((t(a26.19) %*% difmean6.19)^2)/(t(a26.19) %*% (((1/36)+(1/23))*Sp6.19) %*% a26.19)
linear26.19
linear36.19 = ((t(a36.19) %*% difmean6.19)^2)/(t(a36.19) %*% (((1/36)+(1/23))*Sp6.19) %*% a36.19)
linear36.19
tcrit6.19
## reject Ho only found with capital
## part c
lowerx16.19 = (t(a16.19) %*% difmean6.19) - sqrt(tcrit6.19) * sqrt(((1/36)+(1/23))*t(a16.19)%*%Sp6.19%*%a16.19)
lowerx16.19
upperx16.19 = (t(a16.19) %*% difmean6.19) + sqrt(tcrit6.19) * sqrt(((1/36)+(1/23))*t(a16.19)%*%Sp6.19%*%a16.19)
upperx16.19
lowerx26.19 = (t(a26.19) %*% difmean6.19) - sqrt(tcrit6.19) * sqrt(((1/36)+(1/23))*t(a26.19)%*%Sp6.19%*%a26.19)
lowerx26.19
upperx26.19 = (t(a26.19) %*% difmean6.19) + sqrt(tcrit6.19) * sqrt(((1/36)+(1/23))*t(a26.19)%*%Sp6.19%*%a26.19)
upperx26.19
lowerx36.19 = (t(a36.19) %*% difmean6.19) - sqrt(tcrit6.19) * sqrt(((1/36)+(1/23))*t(a36.19)%*%Sp6.19%*%a36.19)
lowerx36.19
upperx36.19 = (t(a36.19) %*% difmean6.19) + sqrt(tcrit6.19) * sqrt(((1/36)+(1/23))*t(a36.19)%*%Sp6.19%*%a36.19)
upperx36.19
## appears to be capital
## part d
## assume random samples of each population, pops indep of each other
## both pops are normally distributed and have equal covariance matrices
data6.19d = read.csv(file.choose())
gx1d = data6.19d[,1]
gx2d = data6.19d[,2]
gx3d = data6.19d[,3]
gas6.19d = matrix(c(gx1d,gx2d,gx3d), nrow = 34, ncol = 3)
gas6.19d
diesel6.19d = diesel6.19
gascov6.19d = cov(gas6.19d)
gascov6.19d
dieselcov6.19d = dieselcov6.19
meangas6.19d = matrix(c(mean(data6.19d[,1]),mean(data6.19d[,2]), mean(data6.19d[,3])), nrow = 3, ncol = 1)
meangas6.19d
meandiesel6.19d = meandiesel6.19
difmean6.19d = meangas6.19d - meandiesel6.19d
difmean6.19d
Sp6.19d = ((((34-1)*gascov6.19d)+ ((23-1)*dieselcov6.19d)))/(36+23-2)
Sp6.19d
invSp6.19d = solve(((1/34)+(1/23))*Sp6.19d)
tsquared6.19d = t(difmean6.19d) %*% invSp6.19d %*% difmean6.19d
tsquared6.19d
qf6.19d = qf(0.99, 3,34+23-1)
qf6.19d
m6.19d = (((34+23-2)*(3))/(34+23-3-1))
tcrit6.19d = m6.19d * qf6.19d
tcrit6.19d
chsquare6.19 = qchisq(0.99,3)
chsquare6.19
## still reject null 
## part e
testCov(gas6.19,diesel6.19, alpha = 0.05)
## not sure how to do e
## have no idea if I'm doing this correctly, need to ask prof
## Problem 8: Exercise 6.26
meantest6.26 = matrix(c(0.153,-.231,-.322,-.339), nrow = 4, ncol = 1)
meantest6.26
meancontrol6.26 = matrix(c(0.151,0.180,0.256,0.257), nrow = 4, ncol = 1)
meancontrol6.26
Sp6.26 = matrix(c(.804,.355,.228,.232,.355,.722,.233,.199,.228,.233,.592,.239,.232,.199,.239,.479), nrow = 4, ncol = 4)
Sp6.26
contrast6.26 = matrix(c(-1,0,0,1,-1,0,0,1,-1,0,0,1), nrow = 3, ncol = 4)
contrast6.26
CSPC6.26 = contrast6.26 %*% Sp6.26 %*% t(contrast6.26)
CSPC6.26
difmean6.26 = meantest6.26 - meancontrol6.26
difmean6.26
contrastmean6.26 = contrast6.26 %*% difmean6.26
b6.26 = ((1/28)+(1/58))^-1
contrastmeanb6.26 = contrastmean6.26 * b6.26
contrastmeanb6.26
tsquared6.26 = t(contrastmeanb6.26) %*% solve(CSPC6.26) %*% contrastmean6.26
tsquared6.26
qf6.26 = qf(0.95, 3,28+58-4+1)
qf6.26
m6.26 = ((28+58-1)*(4-1)/(28+58-4+1))
tcrit6.26 = m6.26 * qf6.26
tcrit6.26
## are there other steps for 8 when reject null?

## Problem 9: Exercise 6.32
X16.32 = c(10.35,13.41,7.78,10.40,17.78,10.40)
X26.32 = c(25.93,38.63,25.15,24.25,41.45,29.20)
species6.32 = c("SS", "JL", "LP", "SS", "JL", "LP")
nutrient6.32 = c(rep("+",3),rep("-",3))
Exam6.32 <- data.frame(X16.32,X26.32,species6.32,nutrient6.32)
Exam6.32
Ex16.32 <- aov(X16.32~species6.32+nutrient6.32, data = Exam6.32)
summary(Ex16.32)
Ex26.32 = aov(X26.32~species6.32+nutrient6.32, data = Exam6.32)
summary(Ex26.32)
Extotal6.32 = manova(cbind(X16.32,X26.32)~species6.32+nutrient6.32, data = Exam6.32)
summary(Extotal6.32)
## MANOVA isn't significant but when break down, find species is 
## significant but nutrient isn't

## Problem 10 portions
SSint6.32 = matrix(c(32,0,0,44), nrow = 2, ncol = 2)
SSint6.32
SSresabs6.32 = matrix(c(324,96,96,436), nrow = 2, ncol = 2)
SSresabs6.32
SSfac26.32 = matrix(c(24,36,36,0), nrow = 2, ncol = 2)
SSfac26.32
SSfac16.32 = matrix(c(496,184,184,208), nrow = 2, ncol = 2)
SSfac16.32
lambda16.32 = SSresabs6.32/(SSfac16.32+SSresabs6.32)
lambda26.32 = SSresabs6.32/(SSfac26.32+SSresabs6.32)
## Problem 10 n = 2, but need p
## part c
## assume random sample and normal distribute numbers.
## Each dep variable normal distribute in each indep variable
## samples are independent of one another
## all pops have common covariance matrix

##Questions
## How do we decide large/small sample and equal cov matrices
## Prob 3: Use lrg data assumption b/c told us to, use chi square
## Prob 3 ellipse doesn't seem to be working
## Prob 4 small sample and different cov matrices, so which do we use?
## Prob 7 big or small?
## Prob 8 what do we do with profile analysis when reject?
## Prob 10 what is p?