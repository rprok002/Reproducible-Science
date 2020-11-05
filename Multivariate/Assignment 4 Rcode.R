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
qt6.2 = qt((1-(.05/4)),10)
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
a23 = matrix(c(0,1), nrow = 2, ncol = 1)
linear13 = ((t(a13) %*% difxbar3)^2)/(t(a13) %*% (((1/45)+(1/55))*pooledcov3) %*% a13)
linear13
linear23 = ((t(a23) %*% difxbar3)^2)/(t(a23) %*% (((1/45)+(1/55))*pooledcov3) %*% a23)
linear23
qf3 = qf(0.99, 2,45+55-2-1)
qf3
m3 = (((45+55-2)*(2))/(45+55-2-1))
tcrit3 = m3 * qf3 
tcrit3
## off-peak contributes more 
## Need assumptions of independent pops and random samples
## When covariances are unequal, need large sample size 

## part b
mu1=matrix(c(204.4,556.6),ncol=1)
mu2=matrix(c(130.0, 355.0),ncol=1)
s1=matrix(c(13825.3, 23823.4, 23823.4, 73107.4),ncol=2)
s2=matrix(c(8632, 19616.7, 19616.7, 55964.5),ncol=2)
snew=(1/45)*s1+(1/55)*s2
munew=c(mu1[1,]-mu2[1,], mu1[2,]-mu2[2,])
tsqr=t(munew)%*%solve(snew)%*%munew
tsqr
qchisq(0.9,2)
library (mixtools)
ellipse(munew, snew, alpha=0.99,npoints=200, type='l', newplot=TRUE, lwd=2, xlab='x11-x12', ylab='x21-x22')

## Problem 4: Exercise 6.6
## part a
x21 = c(3,1,2)
x22 = c(3,6,3)
x2 = cbind(x21,x22)
x2
x31 = c(2,5,3,2)
x32 = c(3,1,1,3)
x3 = cbind(x31,x32)
cov6.62 = cov(x2)
cov6.63 = cov(x3)
cov6.62
cov6.63
Sp6.6a = (((3-1)*cov6.62)+((4-1)*cov6.63))/(3+4-2)
Sp6.6a
tstatS = ((1/3)+(1/4))*Sp6.6a
invtstatS = solve(tstatS)
meantreatment2.1= mean(x2[,1])
meantreatment2.1
meantreatment2.2 = mean(x2[,2])
meantreatment3.1 = mean(x3[,1])
meantreatment3.2 = mean(x3[,2])
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
relate1S = sqrt(((1/3)+(1/4))*1.6)
interval16.6 = sqrt(tcrit6.6)*relate1S
interval16.6low = -1 - interval16.6
interval16.6low
interval16.6up = -1 + interval16.6
interval16.6up
relate2S = sqrt(((1/3)+(1/4))*2)
interval26.6 = sqrt(tcrit6.6)*relate2S
interval26.6low = 2 - interval26.6
interval26.6low
interval26.6up = 2 + interval26.6
interval26.6up
## both intervals cover 0, which supports failing to reject null

## Problem 5: Exercise 6.13
## part d
SSres6.13 = matrix(c(14,-8,-8,30), nrow = 2, ncol = 2)
SSres6.13
SSfac16.13 = matrix(c(104,148,148,248), nrow = 2, ncol = 2)
SSfac16.13
SSfac26.13 = matrix(c(90,51,51,54), nrow = 2, ncol = 2)
SSfac26.13
lambda16.13 = det(SSres6.13)/(det(SSfac16.13+SSres6.13))
lambda26.13 = det(SSres6.13)/(det(SSfac26.13+SSres6.13))
lambda16.13
lambda26.13
factor16.13 = -(6-((2+1-2)/2))*log(lambda16.13)
factor16.13
chifactor16.32 = qchisq(0.95,4)
chifactor16.32
factor26.13 = -6*log(lambda26.13)
factor26.13
chifactor26.32 = qchisq(0.95,6)
chifactor26.32
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
qf6.19 = qf(0.99, 3,36+23-3-1)
qf6.19
m6.19 = (((36+23-2)*(3))/(36+23-3-1))
tcrit6.19 = m6.19 * qf6.19
tcrit6.19
##reject null that there is no difference between means of pops
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
qf6.19d = qf(0.99, 3,34+23-3-1)
qf6.19d
m6.19d = (((34+23-2)*(3))/(34+23-3-1))
tcrit6.19d = m6.19d * qf6.19d
tcrit6.19d
## still reject null 
## part e
ae = (1/36)+(1/23)- (1/(36+23))
be= ((2*3*3)+((3*3)-1))/(6*(3+1)*(2-1))
u=ae*be
u
ce = (36+23)*log(det(Sp6.19))
de = (36*log(det(gascov6.19)))+(23*log(det(dieselcov6.19)))
M= ce-de
M
Ce = (1-u)*M
Ce
v = 0.5*3*(3+1)*(2-1)
v
chi6.19e = qchisq(0.99,6)
chi6.19e
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
qf6.26 = qf(0.95, 3,28+58-4)
qf6.26
m6.26 = ((28+58-2)*(4-1)/(28+58-4))
m6.26
tcrit6.26 = m6.26 * qf6.26
tcrit6.26
## are there other steps for 8 when reject null? No reason to check it

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
summary(Extotal6.32, test = "Wilks")
## MANOVA isn't significant but when break down, find species is 
## significant but nutrient isn't

## Problem 10 portions
SSint6.32 = matrix(c(32,0,0,44), nrow = 2, ncol = 2)
SSint6.32
SSres6.32 = matrix(c(324,-96,-96,436), nrow = 2, ncol = 2)
SSres6.32
SSfac26.32 = matrix(c(24,36,36,0), nrow = 2, ncol = 2)
SSfac26.32
SSfac16.32 = matrix(c(496,184,184,208), nrow = 2, ncol = 2)
SSfac16.32
lambda6.32 = det(SSres6.32)/(det(SSint6.32+SSres6.32))
lambda16.32 = det(SSres6.32)/(det(SSfac16.32+SSres6.32))
lambda26.32 = det(SSres6.32)/(det(SSfac26.32+SSres6.32))
lambda6.32
lambda16.32
lambda26.32
interact6.32 = -((3*4*(2-1))-(((2+1)-((4-1)*(3-1)))/2))*log(lambda6.32)
interact6.32
dfinteract6.32 = 2*(4-1)*(3-1)
dfinteract6.32
chilambda6.32 = qchisq(0.99,12)
chilambda6.32
factor16.32 = -((3*4*(2-1))-(((2+1)-(3-1))/2))*log(lambda16.32)
factor16.32
chifactor16.32 = qchisq(0.99,4)
chifactor16.32
factor26.32 = -((3*4*(2-1))-(((2+1)-(4-1))/2))*log(lambda26.32)
factor26.32
chifactor26.32 = qchisq(0.99,6)
chifactor26.32
## Problem 10 n = 2, but need p = 2
## part c
## assume random sample and normal distribute numbers.
## Each dep variable normal distribute in each indep variable
## samples are independent of one another
## all pops have common covariance matrix
## pg 315, under equation 6.59


## n-p less than 30, then small sample size



