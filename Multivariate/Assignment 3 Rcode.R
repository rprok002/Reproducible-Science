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


## Problem 2: Exercise 5.7
table5.7 = read.csv(file.choose())
sweat = table5.7[,2]
sweat
sodium = table5.7[,3]
potassium = table5.7[,4]
table5.7123 = cbind(sweat,sodium,potassium)
table5.7123
matrix5.7 = matrix(c(sweat, sodium, potassium), nrow = 20, ncol = 3)
matrix5.7
xbar2 = matrix(c(mean(sweat), mean(sodium), mean(potassium)), nrow = 3, ncol = 1)
xbar2
cov2 = cov(matrix5.7)
cov2
qf(0.95, 3, 17)
(3*19)/17
tcrit2 = qf(0.95, 3, 17) * (3*19)/17
tcrit2
sqrttcrit = sqrt(tcrit2)
sqrttcrit
sqrtss1n = sqrt(2.879368/20)
sqrtss1n
mu1lower = 4.640 - (sqrttcrit * sqrtss1n)
mu1lower
mu1upper = 4.640 + (sqrttcrit * sqrtss1n)
mu1upper

sqrtss2n = sqrt(199.7884/20) 
sqrtss2n
mu2lower = 45.400 - (sqrttcrit * sqrtss2n)
mu2lower
mu2upper = 45.400 + (sqrttcrit * sqrtss2n)
mu2upper

sqrtss3n = sqrt(3.627658/20) 
sqrtss3n
mu3lower = 9.965 - (sqrttcrit * sqrtss3n)
mu3lower
mu3upper = 9.965 + (sqrttcrit * sqrtss3n)
mu3upper

conmatrix5.7lower = matrix(c(3.398,35.052,8.571), nrow = 3, ncol = 1)
conmatrix5.7lower
conmatrix5.7upper = matrix(c(5.882,55.748,11.359), nrow = 3, ncol = 1)
conmatrix5.7upper

tcrit0.05 = qt((1-0.05/2*3),19)
bonmu1lower = 4.640 - (tcrit0.05 * sqrtss1n)
bonmu1lower
bonmu1upper = 4.640 + (tcrit0.05 * sqrtss1n)
bonmu1upper

bonmu2lower = 45.400 - (tcrit0.05 * sqrtss2n)
bonmu2lower
bonmu2upper = 45.400 + (tcrit0.05 * sqrtss2n)
bonmu2upper

bonmu3lower = 9.965 - (tcrit0.05 * sqrtss3n)
bonmu3lower
bonmu3upper = 9.965 + (tcrit0.05 * sqrtss3n)
bonmu3upper


## Problem 3: Exercise 5.9 
## part a
sample5.9 = matrix(c(95.52,164.38,55.69,93.39,17.98,31.13), nrow = 6, ncol = 1)
sample5.9
cov5.9 = matrix(c(3266.46,1343.97,731.54,1175.50,162.68,238.37,1343.97,721.91,324.25,537.35,80.17,117.73,731.54,324.25,179.28,281.17,39.15,56.80,1175.50,537.35,281.17,474.98,63.73,94.85,162.68,80.17,39.15,63.73,9.95,13.88,
                  238.37,117.73,56.80,94.85,13.88,21.26), nrow = 6, ncol = 6)
cov5.9
sqrtchisquare5.9 = sqrt(qchisq(0.95,6))
sqrtchisquare5.9
sqrts11n = sqrt(3266.46/61)
sqrts22n = sqrt(721.91/61)
sqrts33n = sqrt(179.28/61)
sqrts44n = sqrt(474.98/61)
sqrts55n = sqrt(9.95/61)
sqrts66n = sqrt(21.26/61)
simmatrix5.9 = matrix(c(sqrtchisquare5.9*sqrts11n,sqrtchisquare5.9*sqrts22n,sqrtchisquare5.9*sqrts33n,sqrtchisquare5.9*sqrts44n,sqrtchisquare5.9*sqrts55n,sqrtchisquare5.9*sqrts66n), nrow = 6, ncol = 1)
simmatrix5.9
lower5.9 = sample5.9 - simmatrix5.9
lower5.9
upper5.9 = sample5.9 + simmatrix5.9
upper5.9
## parb b 
sample5.9b = c(95.52,93.39)
cov5.9b = matrix(c(3266.46,1175.50,1175.50,474.98), nrow = 2, ncol = 2)
ellipse(sample5.9b,cov5.9b, alpha = 0.95, newplot = TRUE, xlab = "Weight", ylab = "Girth")
## part c
qnorm((1-0.95)/12)
zscorecrit5.9 = 2.64
zscorecrit5.9
bonmatrix5.9 = matrix(c(zscorecrit5.9*sqrts11n,zscorecrit5.9*sqrts22n,zscorecrit5.9*sqrts33n,zscorecrit5.9*sqrts44n,zscorecrit5.9*sqrts55n,zscorecrit5.9*sqrts66n), nrow = 6, ncol = 1)
bonmatrix5.9
bonlower5.9 = sample5.9 - bonmatrix5.9
bonlower5.9
bonupper5.9 = sample5.9 + bonmatrix5.9
bonupper5.9
qnorm((1-0.95)/12)
## part d 
sample5.9b = c(95.52,93.39)
cov5.9b = matrix(c(3266.46,1175.50,1175.50,474.98), nrow = 2, ncol = 2)
ellipse(sample5.9b,cov5.9b, alpha = 0.95, newplot = TRUE, xlab = "Weight", ylab = "Girth", xlim = c(70,120), ylim = c(80,106))
abline(v = 76.201,  h = 86.023)
abline( v = 114.839,  h = 100.757)
## part e
widthminuslength = 31.13 - 17.98
zscore5.9e = -qnorm((1-0.95)/12)
sqrt12n = sqrt((9.95-13.88-13.88+21.26)/61)
sqrt12n
coninterval5.9e = zscore5.9e * sqrt12n
lower5.9e = widthminuslength - coninterval5.9e
lower5.9e
upper5.9e = widthminuslength + coninterval5.9e
upper5.9e

## Problem 4: Exercise 5.18
## part a
table4 = read.csv(file.choose())
table4
sciencehistory = table4[,2]
verbal = table4[,3]
science = table4[,4]
matrix5.18 = matrix(c(sciencehistory, verbal, science), nrow = 87, ncol = 3)
matrix5.18
meansciencehistory = mean(sciencehistory)
meanverbal = mean(verbal)
meanscience = mean(science)
mean5.18 = matrix(c(meansciencehistory, meanverbal, meanscience), nrow = 3, ncol = 1)
mean5.18
cov5.18 = cov(matrix5.18)
cov5.18
invcov5.18 = solve(cov5.18)
invcov5.18
dev5.18 = c(meansciencehistory-500, meanverbal-50, meanscience-30)
dev5.18
dev5.18transpose = t(dev5.18)
ttest5.18 = 87*dev5.18transpose %*% solve(cov5.18) %*% dev5.18 
ttest5.18
library(robustbase)
library(pcaPP)
library(rrcov)
qf(0.95, 3, 84)
(86*3)/84
tcrit15.18 = (86*3)/84 * qf(0.95, 3, 84)
tcrit15.18
T2.test(matrix5.18, mu = c(500,50,30), conf.level = 0.95, test = "f")
## Yes, the t-test results show that mu is not equal to these numbers,
## so the students in the table are scoring differently than the average
## college students over the past 10 years
## part b no f
eigen5.18 = eigen(cov5.18)
eigen5.18
sqrtscihiseigen = sqrt(5879.56342)
sqrtvereigen = sqrt(64.37503)
sqrtscieigen = sqrt(14.59216)
pthing = (3*(87-1))/(87*(87-84))
fstat5.18 = qf(0.95,3,84)
sqrt5.18 = sqrt(pthing*fstat5.18)
lengths5.18 = matrix(c(sqrtscihiseigen*sqrt5.18,sqrtvereigen*sqrt5.18,sqrtscieigen*sqrt5.18), nrow = 3, ncol = 1)
lengths5.18
eigenscihis = eigen5.18$vectors[,1]
eigenscihis
eigenver = eigen5.18$vectors[,2]
eigenver
eigensci = eigen5.18$vectors[,3]
eigensci
## part c 
scihistoryQQ = qqnorm(sciencehistory)
qqline(sciencehistory)
verQQ = qqnorm(verbal)
qqline(verbal)
sciQQ = qqnorm(science)
qqline(science)
plot(sciencehistory,verbal, xlab = "Social Science/History", ylab = "Verbal")
plot(sciencehistory,science, xlab = "Social Science/History", ylab = "Science")
plot(verbal,science, xlab = "Verbal", ylab = "Science")

##Problem 5: Exercise 5.20
table5 = read.csv(file.choose())
meantaillength = mean(table5[,1])
meantaillength
meanwinglength = mean(table5[,2])
meanwinglength
mean5.20 = c(meantaillength,meanwinglength)
mean5.20
matrix5.20 = matrix(c(table5[,1],table5[,2]), nrow = 45, ncol = 2)
cov5.20 = cov(matrix5.20)
cov5.20
ellipse(mean5.20,cov5.20, alpha = 0.95, newplot = TRUE, xlab = "Tail Length", ylab = "Wing Length")
value5.20 = c(190,275)
dev5.20 = mean5.20 - value5.20
dev5.20
matrixdev5.20 = matrix(dev5.20, nrow = 2, ncol = 1)
matrixdev5.20
transposematrixdev5.20 = t(matrixdev5.20)
invcov5.20 = solve(cov5.20)
tsquared5.20 = 45*transposematrixdev5.20 %*% invcov5.20 %*% matrixdev5.20
tsquared5.20
tcritinternal5.20 = qf(0.95, 2, 43)
tcritinternal5.20
(2*44)/43
tcrit5.20 = 2.046512 * tcritinternal5.20
tcrit5.20
tsquared5.20 < tcrit5.20
## Fail to reject null hypothesis. So, statistically male and female values
## are not different and male values are plausible for female values
## part b
sqrtchisq5.20 = sqrt(qchisq(0.95,2))
sqrtchisq5.20
s11n5.20 = sqrt(120.6949/45)
s22n5.20 = sqrt(208.5404/45)
tcritmatrix5.20 = matrix(c(sqrtchisq5.20*s11n5.20,sqrtchisq5.20*s22n5.20), nrow = 2, ncol = 1)
tcritmatrix5.20
meanmatrix5.20 = matrix(c(meantaillength,meanwinglength), nrow = 2, ncol = 1)
mu5.20lower = meanmatrix5.20 - tcritmatrix5.20
mu5.20lower
mu5.20upper = meanmatrix5.20 + tcritmatrix5.20
mu5.20upper
zscore5.20 = -qnorm((1-0.95)/4)
zscore5.20
bonmatrix5.20 = matrix(c(zscore5.20*s11n5.20, zscore5.20*s22n5.20), nrow = 2, ncol = 1)
bonlower5.20 = meanmatrix5.20 - bonmatrix5.20
bonlower5.20
bonupper5.20 = meanmatrix5.20 + bonmatrix5.20
bonupper5.20
## Tsquared interval is wider than Bonferroni, so is less conservative
## part c
library(matlib)
QQ5.20x1 = qqnorm(table5[,1])
qqline(table5[,1])
QQ5.20x2 = qqnorm(table5[,2])
qqline(table5[,2])
plot(table5[,1], table5[,2], xlab = "Tail Length", ylab = "Wing Length")
line=abline(0,1)

##Problem 6: Exercise 5.23
## part a
table6 = read.csv(file.choose())
maxbreath = table6[,1]
basheight = table6[,2]
baslength = table6[,3]
nasheight = table6[,4]
matrix5.23 = matrix(c(maxbreath,basheight,baslength,nasheight), nrow = 30, ncol = 4)
qqmaxbreath = qqnorm(table6[,1])
qqline(table6[,1])
qqbasheight = qqnorm(table6[,2])
qqline(table6[,2])
qqbaslength = qqnorm(table6[,3])
qqline(table6[,3])
qqnasheight = qqnorm(table6[,4])
qqline(table6[,4])
XX5.23 = cbind(maxbreath - mean(maxbreath), basheight - mean(basheight), baslength-mean(baslength), nasheight-mean(nasheight))
KK5.23 = (as.matrix(XX5.23) %*% solve(cov(matrix5.23)) %*% t(as.matrix(XX5.23)))
mKK5.23 = round(diag(KK5.23),4)
mKK5.23
J5.23 = seq(1:30)
qcp5.23 = qchisq((30-J5.23+.5)/30,4)
qcp5.23order = sort(qcp5.23)
distances5.23order = sort(mKK5.23)
distances5.23order
plot(qcp5.23order,distances5.23order)
line = abline(0,1)
## part b
mean5.23 = matrix(c(mean(maxbreath), mean(basheight), mean(baslength), mean(nasheight)), nrow = 4, ncol = 1)
mean5.23
matrix5.23 = matrix(c(maxbreath,basheight,baslength,nasheight), nrow = 30, ncol = 4)
cov5.23 = cov(matrix5.23)
cov5.23
tcrit0.055.23 = qt((1-0.05/8), 29)
bons11n5.23 = sqrt(26.309195/30)
bons22n5.23 = sqrt(19.9724138/30)
bons33n5.23 = sqrt(34.6264368/30)
bons44n5.23 = sqrt(7.6367816/30)
bonmatrix5.23 = matrix(c(tcrit0.055.23*bons11n5.23, tcrit0.055.23*bons22n5.23, tcrit0.055.23*bons33n5.23, tcrit0.055.23*bons44n5.23), nrow = 4, ncol = 1)
bonlower5.23 = mean5.23 - bonmatrix5.23
bonlower5.23
bonupper5.23 = mean5.23 + bonmatrix5.23
bonupper5.23
tcrit6 = qf(0.95, 4, 26) * (4*29)/26
sqrttcrit6 = sqrt(tcrit6)
sqrttcrit6
tmatrix5.23 = matrix(c(sqrttcrit6*bons11n5.23, sqrttcrit6*bons22n5.23, sqrttcrit6*bons33n5.23, sqrttcrit6*bons44n5.23), nrow = 4, ncol = 1)
tlower5.23 = mean5.23 - tmatrix5.23
tupper5.23 = mean5.23 + tmatrix5.23
bonlower5.23
bonupper5.23
tlower5.23
tupper5.23

## Problem 7: Exercise 5.30
## part a mean for each
mean5.30 = matrix(c(0.766,0.508,0.438,0.161), nrow = 4, ncol = 1)
mean5.30
cov5.30 = matrix(c(0.856,0.635,0.173,0.096,0.635,0.568,0.127,0.067,0.173,0.128,0.171,0.039,0.096,0.067,0.039,0.043), nrow = 4, ncol = 4)
cov5.30
s11n5.30 = sqrt(0.856/50)
s22n5.30 = sqrt(0.568/50)
s33n5.30 = sqrt(0.171/50)
s44n5.30 = sqrt(0.043/50)
innerzscore5.30 = (1-(0.05/(2*4)) )
innerzscore5.30
## zscore for 0.9938
zscore5.30 = 2.50
simmatrix5.30 = matrix(c(zscore5.30*s11n5.30, zscore5.30*s22n5.30,zscore5.30*s33n5.30,zscore5.30*s44n5.30), nrow = 4, ncol = 1)
simmatrix5.30
bonlower5.30 = mean5.30 - simmatrix5.30
bonlower5.30
bonupper5.30 = mean5.30 +simmatrix5.30
bonupper5.30
## part a total
totalmean5.30 = matrix(0.766+0.508+0.438+0.161)
covlist5.30 = c(cov5.30)
covlist5.30
sumcovlist5.30 = sum(covlist5.30)
sumcovlist5.30
stotaln5.30 = sqrt(sumcovlist5.30/50)
simmatrixtotal5.30 = matrix(c(zscore5.30*stotaln5.30), nrow = 1, ncol = 1)
bontotallower5.30 = totalmean5.30 - simmatrixtotal5.30
bontotallower5.30
bontotalupper5.30 = totalmean5.30 + simmatrixtotal5.30
bontotalupper5.30
## part a difference
pertroleumminusnatural = 0.766 - 0.508
zscore5.30 = 2.50
sqrt12n5.30 = sqrt((0.856-0.635-0.635+0.568)/50)
sqrt12n5.30
coninterval5.30a = zscore5.30 * sqrt12n5.30
bonlower5.30a = pertroleumminusnatural - coninterval5.30a
bonlower5.30a
bonupper5.930a = pertroleumminusnatural + coninterval5.30a
bonupper5.930a
## part b mean for each
chsquare5.30 = qchisq(0.95,4)
chsquare5.30
sqrtchisq5.30 = sqrt(chsquare5.30)
sinmatrix5.30b = matrix(c(sqrtchisq5.30*s11n5.30, sqrtchisq5.30*s22n5.30,sqrtchisq5.30*s33n5.30,sqrtchisq5.30*s44n5.30), nrow = 4, ncol = 1)
sinmatrix5.30b
mulower5.30 = mean5.30 - sinmatrix5.30b
mulower5.30
muupper5.30 = mean5.30 + sinmatrix5.30b
muupper5.30
## parb b total
totalmean5.30 = matrix(0.766+0.508+0.438+0.161)
covlist5.30 = c(cov5.30)
covlist5.30
sumcovlist5.30 = sum(covlist5.30)
sumcovlist5.30
stotaln5.30 = sqrt(sumcovlist5.30/50)
simmatrixtotal5.30b = matrix(c(sqrtchisq5.30*stotaln5.30), nrow = 1, ncol = 1)
mutotallower5.30 = totalmean5.30 - simmatrixtotal5.30b
mutotallower5.30
mutotalupper5.30 = totalmean5.30 + simmatrixtotal5.30b
mutotalupper5.30
## part b difference
pertroleumminusnatural = 0.766 - 0.508
chsquare5.30 = qchisq(0.95,4)
chsquare5.30
sqrt12n5.30 = sqrt((0.856-0.635-0.635+0.568)/50)
sqrt12n5.30
coninterval5.30b = sqrtchisq5.30 * sqrt12n5.30
lower5.30b = pertroleumminusnatural - coninterval5.30b
lower5.30b
upper5.930b = pertroleumminusnatural + coninterval5.30b
upper5.930b

