## Problem 1.2
table1= read.csv(file.choose())
x1 <- table1[,1]
x2 <- table1[,2]
plot(x1,x2)
mean(x1)
mean(x2)
var(x1)
var(x2)
cov(x1,x2)
cov(x2,x1)
cor(x1,x2)
vector1 <- c(mean(x1), mean(x2))
meanarray <- array(vector1, dim= c(2,1))
meanarray
print(meanarray)

##Problem 1.9
table2= read.csv(file.choose())
x1b <- table2[,1]
x2b <- table2[,2]
plot(x1b, x2b)
s11 <- var(x1b)
s22 <- var(x2b)
s12 <- cov(x1b,x2b)
x1brotated <- x1b*cos(0.454)+x2b*sin(0.454)
x2brotated <- -x1b*sin(0.454)+x2b*cos(0.454)
table3 <- data.frame(x1brotated,x2brotated)
print(table3)
s11rot <- var(x1brotated)
s22rot <- var(x2brotated)
table4= read.csv(file.choose())
x1d <- table4[,1]
x2d <- table4[,2]
x1drotated <- (x1d*cos(0.454))+(x2d*sin(0.454))
x2drotated <- ((-x1d*sin(0.454))+(x2d*cos(0.454)))
x1drotated
x2drotated
disfromorigin <- sqrt(((x1drotated^2)/s11rot)+((x2drotated^2)/s22rot))
disfromorigin
a <- cos(0.454)^2
b <- sin(0.454)^2
c <- (a*s11)+(2*cos(0.454)*sin(0.454)*s12)+(b*s22)
d <- (a*s22)-(2*cos(0.454)*sin(0.454)*s12)+(b*s11)
e <- cos(0.454)*sin(0.454)
a11 <- (a/c)+(b/d)
a22 <- (b/c)+(a/d)
a12 <- (e/c)-(e/d)
disfromorigin2 <- sqrt((a11*(x1d^2))+(2*a12*x1d*x2d)+(a22*(x2d^2)))
disfromorigin2

##Problem 1.16
table5= read.csv(file.choose())
DR = table5[,1]
R = table5[,2]
DH = table5[,3]
H = table5[,4]
DU = table5[,5]
U = table5[,6]
meanDR <- mean(DR)
meanR <- mean(R)
meanDH <- mean(DH)
meanH <- mean(H)
meanDU <- mean(DU)
meanU <- mean(U)
meanvector <- c(meanDR,meanR,meanDH,meanH,meanDU,meanU)
meanarray <- array(meanvector, dim = c(6,1))
meanarray
DR11 <- var(DR)
DR12 <- cov(DR,R)
DR13 <- cov(DR,DH)
DR14 <- cov(DR,H)
DR15 <- cov(DR,DU)
DR16 <- cov(DR,U)
DRvarvector <- c(DR11,DR12,DR13,DR14,DR15,DR16)
R21 <- cov(R,DR)
R22 <- var(R)
R23 <- cov(R,DH)
R24 <- cov(R,H)
R25 <- cov(R,DU)
R26 <- cov(R,U)
Rvarvector <- c(R21,R22,R23,R24,R25,R26)
DH31 <- cov(DH,DR)
DH32 <- cov(DH,R)
DH33 <- var(DH)
DH34 <- cov(DH,H)
DH35 <- cov(DH,DU)
DH36 <- cov(DH,U)
DHvarvector <-c(DH31,DH32,DH33,DH34,DH35,DH36)
