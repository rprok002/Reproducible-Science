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
H41 <- cov(H,DR)
H42 <- cov(H,R)
H43 <- cov(H,DH)
H44 <- var(H)
H45 <- cov(H,DU)
H46 <- cov(H,U)
Hvarvector <- c(H41,H42,H43,H44,H45,H46)
DU51 <- cov(DU,DR)
DU52 <- cov(DU,R)
DU53 <- cov(DU,DH)
DU54 <- cov(DU,H)
DU55 <- var(DU)
DU56 <- cov(DU,U)
DUvarvector <-c(DU51,DU52,DU53,DU54,DU55,DU56)
U61 <- cov(U,DR)
U62 <- cov(U,R)
U63 <- cov(U,DH)
U64 <- cov(U,H)
U65 <- cov(U,DU)
U66 <- var(U)
Uvarvector <- c(U61,U62,U63,U64,U65,U66)
vararray <- array(c(DRvarvector,Rvarvector,DHvarvector,Hvarvector,DUvarvector,Uvarvector), dim = c(6,6))
vararray
RDR11 <- cor(DR,DR)
RDR12 <- cor(DR,R)
RDR13 <- cor(DR,DH)
RDR14 <- cor(DR,H)
RDR15 <- cor(DR,DU)
RDR16 <- cor(DR,U)
DRcorvector <- c(RDR11,RDR12,RDR13,RDR14,RDR15,RDR16)
RR21 <- cor(R,DR)
RR22 <- cor(R,R)
RR23 <- cor(R,DH)
RR24 <- cor(R,H)
RR25 <- cor(R,DU)
RR26 <- cor(R,U)
Rcorvector <- c(RR21,RR22,RR23,RR24,RR25,RR26)
RDH31 <- cor(DH,DR)
RDH32 <- cor(DH,R)
RDH33 <- cor(DH,DH)
RDH34 <- cor(DH,H)
RDH35 <- cor(DH,DU)
RDH36 <- cor(DH,U)
DHcorvector <- c(RDH31,RDH32,RDH33,RDH34,RDH35,RDH36)
RH41 <- cor(H,DR)
RH42 <- cor(H,R)
RH43 <- cor(H,DH)
RH44 <- cor(H,H)
RH45 <- cor(H,DU)
RH46 <- cor(H,U)
Hcorvector <- c(RH41,RH42,RH43,RH44,RH45,RH46)
RDU51 <- cor(DU,DR)
RDU52 <- cor(DU,R)
RDU53 <- cor(DU,DH)
RDU54 <- cor(DU,H)
RDU55 <- cor(DU,DU)
RDU56 <- cor(DU,U)
DUcorvector <- c(RDU51,RDU52,RDU53,RDU54,RDU55,RDU56)
RU61 <- cor(U,DR)
RU62 <- cor(U,R)
RU63 <- cor(U,DH)
RU64 <- cor(U,H)
RU65 <- cor(U,DU)
RU66 <- cor(U,U)
Ucorvector <- c(RU61,RU62,RU63,RU64,RU65,RU66)
Rarray <- array(c(DRcorvector,Rcorvector,DHcorvector,Hcorvector,DUcorvector,Ucorvector), dim = c(6,6))
Rarray

##Problem 1.27
table6 <- read.csv(file.choose())
x27 <- table6[,1]
y27 <- table6[,2]
plot(x27,y27)
cor(x27,y27)

##Problem 2.3
library(matlib)
A2.3 <- matrix(c(2,1,1,3), nrow = 2, ncol = 2)
A2.3
transposeA2.3 <- t(A2.3)
transposeA2.3
transposetransposeA2.3 <- t(transposeA2.3)
transposetransposeA2.3
C2.3 <- matrix(c(1,3,4,2), nrow = 2, ncol = 2)
C2.3
transposeC2.3 <- t(C2.3)
transposeC2.3
inversetransposeC2.3 <- inv(transposeC2.3)
inverseC2.3 <- inv(C2.3)
inverseC2.3
transposeinverseC2.3 <- t(inverseC2.3)
inversetransposeC2.3
transposeinverseC2.3
B2.3 <- matrix(c(1,5,4,0,2,3), nrow = 2, ncol = 3)
B2.3
AB2.3 <- A2.3 %*% B2.3
AB2.3
transposeAB2.3 <- t(AB2.3)
transposeB2.3 <- t(B2.3)
multiplytransposeBandA <- transposeB2.3 %*% transposeA2.3
transposeAB2.3
multiplytransposeBandA

##Problem 2.6
matrix2.6 <- matrix(c(9,-2,-2,6), nrow = 2, ncol = 2)
matrix2.6
det(matrix2.6)
eigen(matrix2.6)

##Problem 2.8
matrix2.8 <- matrix(c(1,2,2,-2), nrow = 2, ncol = 2)
matrix2.8
eigen(matrix2.8)

##Problem 2.22
library(matlib)
matrix2.22 <- matrix(c(4,3,8,6,8,-9), nrow = 2, ncol = 3)
matrix2.22
transposematrix2.22 <- t(matrix2.22)
transposematrix2.22
matrix2.22Xtranspose <- matrix2.22 %*% transposematrix2.22
matrix2.22Xtranspose
eigenmatrix2.22Xtranspose <- eigen(matrix2.22Xtranspose)
eigenmatrix2.22Xtranspose
transposeXmatrix2.22 <- transposematrix2.22 %*% matrix2.22
transposeXmatrix2.22
eigentransposeXmatrix2.22 <- eigen(transposeXmatrix2.22)
eigentransposeXmatrix2.22

##Problem 2.25
matrix2.25 <- matrix(c(25,-2,4,-2,4,1,4,1,9), nrow = 3, ncol = 3)
matrix2.25
a11 <- 25
a12 <- -2
a13 <- 4
a21 <- -2
a22 <- 4
a23 <- 1
a31 <- 4
a32 <- 1
a33 <- 9
sqrta11 <- sqrt(a11)
p11 <- a11/(sqrta11*sqrta11)
sqrta22 <- sqrt(a22)
sqrta33 <- sqrt(a33)
p12 <- a12/(sqrta11*sqrta22)
p13 <- a13/(sqrta11*sqrta33)
col1 <- c(p11,p12,p13)
p21 <- a21/(sqrta22*sqrta11)
p22 <- a22/(sqrta22*sqrta22)
p23 <- a23/(sqrta22*sqrta33)
col2 <- c(p21,p22,p23)
p31 <- a31/(sqrta33*sqrta11)
p32 <- a32/(sqrta33*sqrta22)
p33 <- a33/(sqrta33*sqrta33)
col3 <- c(p31,p32,p33)
pdata <- c(col1, col2, col3)
pmatrix2.25 <- matrix(data = pdata, nrow = 3, ncol = 3)
pmatrix2.25
sdcol1 <- c(sqrta11, 0, 0)
sdcol2 <- c(0, sqrta22, 0)
sdcol3 <- c(0,0,sqrta33)
sdp2.25 <- c(sdcol1,sdcol2,sdcol3)
sdpmatrix2.25 <- matrix(data = sdp2.25, nrow = 3, ncol = 3)
sdpmatrix2.25
sdppsdp <- sdpmatrix2.25 %*% pmatrix2.25 %*% sdpmatrix2.25
sdppsdp

##Problem 2.30
X1 <- matrix(c(4,3), nrow = 2, ncol = 1)
X1
Cor1 <- matrix(c(3,0,0,1,2,1,2,0), nrow = 2, ncol = 4)
Cor1
X2 <- matrix(c(2,1), nrow = 2, ncol = 1)
X2
Cor2 <- matrix(c(2,2,1,0,9,-2,-2,4), nrow = 2, ncol = 4)
Cor2
A2.30 <- matrix(c(1,2), nrow = 1, ncol = 2)
A2.30
B2.30 <- matrix(c(1,2,-2,-1), nrow = 2, ncol = 2)
B2.30
A2.30X1 <- A2.30 %*% X1
A2.30X1
B2.30X2 <- B2.30 %*% X2
B2.30X2
X1
A2.30X1
CovX1 <- Cor1
CovX1
CovAx1 <- A2.30 %*% CovX1
CovAx1
X2
B2.30X2
Covx2 <- Cor2
Covx2
CovBx2 <- B2.30 %*% Covx2
CovBx2
CovX1X2 <- c(CovX1,Covx2)
CovX1X2
Covx1x2matrix <- matrix(data = CovX1X2, nrow = 4, ncol = 4)
Covx1x2matrix
