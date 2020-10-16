matrix = matrix(c(4,1,1,7,3,2), nrow = 2, ncol = 3)
matrix2 = matrix(c(2,4,1,2,3,1), nrow = 3, ncol = 2)
matrix
matrix2
matrix %*% matrix2
matrix3 = matrix %*% matrix2
rankMatrix(matrix3)
library(Matrix)
rankMatrix(matrix3)
det(matrix3)
matrix = matrix(c(1,1,1,1,4,-2), nrow = 3, ncol = 2)
matrix
cov(matrix)
devmatrix = matrix(c(0,3,-3), nrow = 3, ncol = 1)
devmatrix
library(matlib)
devmatrixinv = t(devmatrix)
devmatrix %*% devmatrixinv
devmatrixinv
S = matrix(c(9,5,-6,5,16,3,-6,3,25), nrow = 3, ncol = 3)
S
D = matrix(c(3,0,0,0,4,0,0,0,5), nrow = 3, ncol = 3)
D  
D2 = D^-0.5
D2
D2 = matrix(c(0.577,0,0,0,0.5,0,0,0,0.447), nrow = 3, ncol = 3)
D2
R = D2 %*% S %*% D2
R
matrix4 = matrix(c(8,1,1), nrow = 3, ncol = 1)
mean(matrix4)
meanmatrix4 = matrix(c(3.333, 3.333, 3.333), nrow = 3, ncol = 1)
ctranspose = matrix(c(2,-1,1), nrow = 1, ncol = 3)
ctranspose
ctranspose %*% meanmatrix4
cov4 = matrix(c(4,0,1,0,2,2,1,2,3), nrow = 3, ncol = 3)
cov4
c = matrix(c(2,-1,1), nrow = 3, ncol = 1)
ctranspose %*% cov4 %*% c
pnorm(c(13,14,15,16,17,18,19,20,21,22,23), mean = 6.666, sd = sqrt(21))
1/40 * cov4
A = matrix(c(3,7,7,4), ncol = 2, nrow = 2)
A
eigen(A)
R = cov2cor(S)
R
eigen(S)
ts = t(S)
ts
mm = cbind(0.5,0.54,0.84,1.14,1.15,1.19,1.42,1.74,2.25,2.43,2.77,3.07,3.10,3.34,3.59,3.68,4.69,4.71,4.92)
qchisq(0.90,2)
length(mm[mm<4.60517])
matrix5A = matrix(c(1,-2), nrow = 1, ncol = 2)
matrix5 = matrix(c(3,2), nrow = 2, ncol = 1)
matrix5A %*% matrix5
cov5 = matrix(c(4,2,2,5), nrow = 2, ncol = 2)
transposematrix5A = tmatrix(c(1,-2), nrow = 2, ncol = 1)
matrix5A %*% cov5 %*% transposematrix5A
transposematrix5A
trace(S)
