## Problem 1: Exercise 3.2
data1 = read.csv(file.choose())
p1 = data1[,1]
p2 = data1[,2]
plot(p1,p2)
meanp1 = mean(p1)
meanp2 = mean(p2)
data1 = read.csv(file.choose())
p1 = data1[,1]
p2 = data1[,2]
plot(p1,p2)
invcosangle = acos(-0.866)
degreesangle = invcosangle*(180/3.1415)
matrixSn3.2 = matrix(c(2,-3,-3,6), nrow = 2, ncol = 2)
matrixSn3.2
corrcoeff3.2 = matrix(c(1,-0.86,-0.86,1), nrow = 2, ncol = 2)
corrcoeff3.2

## Problem 2: 
meanmatrix = matrix(c(12,14,10), nrow = 3, ncol = 1)
meanmatrix
mean(meanmatrix)
cormatrix = matrix(c(20,-80,140,-80,40,60,140,60,100), nrow = 3, ncol = 3)
cormatrix
cormatrix*(1/3)

## Problem 4: Exercise 3.9
matrix3.9 = matrix(c(12,18,14,20,16,17,20,16,18,19,29,38,30,38,35), nrow = 5, ncol = 3)
matrix3.9
matrix3.9col1 = matrix3.9[,1]
matrix3.9col2 = matrix3.9[,2]
matrix3.9col3 = matrix3.9[,3]
meancol1 = mean(matrix3.9col1)
meancol2 = mean(matrix3.9col2)
meancol3 = mean(matrix3.9col3)
meancorcol1 = matrix3.9col1-meancol1
meancorcol1
meancorcol2 = matrix3.9col2-meancol2
meancorcol2
meancorcol3 = matrix3.9col3-meancol3
meancorcol3
meancormatrix= matrix(c(meancorcol1,meancorcol2,meancorcol3), nrow = 5, ncol = 3)
meancormatrix
covmatrix3.9 = cov(meancormatrix)
covmatrix3.9
library(plm)
library(r2glmm)
calc_sgv(nblocks = 1, blksizes = 3, covmatrix3.9)
eigvector= matrix(c(1,1,-1), nrow = 3, ncol = 1)
eigvector
Sa = covmatrix3.9 %*% eigvector
Sa
