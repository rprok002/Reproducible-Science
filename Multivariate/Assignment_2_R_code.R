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
addcol = meancorcol1 + meancorcol2
addcol
meancorcol3

## Problem 3.14
data3.14 = read.csv(file.choose())
data3.14col1 = data3.14[,1]
data3.14col2 = data3.14[,2]
newcol1 = data3.14col1 * -1
newcol2 = data3.14col2 *2
lincombo1 = newcol1 + newcol2
lincombo1
mean3.14c = mean(lincombo1) 
mean3.14c
var3.14c = var(lincombo1)
newcol1b = data3.14col1 * 2
newcol2b = data3.14col2 * 3
lincombo2 = newcol1b + newcol2b
mean3.14b = mean(lincombo2)
mean3.14b
var3.14b = var(lincombo2)
cov3.14 = cov(lincombo1,lincombo2)
mean3.14c
mean3.14b
var3.14c
var3.14b
cov3.14
meancol1 = mean(data3.14col1)
meancol2 = mean(data3.14col2)
meancol1
meancol2
meanmatrix = matrix(c(5,2), nrow = 2, ncol = 1)
meanmatrix
cinverse = matrix(c(-1, 2), nrow = 1, ncol = 2)
samplemean = cinverse %*% meanmatrix
samplemean
binverse = matrix(c(2,3), nrow = 1, ncol = 2)
samplemean2 = binverse %*% meanmatrix
samplemean2
matrixoriginal = matrix(c(data3.14col1, data3.14col2), nrow = 3, ncol = 2)
matrixoriginal
covoriginal = cov(matrixoriginal)
covoriginal
coriginal = matrix(c(-1,2), nrow = 2, ncol = 1)
boriginal = matrix(c(2,3), nrow = 2, ncol = 1)
cvar2 = cinverse %*% covoriginal %*% coriginal
cvar2
bvar2 = binverse %*% covoriginal %*% boriginal
bvar2
cov2 = binverse %*% covoriginal %*% coriginal
cov2
samplemean
samplemean2
cvar2
bvar2
cov2

##Problem 7
data7 = read.csv(file.choose())
data7col1 = data7[,1]
data7col2 = data7[,2]
data7col3 = data7[,3]
matrix7 = matrix(c(data7col1,data7col2,data7col3), nrow = 5, ncol = 3)
matrix7
covmatrix7 = cov(matrix7)
covmatrix7
cormatrix7 = cor(matrix7)
cormatrix7
s117 = 13.3
s227 = 100.20
s337 = 212.20
D7 = diag(c(s117, s227, s337), nrow = 3, ncol = 3)
D7
D7half = D7^(1/2)
D7half
D7neghalf = D7^(-1/2)
D7neghalf
D7neghalf = diag(c(0.2742, 0.0999, 0.0686), nrow = 3, ncol = 3)
D7neghalf
Cor7 = D7neghalf %*% covmatrix7 %*% D7neghalf
Cor7
Cov7 = D7half %*% cormatrix7 %*% D7half
Cov7
