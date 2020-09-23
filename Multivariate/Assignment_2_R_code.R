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

##Problem 8
inversemeanmatrix8 = matrix(c(2,3,-4,5), nrow = 4, ncol = 1)
inversemeanmatrix8
linearmatrixa = matrix(c(1,2,-1,2), nrow = 1, ncol = 4)
linearmatrixa
meandistr8 = linearmatrixa %*% inversemeanmatrix8
meandistr8
inverselinearmatrixa = matrix(c(1,2,-1,2), nrow = 4, ncol = 1)
inverselinearmatrixa
covmatrix8 = matrix(c(4,5,0,0,5,7,2,3,0,2,4,0,0,3,0,4), nrow = 4, ncol = 4)
covmatrix8
covdistr8 = linearmatrixa %*% covmatrix8 %*% inverselinearmatrixa
covdistr8
a = exp(-(((13-22)/sqrt(88))^2)/2)
a
b = 1/(sqrt(2*3.14159*88))
b
prob = a*b
prob

## Problem 9
meana3 = 104.0
meana4 = 93.8
cova = matrix(c(2624,210.30,210.30,177.36), nrow = 2, ncol = 2)
cova
inverscova = Ginv(cova)
inverscova
meana = meana3 + (210.30*0.00623031*(meana4-meana4))
meana
cova9 = 2624-(210.30*0.00623031*210.30)
cova9
meanb1 = matrix(c(172.7), nrow = 1, ncol = 1)
meanb1
meanb234 = matrix(c(104.6,104.0,93.8), nrow = 3, ncol = 1)
meanb234
covb11 = matrix(c(1037.21), nrow = 1, ncol = 1)
covb11
covb12 = matrix(c(-80.02,1430.70,271.44),nrow = 1, ncol = 3)
covb12
covb21 = matrix(c(-80.02, 1430.70, 271.44), nrow = 3, ncol = 1)
covb21
covb22 = matrix(c(219.84, 92.10, -91.58, 92.10, 2624.00, 210.30, -91.58, 210.30, 177.36), nrow = 3, ncol = 3)
covb22
meanb2 = mean(meanb234)
meanb2
xminusmean = meanb234 - meanb2
xminusmean
inversecovb22 = Ginv(covb22)
inversecovb22
condmeanb = meanb1 + (covb12 %*% inversecovb22 %*% xminusmean)
condmeanb
condcovb = covb11 - (covb12 %*% inversecovb22 %*% covb21)
condcovb
## Problem 13
data213 = read.csv(file.choose())
data213col1 = data213[,1]
data213col1
qqnorm(data213col1)
qqline(data213col1)
x = data213col1[order(data213col1)]
n = seq(1:9)
q = qnorm ((n-.5)/length(x))
cor(q,x)

## Problem 14: Exercise 4.26
matrix4.26 = matrix(c(1,2,3,4,5,6,7,8,9,10, 18.95,19.00,17.95,15.54,14.00,12.95,8.94,7.49,6.00,3.99), nrow = 10, ncol = 2)
matrix4.26
cov4.26 = cov(matrix4.26)
cov4.26
inversecov4.26 = Ginv(cov4.26)
inversecov4.26
?solve
??inv
library(matlib)
matrix4.26col1 = matrix4.26[,1]
matrix4.26col1
mean4.26col1 = mean(matrix4.26col1)
mean4.26col1
x1minusxbar = matrix4.26col1 - mean4.26col1
x1minusxbar
matrix4.26col2 = matrix4.26[,2]
matrix4.26col2
mean4.26col2 = mean(matrix4.26col2)
mean4.26col2
x2minusxbar = matrix4.26col2 - mean4.26col2
x2minusxbar
newmatrix4.26 = matrix(c(x1minusxbar, x2minusxbar), nrow = 10, ncol = 2)
newmatrix4.26
transposenew = t(newmatrix4.26)
dis1 = transposenew[,1] %*% inversecov4.26 %*% newmatrix4.26[1,]
dis1
dis2 = transposenew[,2] %*% inversecov4.26 %*% newmatrix4.26[2,]
dis3 = transposenew[,3] %*% inversecov4.26 %*% newmatrix4.26[3,]
dis4= transposenew[,4] %*% inversecov4.26 %*% newmatrix4.26[4,]
dis5 = transposenew[,5] %*% inversecov4.26 %*% newmatrix4.26[5,]
dis6 = transposenew[,6] %*% inversecov4.26 %*% newmatrix4.26[6,]
dis7 = transposenew[,7] %*% inversecov4.26 %*% newmatrix4.26[7,]
dis8 = transposenew[,8] %*% inversecov4.26 %*% newmatrix4.26[8,]
dis9 = transposenew[,9] %*% inversecov4.26 %*% newmatrix4.26[9,]
dis10 = transposenew[,10] %*% inversecov4.26 %*% newmatrix4.26[10,]
distances = data.frame(c(dis1, dis2, dis3, dis4, dis5, dis6, dis7, dis8, dis9,dis10))
distances
J = seq(1:10)
qcp = qchisq((10-J+.5)/10, 2)
qcp
distancescol1 = distances[,1]
distanceorder = distancescol1[order(distancescol1)]
distanceorder
qcporder = qcp[order(qcp)]
qcporder
plot(qcporder,distanceorder)
line = abline(0,1)
