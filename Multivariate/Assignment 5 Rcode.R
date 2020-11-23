## Problme 1: Exercise 8.1 amd 8.2
## Exercise 8.1
cov8.1 = matrix(c(5,2,2,2), nrow = 2, ncol = 2)
cov8.1
eigen8.1 = eigen(cov8.1)
eigen8.1
## Y1 = e1'X = -0.8944X1 - 0.4472X2
## Y2 = e2'X = 0.4472X1 - 0.8944X2
totalvar8.1 = 5+2
propfirst8.1 = 6/7
propfirst8.1
## first component accounts for 85.7% of variance
##Exercise 8.2