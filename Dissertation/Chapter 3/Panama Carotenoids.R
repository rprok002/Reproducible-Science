## Carotenoids 

## MANOVA rules####
## Assumptions:
## Independent observations: Use intraclass correlation (ICC) in R to test, value is between -1 (strong neg correlation) and 1 (strong positive correlation)
## Normality: Shapiro-Wilks
## Equal variance: Box M Test

## Will be using one way MANOVA (I think)
## If want to have male/female as group as well, will likely be factorial

## Packages####
## Assumptions
library(car)
install.packages("mvnormtest")
library(mvnormtest)
install.packages("psych")
library(psych)
install.packages("biotools")
library(biotools)
library(rstatix)

## Load dataset, attach ####
Carotenoids < read.csv(file.choose())
attach(Carotenoids)

## One-way MANOVA for Carotenoids: Assumptions, outliers ####
## Independent observations: ICC
ICC(Carotenoids[,4:6]) ## Change columns to have all dependent variables
## Look at absolute correlation values
## Normality: Shapiro-Wilks
## Separate out dependent variables into one dataframe and transpose
dependentcarotenoids <- data.frame(data$variable1, data$variable2, continue)
transpose_dependentcarotenoids <- t(dependentcarotenoids)
## Run Shapiro
mshapiro.test(transpose_dependentcarotenoids)
## Variance/Covariance Positive Determinant: needs to be positive
Covdependentcarotenoids <- cov(dependentcarotenoids)
det(Covdependentcarotenoids)

## Equality of Variance Between Groups Control and Infected
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY!!!!!!!!!
group <- c("Infected","Infected","Infected","Infected", "Control", "Control",
           "Control", "Infected","Infected","Infected","Infected","Infected",
           "Control", "Infected","Infected","Infected","Infected","Infected",
           "Infected","Infected","Infected","Infected","Infected","Infected",
           "Control","Infected","Infected","Infected","Infected","Infected",
           "Control", "Control", "Control", "Infected", "Control", "Infected")
factor(group)
group
## Run BoxM
boxM(dependentcarotenoids, group)

## Outliers
mahalanobis_distance(data = dependentcarotenoids)$is.outlier

## Linearity

## Multicoliearity, <0.9 no multicolinearity
cor.test(x = Carotenoids$variable1, y = df$canopy_vol, method = "pearson")$estimate

## MANOVA ####
## Make group (Control and Infected) categorical variables
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY
group = factor(group)
## Bind dependent variables together into one vector
Y = cbind(variable1, variable2, variable3, variable4, more)
## Run MANOVA
MANOVA = manova(Y-group)
## Run Tests on MANOVA
summary(MANOVA, test = "Wilks")
summary(MANOVA, test = "Pillai")
summary(MANOVA, test = "Hotelling-Lawley")
summary(MANOVA, test = "Roy")