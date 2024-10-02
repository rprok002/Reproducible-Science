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
install.packages("GGally")
library(GGally)
install.packages("MASS")
library(MASS)
library(dplyr)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
install.packages("npmv")
library(npmv)
## Load dataset, attach, subset ####
Carotenoids <- read.csv(file.choose())
attach(Carotenoids)
Carotenoidsoriginal <- Carotenoids[,c(3, 5, 7,9,11,13,15,17,19,21,23,25,27,29,31,33,35)]
Carotenoidssqrt <- Carotenoids[,c(4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36)]
## One-way MANOVA for Carotenoids: Assumptions, outliers ####
## Independent observations: ICC
ICC(Carotenoidsoriginal) ## Change columns to have all dependent variables
## Look at absolute correlation values
## Value is 0.086, very close to zero and so correlation not an issue
ICC(Carotenoidssqrt)
## Value is 0.17, still fairly non correlated
## Normality: Shapiro-Wilks
## Separate out dependent variables into one dataframe and transpose
dependentcarotenoids <- data.frame(Carotenoids$Apocarotenoid, Carotenoids$Canary.xanthophyll, Carotenoids$Canthaxanthin,
                                   Carotenoids$Xanthophyll, Carotenoids$Echinenone, Carotenoids$cis.ketocarotenoid,
                                   Carotenoids$X3.hydroxy.echinenone, Carotenoids$lutein.ester.1, Carotenoids$canary.xanthophyll.ester.1,
                                   Carotenoids$beta.carotene, Carotenoids$canary.xanthophyll.ester.2, Carotenoids$ketocarotenoid.ester.1,
                                   Carotenoids$ketocarotenoid.ester.2, Carotenoids$canary.xanthophyll.ester.3, Carotenoids$canthaxanthin.ester,
                                   Carotenoids$X3HE.ester, Carotenoids$ketocarotenoid.ester.3)
dependentcarotenoidssqrt <- data.frame(Carotenoids$sqrtApocarotenoid, Carotenoids$sqrtCanary.xanthophyll, Carotenoids$sqrtCanthaxanthin,
                                       Carotenoids$sqrtXanthophyll, Carotenoids$sqrtEchinenone, Carotenoids$sqrtcis.ketocarotenoid,
                                       Carotenoids$sqrt3.hydroxy.echinenone, Carotenoids$sqrtlutein.ester.1, Carotenoids$sqrtcanary.xanthophyll.ester.1,
                                       Carotenoids$sqrtbeta.carotene, Carotenoids$sqrtcanary.xanthophyll.ester.2, Carotenoids$sqrtketocarotenoid.ester.1,
                                       Carotenoids$sqrtketocarotenoid.ester.2, Carotenoids$sqrtcanary.xanthophyll.ester.3, Carotenoids$sqrtcanthaxanthin.ester,
                                       Carotenoids$sqrt3HE.ester, Carotenoids$sqrtketocarotenoid.ester.3)
transpose_dependentcarotenoids <- t(dependentcarotenoids)
transpose_dependentcarotenoidssqrt <- t(dependentcarotenoidssqrt)
## Run Shapiro
mshapiro.test(transpose_dependentcarotenoids)
## data are not normal
mshapiro.test(transpose_dependentcarotenoidssqrt)
## data still not normal
## GG plots for original data versus sqrt
ggqqplot(Carotenoids, "Apocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtApocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "Canary.xanthophyll", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "sqrtCanary.xanthophyll", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "Canthaxanthin", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "Xanthophyll", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtCanthaxanthin", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "Echinenone", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtEchinenone", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "lutein.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtlutein.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "cis.ketocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtcis.ketocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "X3.hydroxy.echinenone", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "sqrt3.hydroxy.echinenone", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "canary.xanthophyll.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtcanary.xanthophyll.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "beta.carotene", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtbeta.carotene", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "canary.xanthophyll.ester.2", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtcanary.xanthophyll.ester.2", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "ketocarotenoid.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a couple off points but mainly fine
ggqqplot(Carotenoids, "sqrtketocarotenoid.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "ketocarotenoid.ester.2", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "sqrtketocarotenoid.ester.2", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "canary.xanthophyll.ester.3", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "sqrtcanary.xanthophyll.ester.3", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "canthaxanthin.ester", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "sqrtcanthaxanthin.ester", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "X3HE.ester", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "sqrt3HE.ester", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps a little
ggqqplot(Carotenoids, "ketocarotenoid.ester.3", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "sqrtketocarotenoid.ester.3", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps

## though shapiro test positive the qqplots look fine so going to move forward with that

## Variance/Covariance Positive Determinant: needs to be positive
Covdependentcarotenoids <- cov(dependentcarotenoids)
det(Covdependentcarotenoids)
## very positive, looks good
Covdependentcarotenoidssqrt <- cov(dependentcarotenoidssqrt)
det(Covdependentcarotenoidssqrt)
## positive determinant thought not very positive like the original data

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
## need to choose 8 carotenoids to look at only because only 9 control frogs

## Outliers
## Subset by group

mahalanobis_distance(data = dependentcarotenoids)$is.outlier
## No outliers in the data

mahalanobis_distance(data = dependentcarotenoidssqrt)$is.outlier
## No outliers in sqrt data

## Linearity and multicollinearity
## By FrogType group, no sqrt
cor.matoriginal <- Carotenoidsoriginal %>% cor_mat()
cor.matoriginal


## Apocarotenoid and Canary.Xanthophyll
## Apocarotenoid and canary.xanthophyll ester 1
## Canary Xanthophyll and canary xanthophyll ester 1
## cis ketocarotenoid and X3 hydroxy echinenone
## cis ketocarotenoid and canary xanthophyll ester 2
## X3 hydroxy echinenone and lutein ester 1
## X3 hydroxy echinenone and beta carotene
## X3 hydroxy echinenone and canary xanthophyll ester 2
## lutein ester 1 and beta carotene
## canary xanthophyll ester 1 and canary xanthophyll ester 2
## canthaxanthin ester has low correlations so going to use canthaxanthin pure instead

cor.matsqrt <- Carotenoidssqrt %>% cor_mat()
cor.matsqrt
## sqrtApocarotenoid and sqrtCanary.Xanthophyll
## sqrtApocarotenoid and sqrtcanary.xanthophyll ester 1
## sqrtcis ketocarotenoid and sqrt3 hydroxy echinenone
## sqrtcis ketocarotenoid and sqrtcanary xanthophyll ester 2
## sqrtX3 hydroxy echinenone and sqrtcanary xanthophyll ester 2
## sqrtlutein ester 1 and sqrtbeta carotene
## sqrtcanary xanthophyll ester 1 and sqrtcanary xanthophyll ester 2
## sqrtcanthaxnthin ester has better correlations, still going to just use canthaxanthin

library(GGally)
results1 <- Carotenoids %>%
  select(Apocarotenoid, Canary.xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results1
results1$plots

results1a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtCanary.xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results1a
results1a$plots

results2 <- Carotenoids %>%
  select(Apocarotenoid, Canthaxanthin, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results2
results2$plots

results2a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtCanthaxanthin, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results2a
results2a$plots

results3 <- Carotenoids %>%
  select(Apocarotenoid, Xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results3
results3$plots

results3a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtXanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results3a
results3a$plots
## now control are correlated

results4 <- Carotenoids %>%
  select(Apocarotenoid, Echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results4
results4$plots

results4a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtEchinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results4a
results4a$plots

results5 <- Carotenoids %>%
  select(Apocarotenoid, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results5
results5$plots

results5a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results5a
results5a$plots

results6 <- Carotenoids %>%
  select(Apocarotenoid, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results6
results6$plots

results6a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrt3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results6a
results6a$plots


results7 <- Carotenoids %>%
  select(Apocarotenoid, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results7
results7$plots

results7a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results7a
results7a$plots

results8 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results8
results8$plots
## correlated

results8a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results8a
results8a$plots
## still correlated, helps a little bit

results9 <- Carotenoids %>%
  select(Apocarotenoid, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results9
results9$plots

results9a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results9a
results9a$plots

results10 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results10
results10$plots
## control correlated

results10a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results10a
results10a$plots
## control correlated, but better

results11 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results11
results11$plots

results11a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results11a
results11a$plots

results12 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results12
results12$plots

results12a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results12a
results12a$plots

results13 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results13
results13$plots

results13a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results13a
results13a$plots

results14 <- Carotenoids %>%
  select(Apocarotenoid, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results14
results14$plots

results14a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results14a
results14a$plots

results15 <- Carotenoids %>%
  select(Apocarotenoid, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results15
results15$plots

results15a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results15a
results15a$plots

results16 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results16
results16$plots

results16a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results16a
results16a$plots

results17 <- Carotenoids %>%
  select(Canary.xanthophyll, Canthaxanthin, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results17
results17$plots

results17a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtCanthaxanthin, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results17a
results17a$plots

results18 <- Carotenoids %>%
  select(Canary.xanthophyll, Xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results18
results18$plots
## control correlated

results18a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtXanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results18a
results18a$plots
## control correlated, doesn't really help

results19 <- Carotenoids %>%
  select(Canary.xanthophyll, Echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results19
results19$plots

results19a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtEchinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results19a
results19a$plots

results20 <- Carotenoids %>%
  select(Canary.xanthophyll, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results20
results20$plots

results20a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results20a
results20a$plots

results21 <- Carotenoids %>%
  select(Canary.xanthophyll, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results21
results21$plots

results21a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrt3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results21a
results21a$plots

results22 <- Carotenoids %>%
  select(Canary.xanthophyll, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results22
results22$plots

results22a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results22a
results22a$plots

results23 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results23
results23$plots
## correlated, likely will have to use only one of them because they are all derivations of each other

results23a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcanary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results23a
results23a$plots
## fixes correlation here

results24 <- Carotenoids %>%
  select(Canary.xanthophyll, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results24
results24$plots

results24a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results24a
results24a$plots

results25 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results25
results25$plots

results25a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results25a
results25a$plots

results26 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results26
results26$plots

results26a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results26a
results26a$plots

results27 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results27
results27$plots

results27a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results27a
results27a$plots

results28 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.)+ theme_bw(), result = "plots")
results28
results28$plots

results28a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results28a
results28a$plots

results29 <- Carotenoids %>%
  select(Canary.xanthophyll, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results29
results29$plots

results29a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results29a
results29a$plots

results30 <- Carotenoids %>%
  select(Canary.xanthophyll, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results30
results30$plots

results30a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results30a
results30a$plots

results31 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results31
results31$plots

results31a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results31a
results31a$plots

results32 <- Carotenoids %>%
  select(Canthaxanthin, Xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results32
results32$plots

results32a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtXanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results32a
results32a$plots

results33 <- Carotenoids %>%
  select(Canthaxanthin, Echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results33
results33$plots

results33a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtEchinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results33a
results33a$plots

results34 <- Carotenoids %>%
  select(Canthaxanthin, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results34
results34$plots

results34a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results34a
results34a$plots

results35 <- Carotenoids %>%
  select(Canthaxanthin, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results35
results35$plots

results35a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrt3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results35a
results35a$plots

results36 <- Carotenoids %>%
  select(Canthaxanthin, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results36
results36$plots

results36a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results36a
results36a$plots

results37 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results37
results37$plots

results37a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcanary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results37a
results37a$plots

results38 <- Carotenoids %>%
  select(Canthaxanthin, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results38
results38$plots

results38a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results38a
results38a$plots

results39 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results39
results39$plots

results39a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results39a
results39a$plots
## sqrt makes correlation significant for control

results40 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results40
results40$plots
## control correlated

results40a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results40a
results40a$plots
## control correlated

results41 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results41
results41$plots

results41a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results41a
results41a$plots


results42 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results42
results42$plots

results42a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results42a
results42a$plots

results43 <- Carotenoids %>%
  select(Canthaxanthin, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results43
results43$plots

results43a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results43a
results43a$plots

results44 <- Carotenoids %>%
  select(Canthaxanthin, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results44
results44$plots

results44a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results44a
results44a$plots

results45 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results45
results45$plots
## control correlated

results45a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results45a
results45a$plots
## control correlated

results46 <- Carotenoids %>%
  select(Xanthophyll, Echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results46
results46$plots

results46a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtEchinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results46a
results46a$plots

results47 <- Carotenoids %>%
  select(Xanthophyll, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results47
results47$plots

results47a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results47a
results47a$plots

results48 <- Carotenoids %>%
  select(Xanthophyll, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results48
results48$plots

results48a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrt3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results48a
results48a$plots

results49 <- Carotenoids %>%
  select(Xanthophyll, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results49
results49$plots

results49a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results49a
results49a$plots

results50 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results50
results50$plots

results50a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results50a
results50a$plots

results51 <- Carotenoids %>%
  select(Xanthophyll, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results51
results51$plots

results51a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results51a
results51a$plots

results52 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results52
results52$plots

results52a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results52a
results52a$plots

results53 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results53
results53$plots

results53a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results53a
results53a$plots

results54 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results54
results54$plots

results54a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results54a
results54a$plots

results55 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results55
results55$plots

results55a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results55a
results55a$plots

results56 <- Carotenoids %>%
  select(Xanthophyll, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results56
results56$plots

results56a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results56a
results56a$plots

results57 <- Carotenoids %>%
  select(Xanthophyll, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results57
results57$plots

results57a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results57a
results57a$plots

results58 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results58
results58$plots

results58a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results58a
results58a$plots

results59 <- Carotenoids %>%
  select(Echinenone, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results59
results59$plots

results59a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results59a
results59a$plots

results60 <- Carotenoids %>%
  select(Echinenone, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results60
results60$plots

results60a <- Carotenoids %>%
  select(sqrtEchinenone, sqrt3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results60a
results60a$plots

results61 <- Carotenoids %>%
  select(Echinenone, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results61
results61$plots

results61a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results61a
results61a$plots

results62 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results62
results62$plots

results62a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results62a
results62a$plots


results63 <- Carotenoids %>%
  select(Echinenone, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results63
results63$plots

results63a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results63a
results63a$plots

results64 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results64
results64$plots

results64a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results64a
results64a$plots

results65 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results65
results65$plots

results65a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results65a
results65a$plots

results66 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results66
results66$plots

results66a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results66a
results66a$plots

results67 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results67
results67$plots

results67a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results67a
results67a$plots

results68 <- Carotenoids %>%
  select(Echinenone, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results68
results68$plots

results68a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results68a
results68a$plots

results69 <- Carotenoids %>%
  select(Echinenone, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results69
results69$plots

results69a <- Carotenoids %>%
  select(sqrtEchinenone, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results69a
results69a$plots

results70 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results70
results70$plots

results70a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results70a
results70a$plots

results71 <- Carotenoids %>%
  select(cis.ketocarotenoid, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results71
results71$plots
## correlated

results71a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrt3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results71a
results71a$plots

results72 <- Carotenoids %>%
  select(cis.ketocarotenoid, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results72
results72$plots

results72a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results72a
results72a$plots

results73 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results73
results73$plots

results73a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results73a
results73a$plots

results74 <- Carotenoids %>%
  select(cis.ketocarotenoid, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results74
results74$plots

results74a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results74a
results74a$plots

results75 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results75
results75$plots
## correlated

results75a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results75a
results75a$plots

results76 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results76
results76$plots

results76a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results76a
results76a$plots

results77 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results77
results77$plots

results77a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results77a
results77a$plots

results78 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results78
results78$plots

results78a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results78a
results78a$plots

results79 <- Carotenoids %>%
  select(cis.ketocarotenoid, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results79
results79$plots

results79a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results79a
results79a$plots

results80 <- Carotenoids %>%
  select(cis.ketocarotenoid, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results80
results80$plots

results80a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results80a
results80a$plots

results81 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results81
results81$plots

results81a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results81a
results81a$plots


results82 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results82
results82$plots
## correlated

results82a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results82a
results82a$plots

results83 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results83
results83$plots

results83a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtcanary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results83a
results83a$plots

results84 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results84
results84$plots
## correlated

results84a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results84a
results84a$plots

results85 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results85
results85$plots
## correlated

results85a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone,sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results85a
results85a$plots

results86 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results86
results86$plots

results86a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results86a
results86a$plots

results87 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results87
results87$plots

results87a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results87a
results87a$plots

results88 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results88
results88$plots

results88a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results88a
results88a$plots

results89 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results89
results89$plots

results89a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results89a
results89a$plots

results90 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results90
results90$plots

results90a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results90a
results90a$plots

results91 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results91
results91$plots

results91a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results91a
results91a$plots

results92 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results92
results92$plots

results92a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtcanary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results92a
results92a$plots


results93 <- Carotenoids %>%
  select(lutein.ester.1, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results93
results93$plots
## correlated

results93a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results93a
results93a$plots

results94 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results94
results94$plots

results94a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results94a
results94a$plots

results95 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results95
results95$plots

results95a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results95a
results95a$plots

results96 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results96
results96$plots

results96a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results96a
results96a$plots

results97 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results97
results97$plots

results97a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results97a
results97a$plots

results98 <- Carotenoids %>%
  select(lutein.ester.1, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results98
results98$plots

results98a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results98a
results98a$plots

results99 <- Carotenoids %>%
  select(lutein.ester.1, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results99
results99$plots

results99a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results99a
results99a$plots

results100 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results100
results100$plots

results100a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results100a
results100a$plots

results101 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results101
results101$plots

results101a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results101a
results101a$plots

results102 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results102
results102$plots
## correlated

results102a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results102a
results102a$plots

results103 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results103
results103$plots

results103a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results103a
results103a$plots

results104 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results104
results104$plots

results104a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results104a
results104a$plots

results105 <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results105
results105$plots

results105a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results105a
results105a$plots

results106 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results106
results106$plots

results106a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results106a
results106a$plots

results107 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results107
results107$plots

results107a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results107a
results107a$plots

results108 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results108
results108$plots

results108a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results108a
results108a$plots

results109 <- Carotenoids %>%
  select(beta.carotene, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results109
results109$plots
## correlated

results109a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results109a
results109a$plots

results110 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results110
results110$plots

results110a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results110a
results110a$plots

results111 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results111
results111$plots

results111a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results111a
results111a$plots

results112 <- Carotenoids %>%
  select(beta.carotene, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results112
results112$plots

results113 <- Carotenoids %>%
  select(beta.carotene, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results113
results113$plots

results113a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results113a
results113a$plots

results114 <- Carotenoids %>%
  select(beta.carotene, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results114
results114$plots

results114a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results114a
results114a$plots

results115 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results115
results115$plots

results115a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results115a
results115a$plots

results116 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results116
results116$plots

results117 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results117
results117$plots

results117a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results117a
results117a$plots

results118 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results118
results118$plots

results118a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results118a
results118a$plots

results119 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results119
results119$plots

results119a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results119a
results119a$plots

results120 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results120
results120$plots

results120a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results120a
results120a$plots

results121 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results121
results121$plots

results121a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results121a
results121a$plots

results122 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results122
results122$plots

results122a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results122a
results122a$plots

results123 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results123
results123$plots

results123a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results123a
results123a$plots

results124 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results124
results124$plots

results124a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results124a
results124a$plots

results125 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results125
results125$plots

results125a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results125a
results125a$plots

results126 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results126
results126$plots

results126a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results126a
results126a$plots

results127 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results127
results127$plots

results127a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.2, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results127a
results127a$plots

results128 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results128
results128$plots

results128a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.2, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results128a
results128a$plots

results129 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results129
results129$plots

results129a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.2, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results129a
results129a$plots

results130 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results130
results130$plots

results130a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.2, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results130a
results130a$plots

results131 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results131
results131$plots

results131a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.3, sqrtcanthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results131a
results131a$plots

results132 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results132
results132$plots

results132a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.3, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results132a
results132a$plots

results133 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results133
results133$plots

results133a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.3, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results133a
results133a$plots

results134 <- Carotenoids %>%
  select(canthaxanthin.ester, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results134
results134$plots

results134a <- Carotenoids %>%
  select(sqrtcanthaxanthin.ester, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results134a
results134a$plots

results135 <- Carotenoids %>%
  select(canthaxanthin.ester, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results135
results135$plots

results135a <- Carotenoids %>%
  select(sqrtcanthaxanthin.ester, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results135a
results135a$plots

results136 <- Carotenoids %>%
  select(X3HE.ester, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results136
results136$plots

results136a <- Carotenoids %>%
  select(sqrt3HE.ester, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results136a
results136a$plots

## By FrogType group, sqrt

## Multivariate tests of normality ####



## MANOVA ####
## Make group (Control and Infected) categorical variables
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY
group = factor(group)
## Bind dependent variables together into one vector
Y = cbind(variable1, variable2, variable3, variable4, more)
## Run MANOVA
MANOVA = manova(Y-group)
## Run Tests on MANOVA, check if p values are below 0.05
summary(MANOVA, test = "Wilks")
summary(MANOVA, test = "Pillai")
summary(MANOVA, test = "Hotelling-Lawley")
summary(MANOVA, test = "Roy")

## Wilks Lambda partial eta squares values
## Take smaller of two values, number of outcome variables (here, 16) or df of group variable = b
## formula: 1-(0.63202)^(1/b)
## Answer will be percentage of variance between outcome variables is due to difference in group variable



## Factorial MANOVA ####
group <- factor(Frog.Type)
group
sex <- factor(Sex)
sex

FACTORIALSETUP <- lm(dependentcarotenoids~Frog.Type + Sex+Frog.Type*Sex -1, data = Carotenoids)
summary(FACTORIALSETUP)
FACTORIALMANOVAwilks <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Wilks"))
FACTORIALMANOVApillai <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Pillai"))
FACTORIALMANOVAhotelling <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Hotelling-Lawley"))
FACTORIALMANOVAroy <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Roy"))
## If interactions aren't significant, can use Type II
FACTORIALMANOVAwilks <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Wilks"))
FACTORIALMANOVApillai <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Pillai"))
FACTORIALMANOVAhotelling <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Hotelling-Lawley"))
FACTORIALMANOVAroy <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Roy"))

## Wilks Lambda partial eta squares values
## For factorial, separated by group
## Take smaller of two values, number of outcome variables (here, 16) or df of group variable = b
## formula: 1-(0.63202)^(1/b)
## Answer will be percentage of variance between outcome variables is due to difference in group variable

## nonparametric multivariate
nonpartest(weight|bot|fungi|rating~treatment,sberry,permreps=1000)
