## Carotenoids 

## MANOVA rules####
## Assumptions:
## Independent observations: Use intraclass correlation (ICC) in R to test, value is between -1 (strong neg correlation) and 1 (strong positive correlation)
## Normality: Shapiro-Wilks
## Equal variance: Box M Test

## Will be using one way MANOVA (I think)
## If want to have male/female as group as well, will likely be factorial

## Packages####

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
library(broom)
install.packages("npmv")
library(npmv)
install.packages("MVN")
library(MVN)
install.packages("effectsize")
library(effectsize)
install.packages ("rgl")
library(rgl)
install.packages("heplots")
library(heplots)
install.packages("effects")
library(effects)
library(tidyverse)
library(gridExtra)
library(grid)
install.packages("gridtext")
library(gridtext)
install.packages("Hmisc")
library(Hmisc)

## Load dataset, attach, subset ####
Carotenoids <- read.csv(file.choose())
attach(Carotenoids)
Carotenoidsoriginal <- Carotenoids[,c(3, 5, 7,9,11,13,15,17,19,21,23,25,27,29,31,33,35)]
Carotenoidssqrt <- Carotenoids[,c(4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36)]
## Assumptions: independent, normality ####
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

## Assumptions: Variance/Covariance Positive Determinant: needs to be positive ####
Covdependentcarotenoids <- cov(dependentcarotenoids)
det(Covdependentcarotenoids)
## very positive, looks good
Covdependentcarotenoidssqrt <- cov(dependentcarotenoidssqrt)
det(Covdependentcarotenoidssqrt)
## positive determinant thought not very positive like the original data

## Assumptions: Equality of Variance Between Groups Control and Infected ####
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

mahalanobis_distance(data = dependentcarotenoidsnew)$is.outlier
## No outliers in the data

mahalanobis_distance(data = dependentcarotenoidssqrt)$is.outlier
## No outliers in sqrt data

## Assumptions: multicollinearity ####
cor.matoriginal <- Carotenoidsoriginal %>% cor_mat()
cor.matoriginal

cor.matoriginal2 <- Carotenoidssqrt %>% cor_mat()
cor.matoriginal2

cororigincal<- cor(Carotenoidsoriginal)

## p-values
cororiginalp <- rcorr(as.matrix(Carotenoidsoriginal))
cororiginalp
cororiginalp$p

## 1.Xanthophyll, canary xanthophyll, canary xanthophyll ester 1, canary xanthophyll ester 2, canary xanthophyll ester 3 and lutein ester correlated, so summing (though lutein and canary xan not correlated)
## 2.Canthaxanthin, canthaxanthin ester, cis. ketocarotenoid, ketocarotenoid ester 1, ketocarotenoid ester 2 and ketocarotenoid ester 3 correlated, so summing
## 3.Echinenone, x3hE hydroxy echinenone and e3hE ester correlated, summing
## 4.Apocarotenoid and beta carotene
## Kevin says set 2 and 3 could be combined because all ketocarotenoids but echinenone and keto1 not correlated, so leaving like this for now



## Assumptions: linearity by FrogType group, no sqrt and sqrt ####
library(GGally)
results1 <- Carotenoids %>%
  select(Apocarotenoid, Canary.xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results1
results1$plots
## correlated both

results1a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtCanary.xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results1a
results1a$plots
## correlated both

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
## correlated both

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
## control correlated

results53a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results53a
results53a$plots
## sqrt fixes

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
## correlated infected

results71a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrt3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results71a
results71a$plots
## correlated infected

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
## sqrt makes infected correlated

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
## correlated both

results75a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results75a
results75a$plots
## correlated both

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
## correlated infected

results82a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results82a
results82a$plots
## correlated infected

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
## correlated infected

results84a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results84a
results84a$plots
## correlated infected

results85 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results85
results85$plots
## correlated infected

results85a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone,sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results85a
results85a$plots
## correlated infected

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
## correlated control

results90a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrt3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results90a
results90a$plots
## correlated control

results91 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results91
results91$plots
## correlated control

results91a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results91a
results91a$plots
## correlated control

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
## correlated infected

results93a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtbeta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results93a
results93a$plots
## correlated infected

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
## correlated control

results96a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results96a
results96a$plots
## fixed correlation

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
## correlated both

results102a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results102a
results102a$plots
## correlated both 

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
## correlated infected

results109a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results109a
results109a$plots
## fixed correlation

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
## correlated control

results116a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results116a
results116a$plots
## correlated control

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
## sqrt correlated control

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
## correlated control

results126a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results126a
results126a$plots
## correlated control

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
## sqrt correlated control

## Apocarotenoid and canary canthophyll both
## sqrtApocarotenoid and sqrtcanary xanthopyll both
## sqrtApocarotenoid and sqrtXanthophyll control
## Apocarotenoid and canary canthophyll ester 1 both
## sqrtApocarotenoid and sqrtcanary xanthopyll ester 1 both
## Apocarotenoid and canary canthophyll ester 2 control
## sqrtApocarotenoid and sqrtcanary xanthopyll ester 2 control
## canary canthophyll and xanthophyll control
## sqrtcanary xanthophyll and sqrtxanthophyll control
## canary xanthophyll and canary xanthophyll ester 1 control
## sqrtCanthaxanthin and sqrtcanary xanthophyll ester 2 control
## Canthaxanthin and ketocarotenoid ester 1 control
## sqrtCanthaxanthin and sqrtketocarotenoid ester 1 control
## Canthaxanthin and ketocarotenoid ester 3 control
## sqrtCanthaxanthin and sqrtketocarotenoid ester 3 control
## Xanthophyll and ketocarotenoid ester 1
## cis.ketocarotenoid and X3.hydroxy.echinenone infected
## sqrtcis.ketocarotenoid and sqrt3.hydroxy.echinenone infected
## sqrtcis.ketocarotenoid and sqrtlutein ester 1 infected
## cis.ketocarotenoid and canary xanthophyll ester 2 both
## sqrtcis.ketocarotenoid and sqrtcanary xanthophyll ester 2 both
## X3.hydroxy.echinenone and lutein ester 1 infected
## sqrt3.hydroxy.echinenone and sqrtlutein ester 1 infected
## X3.hydroxy.echinenone and beta carotene infected
## sqrt3.hydroxy.echinenone and sqrtbeta carotene infected
## X3.hydroxy.echinenone and canary xanthoplyll ester 2 infected
## sqrt3.hydroxy.echinenone and sqrtcanary xanthophyll ester 2 infected
## X3.hydroxy.echinenone and X3HE ester control
## sqrt3.hydroxy.echinenone and sqrt3HE ester control
## X3.hydroxy.echinenone and ketocarotenoid ester 3 control
## sqrt3.hydroxy.echinenone and sqrtketocarotenoid ester 3 control
## lutein ester 1 and betacarotene infected
## sqrtlutein ester 1 and sqrtbetacarotene infected
## lutein ester 1 and ketocarotenoid ester 2 control
## canary xanthophyll ester 1 and canary xanthophyll ester 2 both
## sqrtcanary xanthophyll ester 1 and sqrtcanary xanthophyll ester 2 both
## beta carotene and canary xanthophyll ester 2 infected
## canary xanthophyll ester 2 and ketocarotenoid ester 1 control
## sqrtcanary xanthophyll ester 2 and sqrtketocarotenoid ester 1 control
## sqrtcanary xanthophyll ester 2 and sqrtketocarotenoid ester 3 control
## ketocarotenoid ester 1 and ketocarotenoid ester 3 control
## sqrtketocarotenoid ester 1 and sqrtketocarotenoid ester 3 control
## sqrt3HE ester control and sqrtketocarotenoid ester 3 control

## Assumptions: Linearity  by Sex, no sqrt and sqrt ####
results137 <- Carotenoids %>%
  select(Apocarotenoid, Canary.xanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results137
results137$plots
## correlated both

results137a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtCanary.xanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results137a
results137a$plots
## correlated both

results138 <- Carotenoids %>%
  select(Apocarotenoid, Canthaxanthin, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results138
results138$plots

results138a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtCanthaxanthin, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results138a
results138a$plots

results139 <- Carotenoids %>%
  select(Apocarotenoid, Xanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results139
results139$plots

results139a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtXanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results139a
results139a$plots

results140 <- Carotenoids %>%
  select(Apocarotenoid, Echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results140
results140$plots

results140a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtEchinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results140a
results140a$plots

results141 <- Carotenoids %>%
  select(Apocarotenoid, cis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results141
results141$plots

results141a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results141a
results141a$plots

results142 <- Carotenoids %>%
  select(Apocarotenoid, X3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results142
results142$plots

results142a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrt3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results142a
results142a$plots

results143 <- Carotenoids %>%
  select(Apocarotenoid, lutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results143
results143$plots

results143a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtlutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results143a
results143a$plots

results144 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results144
results144$plots
## correlated male

results144a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results144a
results144a$plots
## correlated male

results145 <- Carotenoids %>%
  select(Apocarotenoid, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results145
results145$plots

results145a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results145a
results145a$plots

results146 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results146
results146$plots
## correlated both

results146a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results146a
results146a$plots
## just barely fixes

results147 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results147
results147$plots
## correlated male

results147a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results147a
results147a$plots
## fixes

results148 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results148
results148$plots

results148a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results148a
results148a$plots

results149 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results149
results149$plots

results149a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results149a
results149a$plots

results150 <- Carotenoids %>%
  select(Apocarotenoid, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results150
results150$plots

results150a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results150a
results150a$plots

results151 <- Carotenoids %>%
  select(Apocarotenoid, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results151
results151$plots

results151a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results151a
results151a$plots

results152 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results152
results152$plots

results152a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results152a
results152a$plots

results153 <- Carotenoids %>%
  select(Canary.xanthophyll, Canthaxanthin, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results153
results153$plots

results153a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtCanthaxanthin, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results153a
results153a$plots

results154 <- Carotenoids %>%
  select(Canary.xanthophyll, Xanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results154
results154$plots

results154a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtXanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results154a
results154a$plots

results155 <- Carotenoids %>%
  select(Canary.xanthophyll, Echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results155
results155$plots

results155a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtEchinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results155a
results155a$plots

results156 <- Carotenoids %>%
  select(Canary.xanthophyll, cis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results156
results156$plots

results156a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results156a
results156a$plots

results157 <- Carotenoids %>%
  select(Canary.xanthophyll, X3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results157
results157$plots

results157a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrt3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results157a
results157a$plots

results158 <- Carotenoids %>%
  select(Canary.xanthophyll, lutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results158
results158$plots

results158a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtlutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results158a
results158a$plots

results159 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results159
results159$plots
## correlated male

results159a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcanary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results159a
results159a$plots
## correlated male

results160 <- Carotenoids %>%
  select(Canary.xanthophyll, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results160
results160$plots

results160a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results160a
results160a$plots

results161 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results161
results161$plots
## correlated male

results161a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results161a
results161a$plots
## fixes

results162 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results162
results162$plots

results162a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results162a
results162a$plots

results163 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results163
results163$plots

results163a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results163a
results163a$plots

results164 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.)+ theme_bw(), result = "plots")
results164
results164$plots

results164a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results164a
results164a$plots

results165 <- Carotenoids %>%
  select(Canary.xanthophyll, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results165
results165$plots

results165a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results165a
results165a$plots

results166 <- Carotenoids %>%
  select(Canary.xanthophyll, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results166
results166$plots

results166a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results166a
results166a$plots

results167 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results167
results167$plots

results167a <- Carotenoids %>%
  select(sqrtCanary.xanthophyll, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results167a
results167a$plots

results168 <- Carotenoids %>%
  select(Canthaxanthin, Xanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results168
results168$plots

results168a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtXanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results168a
results168a$plots

results169 <- Carotenoids %>%
  select(Canthaxanthin, Echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results169
results169$plots

results169a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtEchinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results169a
results169a$plots

results170 <- Carotenoids %>%
  select(Canthaxanthin, cis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results170
results170$plots
## correlated female

results170a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results170a
results170a$plots
## correlated female

results171 <- Carotenoids %>%
  select(Canthaxanthin, X3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results171
results171$plots
## correlated female

results171a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrt3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results171a
results171a$plots
## correlated female

results172 <- Carotenoids %>%
  select(Canthaxanthin, lutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results172
results172$plots

results172a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtlutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results172a
results172a$plots

results173 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results173
results173$plots
## correlated female

results173a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcanary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results173a
results173a$plots
## correlated female but helps

results174 <- Carotenoids %>%
  select(Canthaxanthin, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results174
results174$plots

results174a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results174a
results174a$plots

results175 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results175
results175$plots
## correlated female

results175a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results175a
results175a$plots
## correlated female

results176 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results176
results176$plots
## correlated female

results176a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results176a
results176a$plots
## correlated female

results177 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results177
results177$plots

results177a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results177a
results177a$plots

results178 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results178
results178$plots
## correlated female

results178a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results178a
results178a$plots
## just barely fixes

results179 <- Carotenoids %>%
  select(Canthaxanthin, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results179
results179$plots

results179a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results179a
results179a$plots

results180 <- Carotenoids %>%
  select(Canthaxanthin, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results180
results180$plots

results180a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results180a
results180a$plots

results181 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results181
results181$plots
## correlated female

results181a <- Carotenoids %>%
  select(sqrtCanthaxanthin, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results181a
results181a$plots
## correlated female but helps

results182 <- Carotenoids %>%
  select(Xanthophyll, Echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results182
results182$plots

results182a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtEchinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results182a
results182a$plots

results183 <- Carotenoids %>%
  select(Xanthophyll, cis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results183
results183$plots

results183a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results183a
results183a$plots
## correlates male

results184 <- Carotenoids %>%
  select(Xanthophyll, X3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results184
results184$plots

results184a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrt3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results184a
results184a$plots

results185 <- Carotenoids %>%
  select(Xanthophyll, lutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results185
results185$plots

results185a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtlutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results185a
results185a$plots

results186 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results186
results186$plots

results186a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results186a
results186a$plots

results187 <- Carotenoids %>%
  select(Xanthophyll, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results187
results187$plots

results187a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results187a
results187a$plots

results188 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results188
results188$plots

results188a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results188a
results188a$plots

results189 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results189
results189$plots

results189a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results189a
results189a$plots

results190 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results190
results190$plots

results190a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results190a
results190a$plots

results191 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results191
results191$plots

results191a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results191a
results191a$plots

results192 <- Carotenoids %>%
  select(Xanthophyll, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results192
results192$plots

results192a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results192a
results192a$plots

results193 <- Carotenoids %>%
  select(Xanthophyll, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results193
results193$plots

results193a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results193a
results193a$plots

results194 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results194
results194$plots

results194a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results194a
results194a$plots

results195 <- Carotenoids %>%
  select(Echinenone, cis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results195
results195$plots

results195a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results195a
results195a$plots

results196 <- Carotenoids %>%
  select(Echinenone, X3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results196
results196$plots

results196a <- Carotenoids %>%
  select(sqrtEchinenone, sqrt3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results196a
results196a$plots

results197 <- Carotenoids %>%
  select(Echinenone, lutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results197
results197$plots

results197a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtlutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results197a
results197a$plots

results198 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results198
results198$plots

results198a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results198a
results198a$plots

results199 <- Carotenoids %>%
  select(Echinenone, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results199
results199$plots

results199a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results199a
results199a$plots

results200 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results200
results200$plots

results200a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results200a
results200a$plots

results201 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results201
results201$plots

results201a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results201a
results201a$plots

results202 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results202
results202$plots

results202a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results202a
results202a$plots

results203 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results203
results203$plots

results203a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results203a
results203a$plots

results204 <- Carotenoids %>%
  select(Echinenone, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results204
results204$plots

results204a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results204a
results204a$plots

results205 <- Carotenoids %>%
  select(Echinenone, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results205
results205$plots

results205a <- Carotenoids %>%
  select(sqrtEchinenone, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results205a
results205a$plots

results206 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results206
results206$plots

results206a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results206a
results206a$plots

results207 <- Carotenoids %>%
  select(cis.ketocarotenoid, X3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results207
results207$plots
## correlated both

results207a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrt3.hydroxy.echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results207a
results207a$plots
## correlated both

results208 <- Carotenoids %>%
  select(cis.ketocarotenoid, lutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results208
results208$plots

results208a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtlutein.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results208a
results208a$plots

results209 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results209
results209$plots

results209a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results209a
results209a$plots

results210 <- Carotenoids %>%
  select(cis.ketocarotenoid, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results210
results210$plots
## correlated male

results210a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results210a
results210a$plots
## correlated male

results211 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results211
results211$plots
## correlated both

results211a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results211a
results211a$plots
## correlated both

results212 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results212
results212$plots

results212a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results212a
results212a$plots

results213 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results213
results213$plots

results213a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results213a
results213a$plots

results214 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results214
results214$plots
## correlated female

results214a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results214a
results214a$plots
### correlated female

results215 <- Carotenoids %>%
  select(cis.ketocarotenoid, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results215
results215$plots

results215a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results215a
results215a$plots

results216 <- Carotenoids %>%
  select(cis.ketocarotenoid, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results216
results216$plots

results216a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results216a
results216a$plots

results217 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results217
results217$plots
## correlated female

results217a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results217a
results217a$plots
## correlated female

results218 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results218
results218$plots
## correlated male

results218a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtlutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results218a
results218a$plots
## correlated male

results219 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results219
results219$plots

results219a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtcanary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results219a
results219a$plots

results220 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results220
results220$plots
## correlated male

results220a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results220a
results220a$plots
## correlated male

results221 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results221
results221$plots
## correlated both

results221a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone,sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results221a
results221a$plots
## correlated both

results222 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results222
results222$plots
## correlated female

results222a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results222a
results222a$plots
## correlated female

results223 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results223
results223$plots

results223a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results223a
results223a$plots

results224 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results224
results224$plots
## correlated male

results224a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results224a
results224a$plots
## almost fixes but still correlated male

results225 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results225
results225$plots

results225a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results225a
results225a$plots

results226 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results226
results226$plots
## correlated female

results226a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results226a
results226a$plots
## correlated female

results227 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results227
results227$plots
## correlated female

results227a <- Carotenoids %>%
  select(sqrt3.hydroxy.echinenone, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results227a
results227a$plots
## correlated female

results228 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results228
results228$plots

results228a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtcanary.xanthophyll.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results228a
results228a$plots

results229 <- Carotenoids %>%
  select(lutein.ester.1, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results229
results229$plots
## correlated both

results229a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results229a
results229a$plots
## correlated both

results230 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results230
results230$plots

results230a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results230a
results230a$plots

results231 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results231
results231$plots

results231a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results231a
results231a$plots

results232 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results232
results232$plots

results232a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results232a
results232a$plots

results233 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results233
results233$plots

results233a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results233a
results233a$plots

results234 <- Carotenoids %>%
  select(lutein.ester.1, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results234
results234$plots

results234a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results234a
results234a$plots

results235 <- Carotenoids %>%
  select(lutein.ester.1, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results235
results235$plots
## correlated female

results235a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results235a
results235a$plots
## correlated female

results236 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results236
results236$plots
## correlated female

results236a <- Carotenoids %>%
  select(sqrtlutein.ester.1, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results236a
results236a$plots
## correlated female

results237 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results237
results237$plots

results237a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results237a
results237a$plots

results238 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results238
results238$plots
## correlated both

results238a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results238a
results238a$plots
## correlated both

results239 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results239
results239$plots
## correlated female

results239a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results239a
results239a$plots
## correlated female

results240 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results240
results240$plots

results240a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results240a
results240a$plots

results241 <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results241
results241$plots

results241a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results241a
results241a$plots

results242 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results242
results242$plots

results242a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results242a
results242a$plots

results243 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results243
results243$plots

results243a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results243a
results243a$plots

results244 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results244
results244$plots

results244a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.1, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results244a
results244a$plots

results245 <- Carotenoids %>%
  select(beta.carotene, canary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results245
results245$plots

results245a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanary.xanthophyll.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results245a
results245a$plots

results246 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results246
results246$plots

results246a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results246a
results246a$plots

results247 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results247
results247$plots

results247a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results247a
results247a$plots

results248 <- Carotenoids %>%
  select(beta.carotene, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results248
results248$plots

results248a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results248a
results248a$plots

results249 <- Carotenoids %>%
  select(beta.carotene, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results249
results249$plots

results249a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results249a
results249a$plots

results250 <- Carotenoids %>%
  select(beta.carotene, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results250
results250$plots
## correlated female

results250a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results250a
results250a$plots
## correlated female

results251 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results251
results251$plots

results251a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results251a
results251a$plots

results252 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results252
results252$plots
## correlated female

results252a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtketocarotenoid.ester.1, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results252a
results252a$plots
## correlated female

results253 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results253
results253$plots

results253a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results253a
results253a$plots

results254 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results254
results254$plots
## correlated female

results254a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results254a
results254a$plots
## correlated female

results255 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results255
results255$plots

results255a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results255a
results255a$plots

results256 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results256
results256$plots

results256a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results256a
results256a$plots

results257 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results257
results257$plots
## correlated female

results257a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.2, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results257a
results257a$plots
## correlated female

results258 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, ketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results258
results258$plots

results258a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtketocarotenoid.ester.2, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results258a
results258a$plots

results259 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results259
results259$plots

results259a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results259a
results259a$plots

results260 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results260
results260$plots

results260a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results260a
results260a$plots

results261 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results261
results261$plots

results261a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results261a
results261a$plots

results262 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results262
results262$plots
## correlated female

results262a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.1, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results262a
results262a$plots
## correlated female

results263 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results263
results263$plots

results263a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.2, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results263a
results263a$plots

results264 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results264
results264$plots

results264a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.2, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results264a
results264a$plots

results265 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results265
results265$plots

results265a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.2, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results265a
results265a$plots
## correlated female

results266 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results266
results266$plots

results266a <- Carotenoids %>%
  select(sqrtketocarotenoid.ester.2, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results266a
results266a$plots

results267 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results267
results267$plots

results267a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.3, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results267a
results267a$plots

results268 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results268
results268$plots

results268a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.3, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results268a
results268a$plots

results269 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results269
results269$plots

results269a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.3, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results269a
results269a$plots

results270 <- Carotenoids %>%
  select(canthaxanthin.ester, X3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results270
results270$plots

results270a <- Carotenoids %>%
  select(sqrtcanthaxanthin.ester, sqrt3HE.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results270a
results270a$plots

results271 <- Carotenoids %>%
  select(canthaxanthin.ester, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results271
results271$plots

results271a <- Carotenoids %>%
  select(sqrtcanthaxanthin.ester, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results271a
results271a$plots

results272 <- Carotenoids %>%
  select(X3HE.ester, ketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results272
results272$plots
## correlated female

results272a <- Carotenoids %>%
  select(sqrt3HE.ester, sqrtketocarotenoid.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results272a
results272a$plots
## correlated female

## Apocarotenoid and canary xanthophyll both
## sqrtApocarotenoid and sqrtcanary xanthophyll both
## Apocarotenoid and canary xanthophyll ester 1 male
## sqrtApocarotenoid and sqrtcanary xanthophyll ester 1 male
## Apocarotenoid and canary xanthophyll ester 2 both
## Apocarotenoid and ketocarotenoid ester 1 male
## Canary xanthophyll and canary xanthophyll ester 1 male
## sqrtCanary xanthophyll and sqrtcanary xanthophyll ester 1 male
## Canary xanthophyll and canary xanthophyll ester 2 male
## sqrtCanary xanthophyll and sqrtcanary xanthophyll ester 2 male
## Canthaxanthin and cisketocarotenoid female
## sqrtCanthaxanthin and sqrtcisketocarotenoid female
## Canthaxanthin and X3.hydroxy.echinenone female
## sqrtCanthaxanthin and sqrt3.hydroxy.echinenone female
## Canthaxanthin and canary xanthophyll ester 1 female
## sqrtCanthaxanthin and sqrtcanary xanthophyll ester 1 female
## Canthaxanthin and canary xanthophyll ester 2 female
## sqrtCanthaxanthin and sqrtcanary xanthophyll ester 2 female
## Canthaxanthin and ketocarotenoid ester 1 female
## sqrtCanthaxanthin and sqrtketocarotenoid ester 1 female
## Canthaxanthin and canary xanthophyll ester 3 female
## Canthaxanthin and ketocarotenoid ester 3 female
## sqrtCanthaxanthin and sqrtketocarotenoid ester 3 female
## sqrtXanthophyll and sqrtcisketocarotenoid male
## cisketocarotenoid and X3.hydroxy.echinenone both
## sqrtcisketocarotenoid and sqrtX3.hydroxy.echinenone both
## cisketocarotenoid and beta carotene male
## sqrtcisketocarotenoid and sqrtbeta carotene male
## cisketocarotenoid and canary xanthophyll ester 2 both
## sqrtcisketocarotenoid and sqrtcanary xanthophyll ester 2 both
## cisketocarotenoid and canary xanthophyll ester 3 female
## sqrtcisketocarotenoid and sqrtcanary xanthophyll ester 3 female
## cisketocarotenoid and ketocarotenoid ester 3 female
## sqrtcisketocarotenoid and sqrtketocarotenoid ester 3 female
## X3.hydroxy.echinenone and lutein ester 1 male
## sqrt3.hydroxy.echinenone and sqrtlutein ester 1 male
## X3.hydroxy.echinenone and beta carotene male
## sqrt3.hydroxy.echinenone and sqrtbeta carotene male
## X3.hydroxy.echinenone and canary xanthophyll ester 2 both
## sqrt3.hydroxy.echinenone and sqrtcanary xanthophyll ester 2 both
## X3.hydroxy.echinenone and ketocarotenoid ester 1 female
## sqrt3.hydroxy.echinenone and sqrtketocarotenoid ester 1 female
## X3.hydroxy.echinenone and canary xanthophyll ester 3 male
## sqrt3.hydroxy.echinenone and sqrtcanary xanthophyll ester 3 male
## X3.hydroxy.echinenone and X3H3 ester female
## sqrtX3.hydroxy.echinenone and sqrtX3H3 ester female
## X3.hydroxy.echinenone and ketocarotenoid ester 3 female
## sqrt3.hydroxy.echinenone and sqrtketocarotenoid ester 3 female
## Lutein ester 1 and beta carotene both
## sqrtLutein ester 1 and sqrtbeta carotene both
## Lutein ester 1 and X3H3 ester female
## sqrtLutein ester 1 and sqrtX3H3 ester female
## Lutein ester 1 and ketocarotenoid ester 3 female
## sqrtLutein ester 1 and sqrtketocarotenoid ester 3 female
## Canary xanthophyll ester 1 and canary xanthophyll ester 2 both
## sqrtCanary xanthophyll ester 1 and sqrtcanary xanthophyll ester 2 both
## Canary xanthophyll ester 1 and ketocarotenoid ester 1 female
## sqrtCanary xanthophyll ester 1 and sqrtketocarotenoid ester 1 female
## Beta carotene and X3H3 ester female
## sqrtBeta carotene and sqrt3H3 ester female
## Canary xanthophyll ester 2 and ketocarotenoid ester 1 female
## sqrtCanary xanthophyll ester 2 and sqrtketocarotenoid ester 1 female
## Canary xanthophyll ester 2 and canary xanthophyll ester 3 female
## sqrtCanary xanthophyll ester 2 and sqrtcanary xanthophyll ester 3 female
## Canary xanthophyll ester 2 and ketocarotenoid ester 3 female
## sqrtCanary xanthophyll ester 2 and sqrtketocarotenoid ester 3 female
## Ketocarotenoid ester 1 and ketocarotenoid ester 3 female
## sqrtKetocarotenoid ester 1 and sqrtketocarotenoid ester 3 female
## sqrtKetocarotenoid ester 2 and sqrtX3H3 ester female
## X3H3 ester and ketocarotenoid ester 3 female
## sqrtX3H3 ester and sqrtketocarotenoid ester 3 female


## Multivariate tests of normality for original and sqrt original####

MVNMardiafull <- mvn(data = Carotenoidsoriginal, mvnTest = "mardia", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNMardiafull$multivariateNormality
## definitely nonormal
MVNMardiafullsqrt <- mvn(data = Carotenoidssqrt, mvnTest = "mardia",multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNMardiafullsqrt$multivariateNormality
## definitely nonormal
MVNHzfull <- mvn(data = Carotenoidsoriginal, mvnTest = "hz", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNHzfull$multivariateNormality
## definitely nonormal
MVNHzfullsqrt <- mvn(data = Carotenoidssqrt, mvnTest = "hz", multivariatePlot = "qq")
MVNHzfullsqrt$multivariateNormality
## P is 0.02 so not normal
MVNroystonfull <- mvn(data = Carotenoidsoriginal, mvnTest = "royston", multivariatePlot = "qq")
MVNroystonfull$multivariateNormality
## definitely nonormal
MVNroystonfullsqrt <- mvn(data = Carotenoidssqrt, mvnTest = "royston", multivariatePlot = "qq")
MVNroystonfullsqrt$multivariateNormality
## definitely nonormal
MVNDhfull <- mvn(data = Carotenoidsoriginal, mvnTest = "dh", multivariatePlot = "qq")
MVNDhfull$multivariateNormality
## definitely nonormal
MVNDhfullsqrt <- mvn(data = Carotenoidssqrt, mvnTest = "dh", multivariatePlot = "qq")
MVNDhfullsqrt$multivariateNormality
## definitely nonormal
MVNenergyfull <- mvn(data = Carotenoidsoriginal, mvnTest = "energy", multivariatePlot = "qq")
MVNenergyfull$multivariateNormality
## definitely nonormal
MVNenergyfullsqrt <- mvn(data = Carotenoidssqrt, mvnTest = "energy", multivariatePlot = "qq")
MVNenergyfullsqrt$multivariateNormality
## definitely nonormal

## Overall comparisons of all 17 variables ####

## Multicollinearity for original 

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

## Multicollinearity for sqrt

## sqrtApocarotenoid and sqrtCanary.Xanthophyll
## sqrtApocarotenoid and sqrtcanary.xanthophyll ester 1
## sqrtcis ketocarotenoid and sqrt3 hydroxy echinenone
## sqrtcis ketocarotenoid and sqrtcanary xanthophyll ester 2
## sqrtX3 hydroxy echinenone and sqrtcanary xanthophyll ester 2
## sqrtlutein ester 1 and sqrtbeta carotene
## sqrtcanary xanthophyll ester 1 and sqrtcanary xanthophyll ester 2
## sqrtcanthaxnthin ester has better correlations, still going to just use canthaxanthin

## Linearity graph comparisons above 0.9 for Group Control/Infected

## Apocarotenoid and canary canthophyll both
## sqrtApocarotenoid and sqrtcanary xanthopyll both
## sqrtApocarotenoid and sqrtXanthophyll control
## Apocarotenoid and canary canthophyll ester 1 both
## sqrtApocarotenoid and sqrtcanary xanthopyll ester 1 both
## Apocarotenoid and canary canthophyll ester 2 control
## sqrtApocarotenoid and sqrtcanary xanthopyll ester 2 control
## canary canthophyll and xanthophyll control
## sqrtcanary xanthophyll and sqrtxanthophyll control
## canary xanthophyll and canary xanthophyll ester 1 control
## sqrtCanthaxanthin and sqrtcanary xanthophyll ester 2 control
## Canthaxanthin and ketocarotenoid ester 1 control
## sqrtCanthaxanthin and sqrtketocarotenoid ester 1 control
## Canthaxanthin and ketocarotenoid ester 3 control
## sqrtCanthaxanthin and sqrtketocarotenoid ester 3 control
## Xanthophyll and ketocarotenoid ester 1
## cis.ketocarotenoid and X3.hydroxy.echinenone infected
## sqrtcis.ketocarotenoid and sqrt3.hydroxy.echinenone infected
## sqrtcis.ketocarotenoid and sqrtlutein ester 1 infected
## cis.ketocarotenoid and canary xanthophyll ester 2 both
## sqrtcis.ketocarotenoid and sqrtcanary xanthophyll ester 2 both
## X3.hydroxy.echinenone and lutein ester 1 infected
## sqrt3.hydroxy.echinenone and sqrtlutein ester 1 infected
## X3.hydroxy.echinenone and beta carotene infected
## sqrt3.hydroxy.echinenone and sqrtbeta carotene infected
## X3.hydroxy.echinenone and canary xanthoplyll ester 2 infected
## sqrt3.hydroxy.echinenone and sqrtcanary xanthophyll ester 2 infected
## X3.hydroxy.echinenone and X3HE ester control
## sqrt3.hydroxy.echinenone and sqrt3HE ester control
## X3.hydroxy.echinenone and ketocarotenoid ester 3 control
## sqrt3.hydroxy.echinenone and sqrtketocarotenoid ester 3 control
## lutein ester 1 and betacarotene infected
## sqrtlutein ester 1 and sqrtbetacarotene infected
## lutein ester 1 and ketocarotenoid ester 2 control
## canary xanthophyll ester 1 and canary xanthophyll ester 2 both
## sqrtcanary xanthophyll ester 1 and sqrtcanary xanthophyll ester 2 both
## beta carotene and canary xanthophyll ester 2 infected
## canary xanthophyll ester 2 and ketocarotenoid ester 1 control
## sqrtcanary xanthophyll ester 2 and sqrtketocarotenoid ester 1 control
## sqrtcanary xanthophyll ester 2 and sqrtketocarotenoid ester 3 control
## ketocarotenoid ester 1 and ketocarotenoid ester 3 control
## sqrtketocarotenoid ester 1 and sqrtketocarotenoid ester 3 control
## sqrt3HE ester control and sqrtketocarotenoid ester 3 control

## Linearity graph comparisons above 0.9 for Group Male/Female

## Apocarotenoid and canary xanthophyll both
## sqrtApocarotenoid and sqrtcanary xanthophyll both
## Apocarotenoid and canary xanthophyll ester 1 male
## sqrtApocarotenoid and sqrtcanary xanthophyll ester 1 male
## Apocarotenoid and canary xanthophyll ester 2 both
## Apocarotenoid and ketocarotenoid ester 1 male
## Canary xanthophyll and canary xanthophyll ester 1 male
## sqrtCanary xanthophyll and sqrtcanary xanthophyll ester 1 male
## Canary xanthophyll and canary xanthophyll ester 2 male
## sqrtCanary xanthophyll and sqrtcanary xanthophyll ester 2 male
## Canthaxanthin and cisketocarotenoid female
## sqrtCanthaxanthin and sqrtcisketocarotenoid female
## Canthaxanthin and X3.hydroxy.echinenone female
## sqrtCanthaxanthin and sqrt3.hydroxy.echinenone female
## Canthaxanthin and canary xanthophyll ester 1 female
## sqrtCanthaxanthin and sqrtcanary xanthophyll ester 1 female
## Canthaxanthin and canary xanthophyll ester 2 female
## sqrtCanthaxanthin and sqrtcanary xanthophyll ester 2 female
## Canthaxanthin and ketocarotenoid ester 1 female
## sqrtCanthaxanthin and sqrtketocarotenoid ester 1 female
## Canthaxanthin and canary xanthophyll ester 3 female
## Canthaxanthin and ketocarotenoid ester 3 female
## sqrtCanthaxanthin and sqrtketocarotenoid ester 3 female
## sqrtXanthophyll and sqrtcisketocarotenoid male
## cisketocarotenoid and X3.hydroxy.echinenone both
## sqrtcisketocarotenoid and sqrtX3.hydroxy.echinenone both
## cisketocarotenoid and beta carotene male
## sqrtcisketocarotenoid and sqrtbeta carotene male
## cisketocarotenoid and canary xanthophyll ester 2 both
## sqrtcisketocarotenoid and sqrtcanary xanthophyll ester 2 both
## cisketocarotenoid and canary xanthophyll ester 3 female
## sqrtcisketocarotenoid and sqrtcanary xanthophyll ester 3 female
## cisketocarotenoid and ketocarotenoid ester 3 female
## sqrtcisketocarotenoid and sqrtketocarotenoid ester 3 female
## X3.hydroxy.echinenone and lutein ester 1 male
## sqrt3.hydroxy.echinenone and sqrtlutein ester 1 male
## X3.hydroxy.echinenone and beta carotene male
## sqrt3.hydroxy.echinenone and sqrtbeta carotene male
## X3.hydroxy.echinenone and canary xanthophyll ester 2 both
## sqrt3.hydroxy.echinenone and sqrtcanary xanthophyll ester 2 both
## X3.hydroxy.echinenone and ketocarotenoid ester 1 female
## sqrt3.hydroxy.echinenone and sqrtketocarotenoid ester 1 female
## X3.hydroxy.echinenone and canary xanthophyll ester 3 male
## sqrt3.hydroxy.echinenone and sqrtcanary xanthophyll ester 3 male
## X3.hydroxy.echinenone and X3H3 ester female
## sqrtX3.hydroxy.echinenone and sqrtX3H3 ester female
## X3.hydroxy.echinenone and ketocarotenoid ester 3 female
## sqrt3.hydroxy.echinenone and sqrtketocarotenoid ester 3 female
## Lutein ester 1 and beta carotene both
## sqrtLutein ester 1 and sqrtbeta carotene both
## Lutein ester 1 and X3H3 ester female
## sqrtLutein ester 1 and sqrtX3H3 ester female
## Lutein ester 1 and ketocarotenoid ester 3 female
## sqrtLutein ester 1 and sqrtketocarotenoid ester 3 female
## Canary xanthophyll ester 1 and canary xanthophyll ester 2 both
## sqrtCanary xanthophyll ester 1 and sqrtcanary xanthophyll ester 2 both
## Canary xanthophyll ester 1 and ketocarotenoid ester 1 female
## sqrtCanary xanthophyll ester 1 and sqrtketocarotenoid ester 1 female
## Beta carotene and X3H3 ester female
## sqrtBeta carotene and sqrt3H3 ester female
## Canary xanthophyll ester 2 and ketocarotenoid ester 1 female
## sqrtCanary xanthophyll ester 2 and sqrtketocarotenoid ester 1 female
## Canary xanthophyll ester 2 and canary xanthophyll ester 3 female
## sqrtCanary xanthophyll ester 2 and sqrtcanary xanthophyll ester 3 female
## Canary xanthophyll ester 2 and ketocarotenoid ester 3 female
## sqrtCanary xanthophyll ester 2 and sqrtketocarotenoid ester 3 female
## Ketocarotenoid ester 1 and ketocarotenoid ester 3 female
## sqrtKetocarotenoid ester 1 and sqrtketocarotenoid ester 3 female
## sqrtKetocarotenoid ester 2 and sqrtX3H3 ester female
## X3H3 ester and ketocarotenoid ester 3 female
## sqrtX3H3 ester and sqrtketocarotenoid ester 3 female

## Carotenoids to keep: Apocarotenoid, Canary Xanthophyll Ester 3, Echineone, Cisketocarotenoid, 
## Xanthophyll, Beta carotene, Canthaxanthin Ester

## Carotenoids not using: Canary Xanthophyll, Canary Xanthophyll Ester 1, Canary Xanthophyll Ester 2,
## X3 Hydroxy.Echinenone, X3H3 ester, Ketocarotenoid ester 1, Ketocarotenoid ester 2, Ketocarotenoid ester 3, 
## Lutein Ester 1, Canthaxanthin

## Redo Assumptions with outliers taken out ####
## Outliers removed in order: F1, M3, M29, F19, M17
Carotenoidsnew <- Carotenoids[,c(3,9,11,13,21,29,31)]
Carotenoidsnewsqrt <- Carotenoids[,c(4,10,12,14,22,30,32)]

## Independent assumptions with little correlation
ICC(Carotenoidsnew) ## -0.084, fine
ICC(Carotenoidsnewsqrt) ## -0.054, fine

## Multivariate Normality
dependentcarotenoidsnew <- data.frame(Carotenoids$Apocarotenoid, Carotenoids$Xanthophyll, Carotenoids$Echinenone, Carotenoids$cis.ketocarotenoid,
                                   Carotenoids$beta.carotene, Carotenoids$canary.xanthophyll.ester.3, Carotenoids$canthaxanthin.ester)
dependentcarotenoidsnewsqrt <- data.frame(Carotenoids$sqrtApocarotenoid, Carotenoids$sqrtXanthophyll, Carotenoids$sqrtEchinenone, Carotenoids$sqrtcis.ketocarotenoid,
                                          Carotenoids$sqrtbeta.carotene, Carotenoids$sqrtcanary.xanthophyll.ester.3, Carotenoids$sqrtcanthaxanthin.ester)

transpose_dependentcarotenoidsnew <- t(dependentcarotenoidsnew)
transpose_dependentcarotenoidsnewsqrt <- t(dependentcarotenoidsnewsqrt)
mshapiro.test(transpose_dependentcarotenoidsnew) ## not normal
mshapiro.test(transpose_dependentcarotenoidsnewsqrt) ## not normal

MVNMardianew <- mvn(data = Carotenoidsnew, mvnTest = "mardia", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNMardianew$multivariateNormality
MVNMardianewsqrt <- mvn(data = Carotenoidsnewsqrt, mvnTest = "mardia", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNMardianewsqrt$multivariateNormality
## sqrt now normal
MVNHznew <- mvn(data = Carotenoidsnew, mvnTest = "hz", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNHznew$multivariateNormality
MVNHznewsqrt <- mvn(data = Carotenoidsnewsqrt, mvnTest = "hz", multivariatePlot = "qq")
MVNHznewsqrt$multivariateNormality
## sqrt now normal
MVNroystonnew <- mvn(data = Carotenoidsnew, mvnTest = "royston", multivariatePlot = "qq")
MVNroystonnew$multivariateNormality
MVNroystonnewsqrt <- mvn(data = Carotenoidsnewsqrt, mvnTest = "royston", multivariatePlot = "qq")
MVNroystonnewsqrt$multivariateNormality
## sqrt now normal
MVNDhnew <- mvn(data = Carotenoidsnew, mvnTest = "dh", multivariatePlot = "qq")
MVNDhnew$multivariateNormality
MVNDhnewsqrt <- mvn(data = Carotenoidsnewsqrt, mvnTest = "dh", multivariatePlot = "qq")
MVNDhnewsqrt$multivariateNormality
##nonnormal
MVNenergynew <- mvn(data = Carotenoidsnew, mvnTest = "energy", multivariatePlot = "qq")
MVNenergynew$multivariateNormality
MVNenergynewsqrt <- mvn(data = Carotenoidsnewsqrt, mvnTest = "energy", multivariatePlot = "qq")
MVNenergynewsqrt$multivariateNormality
## sqrt now normal

## Univariate Normality
ggqqplot(Carotenoids, "Apocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtApocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "Xanthophyll", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtXanthophyll", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "Echinenone", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtEchinenone", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "cis.ketocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtcis.ketocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## helps
ggqqplot(Carotenoids, "beta.carotene", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "sqrtbeta.carotene", facet.by = "Frog.Type",
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

## Assumptions: Variance/Covariance Positive Determinant: needs to be positive
Covddependentcarotenoidsnew <- cov(dependentcarotenoidsnew)
det(Covddependentcarotenoidsnew)
## very positive, looks good
Covdependentcarotenoidsnewsqrt <- cov(dependentcarotenoidsnewsqrt)
det(Covdependentcarotenoidsnewsqrt)
## positive determinant thought not very positive like the original data

## Homogeneity of Variance
groupnew <- c("Infected","Infected","Infected", "Control",
           "Control", "Infected","Infected","Infected","Infected","Infected",
           "Control", "Infected","Infected","Infected","Infected","Infected",
           "Infected","Infected","Infected","Infected","Infected",
           "Control","Infected","Infected","Infected",
           "Control", "Control", "Control", "Infected", "Control", "Infected")
factor(groupnew)
groupnew
boxM(dependentcarotenoidsnew, groupnew)
boxM(dependentcarotenoidsnewsqrt, groupnew) ## sqrt is now higher than 0.05, so is regular but sqrt likely better to use

## Multicollinearity
cor.matnew <- Carotenoidsnew %>% cor_mat()
cor.matnew ## looks good

cor.matnewsqrt <- Carotenoidsnewsqrt %>% cor_mat()
cor.matnewsqrt 

cor(Carotenoidsoriginal)

## Linearity plots for FrogType, 7 variables and with outliers removed ####
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
## now control slightly correlated

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

results112 <- Carotenoids %>%
  select(beta.carotene, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results112
results112$plots

results112a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results112a
results112a$plots

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

## Linearity plots for Sex, 7 variables and with outliers removed ####
results139 <- Carotenoids %>%
  select(Apocarotenoid, Xanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results139
results139$plots

results139a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtXanthophyll, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results139a
results139a$plots

results140 <- Carotenoids %>%
  select(Apocarotenoid, Echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results140
results140$plots

results140a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtEchinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results140a
results140a$plots

results141 <- Carotenoids %>%
  select(Apocarotenoid, cis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results141
results141$plots

results141a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results141a
results141a$plots

results145 <- Carotenoids %>%
  select(Apocarotenoid, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results145
results145$plots

results145a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results145a
results145a$plots

results149 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results149
results149$plots

results149a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results149a
results149a$plots

results150 <- Carotenoids %>%
  select(Apocarotenoid, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results150
results150$plots

results150a <- Carotenoids %>%
  select(sqrtApocarotenoid, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results150a
results150a$plots

results182 <- Carotenoids %>%
  select(Xanthophyll, Echinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results182
results182$plots

results182a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtEchinenone, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results182a
results182a$plots

results183 <- Carotenoids %>%
  select(Xanthophyll, cis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results183
results183$plots

results183a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results183a
results183a$plots
## correlates male

results187 <- Carotenoids %>%
  select(Xanthophyll, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results187
results187$plots

results187a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results187a
results187a$plots

results191 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results191
results191$plots

results191a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results191a
results191a$plots

results192 <- Carotenoids %>%
  select(Xanthophyll, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results192
results192$plots

results192a <- Carotenoids %>%
  select(sqrtXanthophyll, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results192a
results192a$plots

results195 <- Carotenoids %>%
  select(Echinenone, cis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results195
results195$plots
## correlated female

results195a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcis.ketocarotenoid, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results195a
results195a$plots
## correlated female

results199 <- Carotenoids %>%
  select(Echinenone, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results199
results199$plots

results199a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results199a
results199a$plots

results203 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results203
results203$plots

results203a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results203a
results203a$plots

results204 <- Carotenoids %>%
  select(Echinenone, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results204
results204$plots

results204a <- Carotenoids %>%
  select(sqrtEchinenone, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results204a
results204a$plots

results210 <- Carotenoids %>%
  select(cis.ketocarotenoid, beta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results210
results210$plots

results210a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtbeta.carotene, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results210a
results210a$plots

results214 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results214
results214$plots

results214a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results214a
results214a$plots

results215 <- Carotenoids %>%
  select(cis.ketocarotenoid, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results215
results215$plots

results215a <- Carotenoids %>%
  select(sqrtcis.ketocarotenoid, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results215a
results215a$plots

results248 <- Carotenoids %>%
  select(beta.carotene, canary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results248
results248$plots

results248a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanary.xanthophyll.ester.3, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results248a
results248a$plots

results249 <- Carotenoids %>%
  select(beta.carotene, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results249
results249$plots

results249a <- Carotenoids %>%
  select(sqrtbeta.carotene, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results249a
results249a$plots

results267 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, canthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results267
results267$plots

results267a <- Carotenoids %>%
  select(sqrtcanary.xanthophyll.ester.3, sqrtcanthaxanthin.ester, Sex) %>%
  group_by(Sex) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results267a
results267a$plots

## MANOVA ####
## Make group (Control and Infected) categorical variables
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY
groupnew = factor(groupnew)
## Bind dependent variables together into one vector
sqrtApocarotenoid <- Carotenoids$sqrtApocarotenoid
sqrtXanthophyll <- Carotenoids$Xanthophyll
sqrtEchinenone <- Carotenoids$Echinenone
sqrtcis.ketocarotenoid <- Carotenoids$cis.ketocarotenoid
sqrtbeta.carotene <- Carotenoids$sqrtbeta.carotene
sqrtcanary.xanthophyll.ester.3 <- Carotenoids$sqrtcanary.xanthophyll.ester.3
sqrtcanthaxanthin.ester <- Carotenoids$sqrtcanthaxanthin.ester
Y <- cbind(sqrtApocarotenoid,sqrtXanthophyll,sqrtEchinenone,sqrtbeta.carotene,sqrtcis.ketocarotenoid,
           sqrtcanary.xanthophyll.ester.3,sqrtcanthaxanthin.ester)
## Run MANOVA
MANOVA = manova(Y~groupnew)
## Run Tests on MANOVA, check if p values are below 0.05
summary(MANOVA, test = "Wilks")
summary(MANOVA, test = "Pillai")
summary(MANOVA, test = "Hotelling-Lawley")
summary(MANOVA, test = "Roy")

## nothing significant here between groups 

linearHypothesis(MANOVA, "grade.L") |> print(SSP = FALSE)

## Wilks Lambda partial eta squares values
## Take smaller of two values, number of outcome variables (here, 16) or df of group variable = b
## formula: 1-(0.63202)^(1/b)
## Answer will be percentage of variance between outcome variables is due to difference in group variable

## Post hoc tests
## Effect size
eta_squared(MANOVA) ## value is 0.12, not high enough to mean anything really which explains the non-significant p-value

MANOVA_lda <- lda(groupnew ~ Y, CV = F)
MANOVA_lda

MANOVA_df <- data.frame(
  species = Carotenoids[, "Frog.Type"],
  lda = predict(MANOVA_lda)$x
)
MANOVA_df


ggplot(MANOVA_df) +
  geom_point(aes(x = LD1, y = species), size = 4) +
  theme_classic()

## Factorial MANOVA ####
group <- factor(Frog.Type)
group
sex <- factor(Sex)
sex

FACTORIALSETUP <- lm(Y~Frog.Type*Sex -1, data = Carotenoids)
summary(FACTORIALSETUP)
Anova(FACTORIALSETUP, test.statistic="Roy")
Anova(update(FACTORIALSETUP, sqrtApocarotenoid ~ .))
Anova(update(FACTORIALSETUP, sqrtXanthophyll ~ .))
Anova(update(FACTORIALSETUP, sqrtEchinenone ~ .))
Anova(update(FACTORIALSETUP, sqrtcis.ketocarotenoid ~ .))
Anova(update(FACTORIALSETUP, sqrtcanary.xanthophyll.ester.3 ~ .))
Anova(update(FACTORIALSETUP, sqrtbeta.carotene ~ .))
Anova(update(FACTORIALSETUP, sqrtcanthaxanthin.ester ~ .))
## interaction not significant so using type II
FACTORIALMANOVAwilks <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Wilks"))
FACTORIALMANOVApillai <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Pillai"))
FACTORIALMANOVAhotelling <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Hotelling-Lawley"))
FACTORIALMANOVAroy <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Roy"))
## If interactions aren't significant, can use Type II
FACTORIALMANOVAwilks <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Wilks"))
FACTORIALMANOVAwilks
FACTORIALMANOVApillai <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Pillai"))
FACTORIALMANOVApillai
FACTORIALMANOVAhotelling <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Hotelling-Lawley"))
FACTORIALMANOVAhotelling
FACTORIALMANOVAroy <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Roy"))
FACTORIALMANOVAroy
## Frog type significant for all of these 

MANOVA_ldafactorial <- lda(groupnew ~ Y, CV = F)
MANOVA_ldafactorial


## Wilks Lambda partial eta squares values
## For factorial, separated by group
## Take smaller of two values, number of outcome variables (here, 16) or df of group variable = b
## formula: 1-(0.63202)^(1/b)
## Answer will be percentage of variance between outcome variables is due to difference in group variable
## MANCOVA testing handling time### 
## no sex because wasn't significant in two-way MANOVA, compared frog type and handling time
model <- manova (Y~Handling.Time.Infected.Mock*Frog.Type, data = Carotenoids)
summary(model, test = "Wilks", type = "III")
summary(model, test = "Pillai", type = "III")
summary(model, test = "Hotelling-Lawley", type = "III")
summary(model, test = "Roy", type = "III")
## handling time not significant so taking out and not thinking about again

## Nonparametric MANOVA-like Test####
nonpartest(Apocarotenoid | Canary.xanthophyll | Canthaxanthin | Xanthophyll | Echinenone | cis.ketocarotenoid | X3.hydroxy.echinenone
              | lutein.ester.1 | canary.xanthophyll.ester.1 | beta.carotene | canary.xanthophyll.ester.2
              | ketocarotenoid.ester.1 | ketocarotenoid.ester.2 | canary.xanthophyll.ester.3 | canthaxanthin.ester
              | X3HE.ester | ketocarotenoid.ester.3 ~ Frog.Type, data = Carotenoids, permreps = 1000)

nonpartest(Apocarotenoid | Canary.xanthophyll | ketocarotenoid.ester.1 | ketocarotenoid.ester.2 ~ Frog.Type, data = Carotenoids, permreps = 1000, plots = TRUE)

## Visualization in box plots ####
## coefficients for each of the polynomial terms in Frog Type
lm(cbind(sqrtApocarotenoid, sqrtXanthophyll, sqrtEchinenone, sqrtcis.ketocarotenoid,
         sqrtbeta.carotene, sqrtcanary.xanthophyll.ester.3, sqrtcanthaxanthin.ester
         )~ Frog.Type, data=Carotenoids)
## Means, standard deviations, and standard errors of the means
means <- Carotenoids |>
  group_by(Frog.Type) |>
  summarise(
    n = n(),
    Ap_sd = sd(sqrtApocarotenoid, na.rm = TRUE),
    Xa_sd = sd(sqrtXanthophyll, na.rm = TRUE),
    Ec_sd = sd(sqrtEchinenone, na.rm = TRUE),
    Ci_sd = sd(sqrtcis.ketocarotenoid, na.rm = TRUE),
    Be_sd = sd(sqrtbeta.carotene, na.rm = TRUE),
    Ca_sd = sd(sqrtcanary.xanthophyll.ester.3, na.rm = TRUE),
    Canth_sd = sd(sqrtcanthaxanthin.ester, na.rm = TRUE),
    Ap_se = Ap_sd / sqrt(n),
    Xa_se = Xa_sd / sqrt(n),
    Ec_se = Ec_sd / sqrt(n),
    Ci_se = Ci_sd / sqrt(n),
    Be_se = Be_sd / sqrt(n),
    Ca_se = Ca_sd / sqrt(n),
    Canth_se = Canth_sd / sqrt(n),
    sqrtApocarotenoid = mean(sqrtApocarotenoid),
    sqrtXanthophyll = mean(sqrtXanthophyll),
    sqrtEchinenone = mean(sqrtEchinenone),
    sqrtcis.ketocarotenoid = mean(sqrtcis.ketocarotenoid),
    sqrtbeta.carotene = mean(sqrtbeta.carotene),
    sqrtcanary.xanthophyll.ester.3 = mean(sqrtcanary.xanthophyll.ester.3),
    sqrtcanthaxanthin.ester = mean(sqrtcanthaxanthin.ester) ) |> 
  relocate(sqrtApocarotenoid, sqrtXanthophyll, sqrtEchinenone, sqrtcis.ketocarotenoid,
           sqrtbeta.carotene, sqrtcanary.xanthophyll.ester.3, sqrtcanthaxanthin.ester,
           .after = Frog.Type) |>
  print()

## Plot for each dependent variable
Apmean <-ggplot(data = means, aes(x = Frog.Type, y = sqrtApocarotenoid)) +
  geom_point(size = 4) +
  geom_line(aes(group = 1), linewidth = 1.2) +
  geom_errorbar(aes(ymin = sqrtApocarotenoid - Ap_se, 
                    ymax = sqrtApocarotenoid + Ap_se),
                width = .2) +
  theme_bw(base_size = 15)
Apmean

Xamean <-ggplot(data = means, aes(x = Frog.Type, y = sqrtXanthophyll)) +
  geom_point(size = 4) +
  geom_line(aes(group = 1), linewidth = 1.2) +
  geom_errorbar(aes(ymin = sqrtXanthophyll - Xa_se, 
                    ymax = sqrtXanthophyll + Xa_se),
                width = .2) +
  theme_bw(base_size = 15)
Xamean

Ecmean <-ggplot(data = means, aes(x = Frog.Type, y = sqrtEchinenone)) +
  geom_point(size = 4) +
  geom_line(aes(group = 1), linewidth = 1.2) +
  geom_errorbar(aes(ymin = sqrtEchinenone - Ec_se, 
                    ymax = sqrtEchinenone + Ec_se),
                width = .2) +
  theme_bw(base_size = 15)
Ecmean

Cimean <-ggplot(data = means, aes(x = Frog.Type, y = sqrtcis.ketocarotenoid)) +
  geom_point(size = 4) +
  geom_line(aes(group = 1), linewidth = 1.2) +
  geom_errorbar(aes(ymin = sqrtcis.ketocarotenoid - Ci_se, 
                    ymax = sqrtcis.ketocarotenoid + Ci_se),
                width = .2) +
  theme_bw(base_size = 15)
Cimean

Bemean <-ggplot(data = means, aes(x = Frog.Type, y = sqrtbeta.carotene)) +
  geom_point(size = 4) +
  geom_line(aes(group = 1), linewidth = 1.2) +
  geom_errorbar(aes(ymin = sqrtbeta.carotene - Be_se, 
                    ymax = sqrtbeta.carotene + Be_se),
                width = .2) +
  theme_bw(base_size = 15)
Bemean

Camean <-ggplot(data = means, aes(x = Frog.Type, y = sqrtcanary.xanthophyll.ester.3)) +
  geom_point(size = 4) +
  geom_line(aes(group = 1), linewidth = 1.2) +
  geom_errorbar(aes(ymin = sqrtcanary.xanthophyll.ester.3 - Ca_se, 
                    ymax = sqrtcanary.xanthophyll.ester.3 + Ca_se),
                width = .2) +
  theme_bw(base_size = 15)
Camean

Canthmean <-ggplot(data = means, aes(x = Frog.Type, y = sqrtcanthaxanthin.ester)) +
  geom_point(size = 4) +
  geom_line(aes(group = 1), linewidth = 1.2) +
  geom_errorbar(aes(ymin = sqrtcanthaxanthin.ester - Canth_se, 
                    ymax = sqrtcanthaxanthin.ester + Canth_se),
                width = .2) +
  theme_bw(base_size = 15)
Canthmean

## Treating all as bivariate outcomes
ggplot(data = means, aes(x = sqrtApocarotenoid, y = sqrtXanthophyll, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtApocarotenoid - Ap_se, 
                     xmax = sqrtApocarotenoid + Ap_se)) +
  geom_errorbar(aes(ymin = sqrtXanthophyll - Xa_se, 
                    ymax = sqrtXanthophyll + Xa_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtApocarotenoid, y = sqrtEchinenone, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtApocarotenoid - Ap_se, 
                     xmax = sqrtApocarotenoid + Ap_se)) +
  geom_errorbar(aes(ymin = sqrtEchinenone - Ec_se, 
                    ymax = sqrtEchinenone + Ec_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtApocarotenoid, y = sqrtcis.ketocarotenoid, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtApocarotenoid - Ap_se, 
                     xmax = sqrtApocarotenoid + Ap_se)) +
  geom_errorbar(aes(ymin = sqrtcis.ketocarotenoid - Ci_se, 
                    ymax = sqrtcis.ketocarotenoid + Ci_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtApocarotenoid, y = sqrtbeta.carotene, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtApocarotenoid - Ap_se, 
                     xmax = sqrtApocarotenoid + Ap_se)) +
  geom_errorbar(aes(ymin = sqrtbeta.carotene - Be_se, 
                    ymax = sqrtbeta.carotene + Be_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtApocarotenoid, y = sqrtcanary.xanthophyll.ester.3, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtApocarotenoid - Ap_se, 
                     xmax = sqrtApocarotenoid + Ap_se)) +
  geom_errorbar(aes(ymin = sqrtcanary.xanthophyll.ester.3 - Ca_se, 
                    ymax = sqrtcanary.xanthophyll.ester.3 + Ca_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtApocarotenoid, y = sqrtcanthaxanthin.ester, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtApocarotenoid - Ap_se, 
                     xmax = sqrtApocarotenoid + Ap_se)) +
  geom_errorbar(aes(ymin = sqrtcanthaxanthin.ester - Canth_se, 
                    ymax = sqrtcanthaxanthin.ester + Canth_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtXanthophyll, y = sqrtEchinenone, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtXanthophyll - Xa_se, 
                     xmax = sqrtXanthophyll + Xa_se)) +
  geom_errorbar(aes(ymin = sqrtEchinenone - Ec_se, 
                    ymax = sqrtEchinenone + Ec_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtXanthophyll, y = sqrtcis.ketocarotenoid, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtXanthophyll - Xa_se, 
                     xmax = sqrtXanthophyll + Xa_se)) +
  geom_errorbar(aes(ymin = sqrtcis.ketocarotenoid - Ci_se, 
                    ymax = sqrtcis.ketocarotenoid + Ci_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtXanthophyll, y = sqrtbeta.carotene, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtXanthophyll - Xa_se, 
                     xmax = sqrtXanthophyll + Xa_se)) +
  geom_errorbar(aes(ymin = sqrtbeta.carotene - Be_se, 
                    ymax = sqrtbeta.carotene + Be_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtXanthophyll, y = sqrtcanary.xanthophyll.ester.3, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtXanthophyll - Xa_se, 
                     xmax = sqrtXanthophyll + Xa_se)) +
  geom_errorbar(aes(ymin = sqrtcanary.xanthophyll.ester.3 - Ca_se, 
                    ymax = sqrtcanary.xanthophyll.ester.3 + Ca_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtXanthophyll, y = sqrtcanthaxanthin.ester, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtXanthophyll - Xa_se, 
                     xmax = sqrtXanthophyll + Xa_se)) +
  geom_errorbar(aes(ymin = sqrtcanthaxanthin.ester - Canth_se, 
                    ymax = sqrtcanthaxanthin.ester + Canth_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtEchinenone, y = sqrtcis.ketocarotenoid, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtEchinenone - Ec_se, 
                     xmax = sqrtEchinenone + Ec_se)) +
  geom_errorbar(aes(ymin = sqrtcis.ketocarotenoid - Ci_se, 
                    ymax = sqrtcis.ketocarotenoid + Ci_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtEchinenone, y = sqrtbeta.carotene, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtEchinenone - Ec_se, 
                     xmax = sqrtEchinenone + Ec_se)) +
  geom_errorbar(aes(ymin = sqrtbeta.carotene - Be_se, 
                    ymax = sqrtbeta.carotene + Be_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtEchinenone, y = sqrtcanary.xanthophyll.ester.3, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtEchinenone - Ec_se, 
                     xmax = sqrtEchinenone + Ec_se)) +
  geom_errorbar(aes(ymin = sqrtcanary.xanthophyll.ester.3 - Ca_se, 
                    ymax = sqrtcanary.xanthophyll.ester.3 + Ca_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtEchinenone, y = sqrtcanthaxanthin.ester, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtEchinenone - Ec_se, 
                     xmax = sqrtEchinenone + Ec_se)) +
  geom_errorbar(aes(ymin = sqrtcanthaxanthin.ester - Canth_se, 
                    ymax = sqrtcanthaxanthin.ester + Canth_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtcis.ketocarotenoid, y = sqrtbeta.carotene, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtcis.ketocarotenoid - Ci_se, 
                     xmax = sqrtcis.ketocarotenoid + Ci_se)) +
  geom_errorbar(aes(ymin = sqrtbeta.carotene - Be_se, 
                    ymax = sqrtbeta.carotene + Be_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtcis.ketocarotenoid, y = sqrtcanary.xanthophyll.ester.3, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtcis.ketocarotenoid - Ci_se, 
                     xmax = sqrtcis.ketocarotenoid + Ci_se)) +
  geom_errorbar(aes(ymin = sqrtcanary.xanthophyll.ester.3 - Ca_se, 
                    ymax = sqrtcanary.xanthophyll.ester.3 + Ca_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtcis.ketocarotenoid, y = sqrtcanthaxanthin.ester, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtcis.ketocarotenoid - Ci_se, 
                     xmax = sqrtcis.ketocarotenoid + Ci_se)) +
  geom_errorbar(aes(ymin = sqrtcanthaxanthin.ester - Canth_se, 
                    ymax = sqrtcanthaxanthin.ester + Canth_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

ggplot(data = means, aes(x = sqrtcanary.xanthophyll.ester.3, y = sqrtcanthaxanthin.ester, 
                         color = Frog.Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = sqrtcanary.xanthophyll.ester.3 - Ca_se, 
                     xmax = sqrtcanary.xanthophyll.ester.3 + Ca_se)) +
  geom_errorbar(aes(ymin = sqrtcanthaxanthin.ester - Canth_se, 
                    ymax = sqrtcanthaxanthin.ester + Canth_se)) +
  geom_line(aes(group = 1), linewidth = 1.5) +
  geom_label(aes(label = Frog.Type), 
             nudge_x = -0.015, nudge_y = 0.02) +
  scale_color_discrete(guide = "none") +
  theme_bw(base_size = 15)

## HE plots
heplot(MANOVA)
pairs(MANOVA)
heplot(FACTORIALSETUP)

MANOVA.effect <- allEffects(MANOVA)
plot(MANOVA.effect, ask=FALSE)

## Normality tests Control ####
Carotenoidscontrol <- subset(Carotenoids, Frog.Type == "Control")

ggdensity(Carotenoidscontrol$Apocarotenoid)
ggqqplot(Carotenoidscontrol$Apocarotenoid)
shapiro.test(Carotenoidscontrol$Apocarotenoid)
## just not normal

ggdensity(Carotenoidscontrol$Canary.xanthophyll)
ggqqplot(Carotenoidscontrol$Canary.xanthophyll)
shapiro.test(Carotenoidscontrol$Canary.xanthophyll)
## normal

ggdensity(Carotenoidscontrol$Canthaxanthin)
ggqqplot(Carotenoidscontrol$Canthaxanthin)
shapiro.test(Carotenoidscontrol$Canthaxanthin)
## just not normal

ggdensity(Carotenoidscontrol$Xanthophyll)
ggqqplot(Carotenoidscontrol$Xanthophyll)
shapiro.test(Carotenoidscontrol$Xanthophyll)
## normal

ggdensity(Carotenoidscontrol$Echinenone)
ggqqplot(Carotenoidscontrol$Echinenone)
shapiro.test(Carotenoidscontrol$Echinenone)
## not normal

ggdensity(Carotenoidscontrol$cis.ketocarotenoid)
ggqqplot(Carotenoidscontrol$cis.ketocarotenoid)
shapiro.test(Carotenoidscontrol$cis.ketocarotenoid)
## normal

ggdensity(Carotenoidscontrol$X3.hydroxy.echinenone)
ggqqplot(Carotenoidscontrol$X3.hydroxy.echinenone)
shapiro.test(Carotenoidscontrol$X3.hydroxy.echinenone)
## normal

ggdensity(Carotenoidscontrol$lutein.ester.1)
ggqqplot(Carotenoidscontrol$lutein.ester.1)
shapiro.test(Carotenoidscontrol$lutein.ester.1)
## normal

ggdensity(Carotenoidscontrol$canary.xanthophyll.ester.1)
ggqqplot(Carotenoidscontrol$canary.xanthophyll.ester.1)
shapiro.test(Carotenoidscontrol$canary.xanthophyll.ester.1)
## normal

ggdensity(Carotenoidscontrol$beta.carotene)
ggqqplot(Carotenoidscontrol$beta.carotene)
shapiro.test(Carotenoidscontrol$beta.carotene)
## normal

ggdensity(Carotenoidscontrol$canary.xanthophyll.ester.2)
ggqqplot(Carotenoidscontrol$canary.xanthophyll.ester.2)
shapiro.test(Carotenoidscontrol$canary.xanthophyll.ester.2)
## normal

ggdensity(Carotenoidscontrol$ketocarotenoid.ester.1)
ggqqplot(Carotenoidscontrol$ketocarotenoid.ester.1)
shapiro.test(Carotenoidscontrol$ketocarotenoid.ester.1)
## not normal

ggdensity(Carotenoidscontrol$ketocarotenoid.ester.2)
ggqqplot(Carotenoidscontrol$ketocarotenoid.ester.2)
shapiro.test(Carotenoidscontrol$ketocarotenoid.ester.2)
## normal

ggdensity(Carotenoidscontrol$canary.xanthophyll.ester.3)
ggqqplot(Carotenoidscontrol$canary.xanthophyll.ester.3)
shapiro.test(Carotenoidscontrol$canary.xanthophyll.ester.3)
## not normal

ggdensity(Carotenoidscontrol$canthaxanthin.ester)
ggqqplot(Carotenoidscontrol$canthaxanthin.ester)
shapiro.test(Carotenoidscontrol$canthaxanthin.ester)
## normal

ggdensity(Carotenoidscontrol$X3HE.ester)
ggqqplot(Carotenoidscontrol$X3HE.ester)
shapiro.test(Carotenoidscontrol$X3HE.ester)
## normal

ggdensity(Carotenoidscontrol$ketocarotenoid.ester.3)
ggqqplot(Carotenoidscontrol$ketocarotenoid.ester.3)
shapiro.test(Carotenoidscontrol$ketocarotenoid.ester.3)
## normal

## T-tests Control vs Infected####
Carotenoidsf <- subset(Carotenoids,Sex == "F")
Carotenoidsm <- subset(Carotenoids,Sex == "M")

ggdensity(Carotenoids$sqrtApocarotenoid)
ggqqplot(Carotenoids$sqrtApocarotenoid)
ggdensity(Carotenoids$Apocarotenoid)
ggqqplot(Carotenoids$Apocarotenoid)
shapiro.test(Carotenoids$sqrtApocarotenoid)
shapiro.test(Carotenoids$Apocarotenoid)
leveneTest(sqrtApocarotenoid ~ Frog.Type, data = Carotenoids)
leveneTest(Apocarotenoid ~ Frog.Type, data = Carotenoids)
t.test(sqrtApocarotenoid ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
t.test(Apocarotenoid ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtApocarotenoid)
ggqqplot(Carotenoidsf$sqrtApocarotenoid)
ggdensity(Carotenoidsf$Apocarotenoid)
ggqqplot(Carotenoidsf$Apocarotenoid)
shapiro.test(Carotenoidsf$sqrtApocarotenoid)
shapiro.test(Carotenoidsf$Apocarotenoid)
leveneTest(sqrtApocarotenoid ~ Frog.Type, data = Carotenoidsf)
leveneTest(Apocarotenoid ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtApocarotenoid ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
t.test(Apocarotenoid ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtApocarotenoid)
ggqqplot(Carotenoidsm$sqrtApocarotenoid)
ggdensity(Carotenoidsm$Apocarotenoid)
ggqqplot(Carotenoidsm$Apocarotenoid)
shapiro.test(Carotenoidsm$sqrtApocarotenoid)
shapiro.test(Carotenoidsm$Apocarotenoid)
leveneTest(sqrtApocarotenoid ~ Frog.Type, data = Carotenoidsm)
leveneTest(Apocarotenoid ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtApocarotenoid ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
t.test(Apocarotenoid ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtCanary.xanthophyll)
ggqqplot(Carotenoids$sqrtCanary.xanthophyll)
ggdensity(Carotenoids$Canary.xanthophyll)
ggqqplot(Carotenoids$Canary.xanthophyll)
shapiro.test(Carotenoids$sqrtCanary.xanthophyll)
shapiro.test(Carotenoids$Canary.xanthophyll)
leveneTest(sqrtCanary.xanthophyll ~ Frog.Type, data = Carotenoids)
leveneTest(Canary.xanthophyll ~ Frog.Type, data = Carotenoids)
t.test(sqrtCanary.xanthophyll ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtCanary.xanthophyll)
ggqqplot(Carotenoidsf$sqrtCanary.xanthophyll)
ggdensity(Carotenoidsf$Canary.xanthophyll)
ggqqplot(Carotenoidsf$Canary.xanthophyll)
shapiro.test(Carotenoidsf$sqrtCanary.xanthophyll)
shapiro.test(Carotenoidsf$Canary.xanthophyll)
leveneTest(sqrtCanary.xanthophyll ~ Frog.Type, data = Carotenoidsf)
leveneTest(Canary.xanthophyll ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtCanary.xanthophyll ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtCanary.xanthophyll)
ggqqplot(Carotenoidsm$sqrtCanary.xanthophyll)
ggdensity(Carotenoidsm$Canary.xanthophyll)
ggqqplot(Carotenoidsm$Canary.xanthophyll)
shapiro.test(Carotenoidsm$sqrtCanary.xanthophyll)
shapiro.test(Carotenoidsm$Canary.xanthophyll)
leveneTest(sqrtCanary.xanthophyll ~ Frog.Type, data = Carotenoidsm)
leveneTest(Canary.xanthophyll ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtCanary.xanthophyll ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtCanthaxanthin)
ggqqplot(Carotenoids$sqrtCanthaxanthin)
shapiro.test(Carotenoids$sqrtCanthaxanthin)
leveneTest(sqrtCanthaxanthin ~ Frog.Type, data = Carotenoids)
t.test(sqrtCanthaxanthin ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtCanthaxanthin)
ggqqplot(Carotenoidsf$sqrtCanthaxanthin)
shapiro.test(Carotenoidsf$sqrtCanthaxanthin)
leveneTest(sqrtCanthaxanthin ~ Frog.Type, data = Carotenoidsf)
wilcox.test(sqrtCanthaxanthin ~ Frog.Type, data = Carotenoidsf, alternative = "two.sided", exact = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtCanthaxanthin)
ggqqplot(Carotenoidsm$sqrtCanthaxanthin)
shapiro.test(Carotenoidsm$sqrtCanthaxanthin)
leveneTest(sqrtCanthaxanthin ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtCanthaxanthin ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtXanthophyll)
ggqqplot(Carotenoids$sqrtXanthophyll)
shapiro.test(Carotenoids$sqrtXanthophyll)
leveneTest(sqrtXanthophyll ~ Frog.Type, data = Carotenoids)
t.test(sqrtXanthophyll ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtXanthophyll)
ggqqplot(Carotenoidsf$sqrtXanthophyll)
shapiro.test(Carotenoidsf$sqrtXanthophyll)
leveneTest(sqrtXanthophyll ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtXanthophyll ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtXanthophyll)
ggqqplot(Carotenoidsm$sqrtXanthophyll)
shapiro.test(Carotenoidsm$sqrtXanthophyll)
leveneTest(sqrtXanthophyll ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtXanthophyll ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtEchinenone)
ggqqplot(Carotenoids$sqrtEchinenone)
shapiro.test(Carotenoids$sqrtEchinenone)
leveneTest(sqrtEchinenone ~ Frog.Type, data = Carotenoids)
t.test(sqrtEchinenone ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtEchinenone)
ggqqplot(Carotenoidsf$sqrtEchinenone)
shapiro.test(Carotenoidsf$sqrtEchinenone)
leveneTest(sqrtEchinenone ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtEchinenone ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtEchinenone)
ggqqplot(Carotenoidsm$sqrtEchinenone)
shapiro.test(Carotenoidsm$sqrtEchinenone)
leveneTest(sqrtEchinenone ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtEchinenone ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtcis.ketocarotenoid)
ggqqplot(Carotenoids$sqrtcis.ketocarotenoid)
shapiro.test(Carotenoids$sqrtcis.ketocarotenoid)
leveneTest(sqrtcis.ketocarotenoid ~ Frog.Type, data = Carotenoids)
t.test(sqrtcis.ketocarotenoid ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtcis.ketocarotenoid)
ggqqplot(Carotenoidsf$sqrtcis.ketocarotenoid)
shapiro.test(Carotenoidsf$sqrtcis.ketocarotenoid)
leveneTest(sqrtcis.ketocarotenoid ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtcis.ketocarotenoid ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtcis.ketocarotenoid)
ggqqplot(Carotenoidsm$sqrtcis.ketocarotenoid)
shapiro.test(Carotenoidsm$sqrtcis.ketocarotenoid)
leveneTest(sqrtcis.ketocarotenoid ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtcis.ketocarotenoid ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrt3.hydroxy.echinenone)
ggqqplot(Carotenoids$sqrt3.hydroxy.echinenone)
shapiro.test(Carotenoids$sqrt3.hydroxy.echinenone)
leveneTest(sqrt3.hydroxy.echinenone ~ Frog.Type, data = Carotenoids)
t.test(sqrt3.hydroxy.echinenone ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrt3.hydroxy.echinenone)
ggqqplot(Carotenoidsf$sqrt3.hydroxy.echinenone)
shapiro.test(Carotenoidsf$sqrt3.hydroxy.echinenone)
leveneTest(sqrt3.hydroxy.echinenone ~ Frog.Type, data = Carotenoidsf)
t.test(sqrt3.hydroxy.echinenone ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrt3.hydroxy.echinenone)
ggqqplot(Carotenoidsm$sqrt3.hydroxy.echinenone)
shapiro.test(Carotenoidsm$sqrt3.hydroxy.echinenone)
leveneTest(sqrt3.hydroxy.echinenone ~ Frog.Type, data = Carotenoidsm)
t.test(sqrt3.hydroxy.echinenone ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtlutein.ester.1)
ggqqplot(Carotenoids$sqrtlutein.ester.1)
shapiro.test(Carotenoids$sqrtlutein.ester.1)
leveneTest(sqrtlutein.ester.1 ~ Frog.Type, data = Carotenoids)
t.test(sqrtlutein.ester.1 ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtlutein.ester.1)
ggqqplot(Carotenoidsf$sqrtlutein.ester.1)
shapiro.test(Carotenoidsf$sqrtlutein.ester.1)
leveneTest(sqrtlutein.ester.1 ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtlutein.ester.1 ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtlutein.ester.1)
ggqqplot(Carotenoidsm$sqrtlutein.ester.1)
shapiro.test(Carotenoidsm$sqrtlutein.ester.1)
leveneTest(sqrtlutein.ester.1 ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtlutein.ester.1 ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtcanary.xanthophyll.ester.1)
ggqqplot(Carotenoids$sqrtcanary.xanthophyll.ester.1)
shapiro.test(Carotenoids$sqrtcanary.xanthophyll.ester.1)
leveneTest(sqrtcanary.xanthophyll.ester.1 ~ Frog.Type, data = Carotenoids)
t.test(sqrtcanary.xanthophyll.ester.1 ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtcanary.xanthophyll.ester.1)
ggqqplot(Carotenoidsf$sqrtcanary.xanthophyll.ester.1)
shapiro.test(Carotenoidsf$sqrtcanary.xanthophyll.ester.1)
leveneTest(sqrtcanary.xanthophyll.ester.1 ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtcanary.xanthophyll.ester.1 ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtcanary.xanthophyll.ester.1)
ggqqplot(Carotenoidsm$sqrtcanary.xanthophyll.ester.1)
shapiro.test(Carotenoidsm$sqrtcanary.xanthophyll.ester.1)
leveneTest(sqrtcanary.xanthophyll.ester.1 ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtcanary.xanthophyll.ester.1 ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtbeta.carotene)
ggqqplot(Carotenoids$sqrtbeta.carotene)
shapiro.test(Carotenoids$sqrtbeta.carotene)
leveneTest(sqrtbeta.carotene ~ Frog.Type, data = Carotenoids)
t.test(sqrtbeta.carotene ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtbeta.carotene)
ggqqplot(Carotenoidsf$sqrtbeta.carotene)
shapiro.test(Carotenoidsf$sqrtbeta.carotene)
leveneTest(sqrtbeta.carotene ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtbeta.carotene ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtbeta.carotene)
ggqqplot(Carotenoidsm$sqrtbeta.carotene)
shapiro.test(Carotenoidsm$sqrtbeta.carotene)
leveneTest(sqrtbeta.carotene ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtbeta.carotene ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtcanary.xanthophyll.ester.2)
ggqqplot(Carotenoids$sqrtcanary.xanthophyll.ester.2)
shapiro.test(Carotenoids$sqrtcanary.xanthophyll.ester.2)
leveneTest(sqrtcanary.xanthophyll.ester.2 ~ Frog.Type, data = Carotenoids)
t.test(sqrtcanary.xanthophyll.ester.2 ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtcanary.xanthophyll.ester.2)
ggqqplot(Carotenoidsf$sqrtcanary.xanthophyll.ester.2)
shapiro.test(Carotenoidsf$sqrtcanary.xanthophyll.ester.2)
leveneTest(sqrtcanary.xanthophyll.ester.2 ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtcanary.xanthophyll.ester.2 ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtcanary.xanthophyll.ester.2)
ggqqplot(Carotenoidsm$sqrtcanary.xanthophyll.ester.2)
shapiro.test(Carotenoidsm$sqrtcanary.xanthophyll.ester.2)
leveneTest(sqrtcanary.xanthophyll.ester.2 ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtcanary.xanthophyll.ester.2 ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtketocarotenoid.ester.1)
ggqqplot(Carotenoids$sqrtketocarotenoid.ester.1)
shapiro.test(Carotenoids$sqrtketocarotenoid.ester.1)
leveneTest(sqrtketocarotenoid.ester.1 ~ Frog.Type, data = Carotenoids)
t.test(sqrtketocarotenoid.ester.1 ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtketocarotenoid.ester.1)
ggqqplot(Carotenoidsf$sqrtketocarotenoid.ester.1)
shapiro.test(Carotenoidsf$sqrtketocarotenoid.ester.1)
leveneTest(sqrtketocarotenoid.ester.1 ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtketocarotenoid.ester.1 ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtketocarotenoid.ester.1)
ggqqplot(Carotenoidsm$sqrtketocarotenoid.ester.1)
shapiro.test(Carotenoidsm$sqrtketocarotenoid.ester.1)
leveneTest(sqrtketocarotenoid.ester.1 ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtketocarotenoid.ester.1 ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtketocarotenoid.ester.2)
ggqqplot(Carotenoids$sqrtketocarotenoid.ester.2)
shapiro.test(Carotenoids$sqrtketocarotenoid.ester.2)
leveneTest(sqrtketocarotenoid.ester.2 ~ Frog.Type, data = Carotenoids)
t.test(sqrtketocarotenoid.ester.2 ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtketocarotenoid.ester.2)
ggqqplot(Carotenoidsf$sqrtketocarotenoid.ester.2)
shapiro.test(Carotenoidsf$sqrtketocarotenoid.ester.2)
leveneTest(sqrtketocarotenoid.ester.2 ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtketocarotenoid.ester.2 ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtketocarotenoid.ester.2)
ggqqplot(Carotenoidsm$sqrtketocarotenoid.ester.2)
shapiro.test(Carotenoidsm$sqrtketocarotenoid.ester.2)
leveneTest(sqrtketocarotenoid.ester.2 ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtketocarotenoid.ester.2 ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtcanary.xanthophyll.ester.3)
ggqqplot(Carotenoids$sqrtcanary.xanthophyll.ester.3)
shapiro.test(Carotenoids$sqrtcanary.xanthophyll.ester.3)
leveneTest(sqrtcanary.xanthophyll.ester.3 ~ Frog.Type, data = Carotenoids)
t.test(sqrtcanary.xanthophyll.ester.3 ~ Frog.Type, data = Carotenoids, var.equal = FALSE)
## not significant

ggdensity(Carotenoidsf$sqrtcanary.xanthophyll.ester.3)
ggqqplot(Carotenoidsf$sqrtcanary.xanthophyll.ester.3)
shapiro.test(Carotenoidsf$sqrtcanary.xanthophyll.ester.3)
leveneTest(sqrtcanary.xanthophyll.ester.3 ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtcanary.xanthophyll.ester.3 ~ Frog.Type, data = Carotenoidsf, var.equal = FALSE)
## not significant

ggdensity(Carotenoidsm$sqrtcanary.xanthophyll.ester.3)
ggqqplot(Carotenoidsm$sqrtcanary.xanthophyll.ester.3)
shapiro.test(Carotenoidsm$sqrtcanary.xanthophyll.ester.3)
leveneTest(sqrtcanary.xanthophyll.ester.3 ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtcanary.xanthophyll.ester.3 ~ Frog.Type, data = Carotenoidsm, var.equal = FALSE)
## not significant

ggdensity(Carotenoids$sqrtcanthaxanthin.ester)
ggqqplot(Carotenoids$sqrtcanthaxanthin.ester)
shapiro.test(Carotenoids$sqrtcanthaxanthin.ester)
leveneTest(sqrtcanthaxanthin.ester ~ Frog.Type, data = Carotenoids)
t.test(sqrtcanthaxanthin.ester ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtcanthaxanthin.ester)
ggqqplot(Carotenoidsf$sqrtcanthaxanthin.ester)
shapiro.test(Carotenoidsf$sqrtcanthaxanthin.ester)
leveneTest(sqrtcanthaxanthin.ester ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtcanthaxanthin.ester ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtcanthaxanthin.ester)
ggqqplot(Carotenoidsm$sqrtcanthaxanthin.ester)
shapiro.test(Carotenoidsm$sqrtcanthaxanthin.ester)
leveneTest(sqrtcanthaxanthin.ester ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtcanthaxanthin.ester ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrt3HE.ester)
ggqqplot(Carotenoids$sqrt3HE.ester)
shapiro.test(Carotenoids$sqrt3HE.ester)
leveneTest(sqrt3HE.ester ~ Frog.Type, data = Carotenoids)
t.test(sqrt3HE.ester ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrt3HE.ester)
ggqqplot(Carotenoidsf$sqrt3HE.ester)
shapiro.test(Carotenoidsf$sqrt3HE.ester)
leveneTest(sqrt3HE.ester ~ Frog.Type, data = Carotenoidsf)
t.test(sqrt3HE.ester ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrt3HE.ester)
ggqqplot(Carotenoidsm$sqrt3HE.ester)
shapiro.test(Carotenoidsm$sqrt3HE.ester)
leveneTest(sqrt3HE.ester ~ Frog.Type, data = Carotenoidsm)
t.test(sqrt3HE.ester ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

ggdensity(Carotenoids$sqrtketocarotenoid.ester.3)
ggqqplot(Carotenoids$sqrtketocarotenoid.ester.3)
shapiro.test(Carotenoids$sqrtketocarotenoid.ester.3)
leveneTest(sqrtketocarotenoid.ester.3 ~ Frog.Type, data = Carotenoids)
t.test(sqrtketocarotenoid.ester.3 ~ Frog.Type, data = Carotenoids, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsf$sqrtketocarotenoid.ester.3)
ggqqplot(Carotenoidsf$sqrtketocarotenoid.ester.3)
shapiro.test(Carotenoidsf$sqrtketocarotenoid.ester.3)
leveneTest(sqrtketocarotenoid.ester.3 ~ Frog.Type, data = Carotenoidsf)
t.test(sqrtketocarotenoid.ester.3 ~ Frog.Type, data = Carotenoidsf, var.equal = TRUE)
## not significant

ggdensity(Carotenoidsm$sqrtketocarotenoid.ester.3)
ggqqplot(Carotenoidsm$sqrtketocarotenoid.ester.3)
shapiro.test(Carotenoidsm$sqrtketocarotenoid.ester.3)
leveneTest(sqrtketocarotenoid.ester.3 ~ Frog.Type, data = Carotenoidsm)
t.test(sqrtketocarotenoid.ester.3 ~ Frog.Type, data = Carotenoidsm, var.equal = TRUE)
## not significant

## Boxplots quantiles ####
Carotenoidsboxplot <-read.csv(file.choose())
Carotenoidsboxplotapo <- subset(Carotenoidsboxplot,Carotenoid == "Apocarotenoid")
Carotenoidsboxplotcanxan <- subset(Carotenoidsboxplot,Carotenoid == "Canary Xanthophyll")
Carotenoidsboxplotcanxan1 <- subset(Carotenoidsboxplot,Carotenoid == "Canary Xanthophyll Ester 1")
Carotenoidsboxplotcanxan2 <- subset(Carotenoidsboxplot,Carotenoid == "Canary Xanthophyll Ester 2")
Carotenoidsboxplotcanxan3 <- subset(Carotenoidsboxplot,Carotenoid == "Canary Xanthophyll Ester 3")
Carotenoidsboxplotxan <- subset(Carotenoidsboxplot,Carotenoid == "Xanthophyll")
Carotenoidsboxplotbeta <- subset(Carotenoidsboxplot,Carotenoid == "Beta Carotene")
Carotenoidsboxplotech <- subset(Carotenoidsboxplot,Carotenoid == "Echinenone")
Carotenoidsboxplotcisket <- subset(Carotenoidsboxplot,Carotenoid == "cis Ketocarotenoid")
Carotenoidsboxplotket1 <- subset(Carotenoidsboxplot,Carotenoid == "Ketocarotenoid Ester 1")
Carotenoidsboxplotket2 <- subset(Carotenoidsboxplot,Carotenoid == "Ketocarotenoid Ester 2")
Carotenoidsboxplotket3 <- subset(Carotenoidsboxplot,Carotenoid == "Ketocarotenoid Ester 3")
Carotenoidsboxplotcan <- subset(Carotenoidsboxplot,Carotenoid == "Canthaxanthin")
Carotenoidsboxplotcanest <- subset(Carotenoidsboxplot,Carotenoid == "Canthaxanthin Ester")
Carotenoidsboxplotcanxhe <- subset(Carotenoidsboxplot,Carotenoid == "3-Hydroxy Echinenone")
Carotenoidsboxplotcanx3h3 <- subset(Carotenoidsboxplot,Carotenoid == "X3H3 Ester")
Carotenoidsboxplotlut <- subset(Carotenoidsboxplot,Carotenoid == "Lutein Ester 1")
## Individial
Apo <- ggplot(Carotenoidsboxplotapo, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,5)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=1.68, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=1.46, label= "x", fontface = "bold", size = 2)
Apo

Canxan <- ggplot(Carotenoidsboxplotcanxan, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,50)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=26.15, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=23.21, label= "x", fontface = "bold", size = 2)
Canxan

Canxan1 <- ggplot(Carotenoidsboxplotcanxan1, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,80)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=28.17, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=24.05, label= "x", fontface = "bold", size = 2)
Canxan1

Canxan2 <- ggplot(Carotenoidsboxplotcanxan2, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.7)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,120)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=56.40, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=51.19, label= "x", fontface = "bold", size = 2)
Canxan2

Canxan3 <- ggplot(Carotenoidsboxplotcanxan3, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.7)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,40)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=22.29, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=20.73, label= "x", fontface = "bold", size = 2)
Canxan3

Xan <- ggplot(Carotenoidsboxplotxan, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,30)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=8.52, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=7.48, label= "x", fontface = "bold", size = 2)
Xan

Beta <- ggplot(Carotenoidsboxplotbeta, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 400, by = 100))+
  expand_limits(y = 400)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=167.83, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=129.18, label= "x", fontface = "bold", size = 2)
Beta

Ech <- ggplot(Carotenoidsboxplotech, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 30, by = 10))+
  expand_limits(y = 30)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=8.46, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=6.05, label= "x", fontface = "bold", size = 2)
Ech

Cisk <- ggplot(Carotenoidsboxplotcisket, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 20, by = 5))+
  expand_limits(y = 20)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=7.29, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=5.91, label= "x", fontface = "bold", size = 2)
Cisk

Ket1 <- ggplot(Carotenoidsboxplotket1, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,30)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=9.93, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=7.47, label= "x", fontface = "bold", size = 2)
Ket1

Ket2 <- ggplot(Carotenoidsboxplotket2, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 15, by = 5))+
  expand_limits(y = 15)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=5.42, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=5.04, label= "x", fontface = "bold", size = 2)
Ket2

Ket3 <- ggplot(Carotenoidsboxplotket3, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 60, by = 20))+
  expand_limits(y = 60)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=26.42, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=23.19, label= "x", fontface = "bold", size = 2)
Ket3

Canth <- ggplot(Carotenoidsboxplotcan, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,30)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=9.24, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=7.01, label= "x", fontface = "bold", size = 2)
Canth

Canthest <- ggplot(Carotenoidsboxplotcanest, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 50, by = 10))+
  expand_limits(y = 50)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=14.58, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=13.25, label= "x", fontface = "bold", size = 2)
Canthest

XHE <- ggplot(Carotenoidsboxplotcanxhe, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 70, by = 20))+
  expand_limits(y = 70)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=25.19, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=21.98, label= "x", fontface = "bold", size = 2)
XHE

X3H3ester <- ggplot(Carotenoidsboxplotcanx3h3, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 60, by = 20))+
  expand_limits(y = 60)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=24.83, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=23.19, label= "x", fontface = "bold", size = 2)
X3H3ester

Lut <- ggplot(Carotenoidsboxplotlut, aes(x=Carotenoid, y=Value, color=Frog.Type)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = seq(0, 80, by = 20))+
  expand_limits(y = 80)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=24.93, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=21.90, label= "x", fontface = "bold", size = 2)
Lut

top_row <- ggarrange(Apo,Beta,Ech,XHE,X3H3ester, ncol=5,nrow=1, common.legend = TRUE)
second_row <- ggarrange(Xan, Canxan, Canxan1, Canxan2, Canxan3, ncol=5, nrow=1)
third_row <- ggarrange(Cisk,Ket1,Ket2,Ket3,Lut, ncol=5, nrow=1)
bottom_row <- ggarrange(Canth,Canthest, ncol=3, nrow=1)

blank <- ggplot() + theme_void()
boxplotlegend <- as_ggplot(get_legend(Apo))


figure <- ggarrange(Apo,Beta,Ech,XHE,X3H3ester,
                    Xan, Canxan, Canxan1, Canxan2, Canxan3,
                    Cisk,Ket1,Ket2,Ket3,Lut,
                    blank, Canth, Canthest, boxplotlegend, blank,
                    ncol=5, nrow=4, legend = "none")
figure
annotate_figure(figure,
                left = text_grob("µg carotenoid per g skin", rot = 90))


## Full boxplot, probably won't use because too hard to control
ggplot(Carotenoidsboxplot, aes(x=Carotenoid, y=Value, fill=Frog.Type)) + 
  geom_boxplot()+
  facet_wrap(~factor(Carotenoid, c("Apocarotenoid", "beta carotene","Xanthophyll", "Canary Xanthophyll", "canary xanthophyll ester 1",
                                   "canary xanthophyll ester 2", "canary xanthophyll ester 3", "Echinenone", "3-hydroxy-echinenone",
                                   "X3H3 ester", "cis.ketocarotenoid", "ketocarotenoid ester 1", "ketocarotenoid ester 2",
                                   "ketocarotenoid ester 3", "lutein ester 1", "Canthaxanthin")), scale="free")+
  stat_summary(fun = mean, color = "darkred", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 3,
               show.legend = FALSE)+
  theme(strip.text.x = element_blank(), strip.background = element_blank())

## Boxplots mean se ####
Carotenoidsboxplotmeanse <-read.csv(file.choose())
Carotenoidsboxplotmeanapo <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Apocarotenoid")
Carotenoidsboxplotmeancanxan <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Canary Xanthophyll")
Carotenoidsboxplotmeancanxan1 <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Canary Xanthophyll Ester 1")
Carotenoidsboxplotmeancanxan2 <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Canary Xanthophyll Ester 2")
Carotenoidsboxplotmeancanxan3 <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Canary Xanthophyll Ester 3")
Carotenoidsboxplotmeanxan <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Xanthophyll")
Carotenoidsboxplotmeanbeta <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Beta Carotene")
Carotenoidsboxplotmeanech <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Echinenone")
Carotenoidsboxplotmeancisket <- subset(Carotenoidsboxplotmeanse,Carotenoid == "cis Ketocarotenoid")
Carotenoidsboxplotmeanket1 <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Ketocarotenoid Ester 1")
Carotenoidsboxplotmeanket2 <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Ketocarotenoid Ester 2")
Carotenoidsboxplotmeanket3 <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Ketocarotenoid Ester 3")
Carotenoidsboxplotmeancan <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Canthaxanthin")
Carotenoidsboxplotmeancanest <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Canthaxanthin Ester")
Carotenoidsboxplotmeancanxhe <- subset(Carotenoidsboxplotmeanse,Carotenoid == "3-Hydroxy Echinenone")
Carotenoidsboxplotmeancanx3h3 <- subset(Carotenoidsboxplotmeanse,Carotenoid == "X3H3 Ester")
Carotenoidsboxplotmeanlut <- subset(Carotenoidsboxplotmeanse,Carotenoid == "Lutein Ester 1")

Apomean <- ggplot(Carotenoidsboxplotmeanapo, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,3)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=1.68, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=1.46, label= "x", fontface = "bold", size = 2)
Apomean

Canxanmean <- ggplot(Carotenoidsboxplotmeancanxan, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,40)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=26.15, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=23.21, label= "x", fontface = "bold", size = 2)
Canxanmean

Canxan1mean <- ggplot(Carotenoidsboxplotmeancanxan1, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,40)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=28.17, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=24.05, label= "x", fontface = "bold", size = 2)
Canxan1mean

Canxan2mean <- ggplot(Carotenoidsboxplotmeancanxan2, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,80)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=56.40, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=51.19, label= "x", fontface = "bold", size = 2)
Canxan2mean

Canxan3mean <- ggplot(Carotenoidsboxplotmeancanxan3, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,30)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=22.29, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=20.73, label= "x", fontface = "bold", size = 2)
Canxan3mean

Xanmean <- ggplot(Carotenoidsboxplotmeanxan, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,15)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=8.52, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=7.48, label= "x", fontface = "bold", size = 2)
Xanmean

Betamean <- ggplot(Carotenoidsboxplotmeanbeta, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,200)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=167.83, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=129.18, label= "x", fontface = "bold", size = 2)
Betamean

Echmean <- ggplot(Carotenoidsboxplotmeanech, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,15)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=8.46, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=6.05, label= "x", fontface = "bold", size = 2)
Echmean

Cisketmean <- ggplot(Carotenoidsboxplotmeancisket, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,10)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=7.29, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=5.91, label= "x", fontface = "bold", size = 2)
Cisketmean

Ket1mean <- ggplot(Carotenoidsboxplotmeanket1, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,15)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=9.93, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=7.47, label= "x", fontface = "bold", size = 2)
Ket1mean

Ket2mean <- ggplot(Carotenoidsboxplotmeanket2, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,10)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=5.42, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=5.04, label= "x", fontface = "bold", size = 2)
Ket2mean

Ket3mean <- ggplot(Carotenoidsboxplotmeanket3, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,40)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=26.42, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=23.19, label= "x", fontface = "bold", size = 2)
Ket3mean

Canmean <- ggplot(Carotenoidsboxplotmeancan, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,15)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=9.24, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=7.01, label= "x", fontface = "bold", size = 2)
Canmean

Canestmean <- ggplot(Carotenoidsboxplotmeancanest, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,20)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=14.58, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=13.25, label= "x", fontface = "bold", size = 2)
Canestmean

Xhemean <- ggplot(Carotenoidsboxplotmeancanxhe, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,40)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=25.19, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=21.98, label= "x", fontface = "bold", size = 2)
Xhemean

X3h3mean <- ggplot(Carotenoidsboxplotmeancanx3h3, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,30)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=24.83, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=23.19, label= "x", fontface = "bold", size = 2)
X3h3mean

Lutmean <- ggplot(Carotenoidsboxplotmeanlut, aes(x = Carotenoid, color = Frog.Type)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,30)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=24.33, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=21.90, label= "x", fontface = "bold", size = 2)
Lutmean


blank <- ggplot() + theme_void()
boxplotlegendmean <- as_ggplot(get_legend(Apomean))


figuremean <- ggarrange(Apomean,Betamean,Echmean,Xhemean,X3h3mean,
                    Xanmean, Canxanmean, Canxan1mean, Canxan2mean, Canxan3mean,
                    Cisketmean,Ket1mean,Ket2mean,Ket3mean,Lutmean,
                    blank, Canmean, Canestmean, boxplotlegendmean, blank,
                    ncol=5, nrow=4, legend = "none")
figuremean
annotate_figure(figuremean,
                left = text_grob("µg carotenoid per µg skin", rot = 90))

## Compare to Crothers means ####
install.packages("BSDA")
library(BSDA)

## Apo Control vs Crothers
tsum.test(mean.x=1.68,   s.x=.46, n.x=9,
          mean.y=5.67, s.y=4.88, n.y=19)
## sig diff

## Canxan Control vs Crothers
tsum.test(mean.x=26.15,   s.x=14.84, n.x=9,
          mean.y=21.29, s.y=17.26, n.y=19)
## not sig

## Canxan1 Control vs Crothers
tsum.test(mean.x=28.17,   s.x=7.36, n.x=9,
          mean.y=56.55, s.y=55.92, n.y=19)

## Canxan2 Control vs Crothers
tsum.test(mean.x=56.40,   s.x=34.22, n.x=9,
          mean.y=76.67, s.y=63.44, n.y=19)

## Xan Control vs Crothers
tsum.test(mean.x=8.52,   s.x=4.63, n.x=9,
          mean.y=6.47, s.y=6.73, n.y=19)

## Canthest Control vs Crothers
tsum.test(mean.x=14.58,   s.x=4.50, n.x=9,
          mean.y=50.64, s.y=32.31, n.y=19)

## X3Hydr Control vs Crothers
tsum.test(mean.x=25.19,   s.x=18.13, n.x=9,
          mean.y=47.31, s.y=43.29, n.y=19)

## Beta Control vs Crothers
tsum.test(mean.x=167.83,   s.x=78.23, n.x=9,
          mean.y=195.08, s.y=134.50, n.y=19)

## Lut Control vs Crothers
tsum.test(mean.x=24.33,   s.x=14.88, n.x=9,
          mean.y=34.76, s.y=27.05, n.y=19)

## cisk Control vs Crothers
tsum.test(mean.x=7.29,   s.x=4.58, n.x=9,
          mean.y=8.30, s.y=9.99, n.y=19)

## ket2 Control vs Crothers
tsum.test(mean.x=5.42,   s.x=2.04, n.x=9,
          mean.y=2.63, s.y=8.45, n.y=19)

## Sup vs Unsup T-tests ####
Carotenoidssupunsup <- read.csv(file.choose())

ggdensity(Carotenoidssupunsup$sqrtApocarotenoid)
ggqqplot(Carotenoidssupunsup$sqrtApocarotenoid)
shapiro.test(Carotenoidssupunsup$sqrtApocarotenoid)
leveneTest(sqrtApocarotenoid ~ Group, data = Carotenoidssupunsup)
wilcox.test(sqrtApocarotenoid ~ Group, data = Carotenoidssupunsup, alternative = "two.sided", exact = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtCanaryXanthophyll)
ggqqplot(Carotenoidssupunsup$sqrtCanaryXanthophyll)
shapiro.test(Carotenoidssupunsup$sqrtCanaryXanthophyll)
leveneTest(sqrtCanaryXanthophyll ~ Group, data = Carotenoidssupunsup)
t.test(sqrtCanaryXanthophyll ~ Group, data = Carotenoidssupunsup, var.equal = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtCanthaxanthin)
ggqqplot(Carotenoidssupunsup$sqrtCanthaxanthin)
shapiro.test(Carotenoidssupunsup$sqrtCanthaxanthin)
leveneTest(sqrtCanthaxanthin ~ Group, data = Carotenoidssupunsup)
t.test(sqrtCanthaxanthin ~ Group, data = Carotenoidssupunsup, var.equal = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtXanthophyll)
ggqqplot(Carotenoidssupunsup$sqrtXanthophyll)
shapiro.test(Carotenoidssupunsup$sqrtXanthophyll)
leveneTest(sqrtXanthophyll ~ Group, data = Carotenoidssupunsup)
t.test(sqrtXanthophyll ~ Group, data = Carotenoidssupunsup, var.equal = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtcisketocarotenoid)
ggqqplot(Carotenoidssupunsup$sqrtcisketocarotenoid)
shapiro.test(Carotenoidssupunsup$sqrtcisketocarotenoid)
leveneTest(sqrtcisketocarotenoid ~ Group, data = Carotenoidssupunsup)
t.test(sqrtcisketocarotenoid ~ Group, data = Carotenoidssupunsup, var.equal = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtEchinenone)
ggqqplot(Carotenoidssupunsup$sqrtEchinenone)
shapiro.test(Carotenoidssupunsup$sqrtEchinenone)
leveneTest(sqrtEchinenone ~ Group, data = Carotenoidssupunsup)
wilcox.test(sqrtEchinenone ~ Group, data = Carotenoidssupunsup, alternative = "two.sided", exact = TRUE)
## significant
## df is 26

ggdensity(Carotenoidssupunsup$sqrt3HE)
ggqqplot(Carotenoidssupunsup$sqrt3HE)
shapiro.test(Carotenoidssupunsup$sqrt3HE)
leveneTest(sqrt3HE ~ Group, data = Carotenoidssupunsup)
t.test(sqrt3HE ~ Group, data = Carotenoidssupunsup, var.equal = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtluteinester1)
ggqqplot(Carotenoidssupunsup$sqrtluteinester1)
shapiro.test(Carotenoidssupunsup$sqrtluteinester1)
leveneTest(sqrtluteinester1 ~ Group, data = Carotenoidssupunsup)
t.test(sqrtluteinester1 ~ Group, data = Carotenoidssupunsup, var.equal = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtcanaryxanthophyllester1)
ggqqplot(Carotenoidssupunsup$sqrtcanaryxanthophyllester1)
shapiro.test(Carotenoidssupunsup$sqrtcanaryxanthophyllester1)
leveneTest(sqrtcanaryxanthophyllester1 ~ Group, data = Carotenoidssupunsup)
wilcox.test(sqrtcanaryxanthophyllester1 ~ Group, data = Carotenoidssupunsup, alternative = "two.sided", exact = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtbetacarotene)
ggqqplot(Carotenoidssupunsup$sqrtbetacarotene)
shapiro.test(Carotenoidssupunsup$sqrtbetacarotene)
leveneTest(sqrtbetacarotene ~ Group, data = Carotenoidssupunsup)
t.test(sqrtbetacarotene ~ Group, data = Carotenoidssupunsup, var.equal = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtcanaryxanthophyllester2)
ggqqplot(Carotenoidssupunsup$sqrtcanaryxanthophyllester2)
shapiro.test(Carotenoidssupunsup$sqrtcanaryxanthophyllester2)
leveneTest(sqrtcanaryxanthophyllester2 ~ Group, data = Carotenoidssupunsup)
t.test(sqrtcanaryxanthophyllester2 ~ Group, data = Carotenoidssupunsup, var.equal = TRUE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtketocarotenoidester2)
ggqqplot(Carotenoidssupunsup$sqrtketocarotenoidester2)
shapiro.test(Carotenoidssupunsup$sqrtketocarotenoidester2)
leveneTest(sqrtketocarotenoidester2 ~ Group, data = Carotenoidssupunsup)
wilcox.test(sqrtketocarotenoidester2 ~ Group, data = Carotenoidssupunsup, alternative = "two.sided", exact = TRUE)
## significant

ggdensity(Carotenoidssupunsup$sqrtcanaryxanthophyllester3)
ggqqplot(Carotenoidssupunsup$sqrtcanaryxanthophyllester3)
shapiro.test(Carotenoidssupunsup$sqrtcanaryxanthophyllester3)
leveneTest(sqrtcanaryxanthophyllester3 ~ Group, data = Carotenoidssupunsup)
t.test(sqrtcanaryxanthophyllester3 ~ Group, data = Carotenoidssupunsup, var.equal = FALSE)
## not significant

ggdensity(Carotenoidssupunsup$sqrtcanthaxanthinester)
ggqqplot(Carotenoidssupunsup$sqrtcanthaxanthinester)
shapiro.test(Carotenoidssupunsup$sqrtcanthaxanthinester)
leveneTest(sqrtcanthaxanthinester ~ Group, data = Carotenoidssupunsup)
t.test(sqrtcanthaxanthinester ~ Group, data = Carotenoidssupunsup, var.equal = FALSE)
## significant

## Sup vs Unsup Boxplots ####

Carotenoidssupunsupboxplot <-read.csv(file.choose())
Carotenoidssupunsupboxplotapo <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Apocarotenoid")
Carotenoidssupunsupboxplotcanxan <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Canary Xanthophyll")
Carotenoidssupunsupboxplotcanxan1 <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Canary Xanthophyll Ester 1")
Carotenoidssupunsupboxplotcanxan2 <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Canary Xanthophyll Ester 2")
Carotenoidssupunsupboxplotcanxan3 <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Canary Xanthophyll Ester 3")
Carotenoidssupunsupboxplotxan <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Xanthophyll")
Carotenoidssupunsupboxplotbeta <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Beta Carotene")
Carotenoidssupunsupboxplotech <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Echinenone")
Carotenoidssupunsupboxplotcisket <- subset(Carotenoidssupunsupboxplot,Carotenoid == "cis Ketocarotenoid")
Carotenoidssupunsupboxplotket2 <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Ketocarotenoid Ester 2")
Carotenoidssupunsupboxplotcan <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Canthaxanthin")
Carotenoidssupunsupboxplotcanest <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Canthaxanthin Ester")
Carotenoidssupunsupboxplotcanxhe <- subset(Carotenoidssupunsupboxplot,Carotenoid == "3-Hydroxy Echinenone")
Carotenoidssupunsupboxplotlut <- subset(Carotenoidssupunsupboxplot,Carotenoid == "Lutein Ester 1")

Aposup <- ggplot(Carotenoidssupunsupboxplotapo, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,15)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=1.68, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=5.67, label= "x", fontface = "bold", size = 2)
Aposup

Canxansup <- ggplot(Carotenoidssupunsupboxplotcanxan, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,80)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=26.15, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=21.29, label= "x", fontface = "bold", size = 2)
Canxansup

Canxan1sup <- ggplot(Carotenoidssupunsupboxplotcanxan1, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.7)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,250)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=28.17, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=56.55, label= "x", fontface = "bold", size = 2)
Canxan1sup

Canxan2sup <- ggplot(Carotenoidssupunsupboxplotcanxan2, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.7)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,250)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=56.40, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=76.67, label= "x", fontface = "bold", size = 2)
Canxan2sup

Canxan3sup <- ggplot(Carotenoidssupunsupboxplotcanxan3, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.7)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,100)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=22.29, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=30.86, label= "x", fontface = "bold", size = 2)
Canxan3sup

Xansup <- ggplot(Carotenoidssupunsupboxplotxan, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,30)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=8.52, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=6.47, label= "x", fontface = "bold", size = 2)
Xansup

Betasup <- ggplot(Carotenoidssupunsupboxplotbeta, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,500)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=167.83, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=195.08, label= "x", fontface = "bold", size = 2)
Betasup

Echsup <- ggplot(Carotenoidssupunsupboxplotech, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,60)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=8.46, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=4.76, label= "x", fontface = "bold", size = 2)+
  geom_signif(y_position = c(55), xmin = c(0.81), xmax = c(1.19),
              annotation = c("***"), tip_length = 0, textsize = 5, color = "black")
Echsup

Cisksup <- ggplot(Carotenoidssupunsupboxplotcisket, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,50)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=7.29, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=8.30, label= "x", fontface = "bold", size = 2)
Cisksup

Ket2sup <- ggplot(Carotenoidssupunsupboxplotket2, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,40)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=5.42, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=2.63, label= "x", fontface = "bold", size = 2)+
  geom_signif(y_position = c(37), xmin = c(0.81), xmax = c(1.19),
              annotation = c("***"), tip_length = 0, textsize = 5, color = "black")
Ket2sup

Canthsup <- ggplot(Carotenoidssupunsupboxplotcan, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,80)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=9.24, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=11.93, label= "x", fontface = "bold", size = 2)
Canthsup

Canthestsup <- ggplot(Carotenoidssupunsupboxplotcanest, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,150)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=14.58, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=50.64, label= "x", fontface = "bold", size = 2)+
  geom_signif(y_position = c(140), xmin = c(0.81), xmax = c(1.19),
              annotation = c("***"), tip_length = 0, textsize = 5, color = "black")
Canthestsup

XHEsup <- ggplot(Carotenoidssupunsupboxplotcanxhe, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,200)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=25.19, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=47.31, label= "x", fontface = "bold", size = 2)
XHEsup

Lutsup <- ggplot(Carotenoidssupunsupboxplotlut, aes(x=Carotenoid, y=Value, color=Group)) + 
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylim(0,100)+
  scale_color_manual(values=c("darkgrey","black"))+
  scale_fill_manual(values=c("white","white"))+
  annotate("text", x=0.81, y=24.93, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.19, y=34.76, label= "x", fontface = "bold", size = 2)
Lutsup

blank <- ggplot() + theme_void()
boxplotlegend <- as_ggplot(get_legend(Aposup))


figuresup <- ggarrange(Aposup,Betasup,Echsup,XHEsup,
                    Xansup, Canxansup, Canxan1sup, Canxan2sup, Canxan3sup,
                    Cisksup,Ket2sup,Lutsup,Canthsup, Canthestsup, boxplotlegend, 
                    ncol=5, nrow=3, legend = "none")
figuresup
annotate_figure(figuresup,
                left = text_grob("µg carotenoid per g skin", rot = 90))

## Sup vs Unsup Mean SE ####

Carotenoidssupunsupboxplotmeanse <-read.csv(file.choose())
Carotenoidssupunsupboxplotmeanapo <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Apocarotenoid")
Carotenoidssupunsupboxplotmeancanxan <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Canary Xanthophyll")
Carotenoidssupunsupboxplotmeancanxan1 <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Canary Xanthophyll Ester 1")
Carotenoidssupunsupboxplotmeancanxan2 <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Canary Xanthophyll Ester 2")
Carotenoidssupunsupboxplotmeancanxan3 <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Canary Xanthophyll Ester 3")
Carotenoidssupunsupboxplotmeanxan <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Xanthophyll")
Carotenoidssupunsupboxplotmeanbeta <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Beta Carotene")
Carotenoidssupunsupboxplotmeanech <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Echinenone")
Carotenoidssupunsupboxplotmeancisket <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "cis Ketocarotenoid")
Carotenoidssupunsupboxplotmeanket2 <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Ketocarotenoid Ester 2")
Carotenoidssupunsupboxplotmeancan <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Canthaxanthin")
Carotenoidssupunsupboxplotmeancanest <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Canthaxanthin Ester")
Carotenoidssupunsupboxplotmeancanxhe <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "3-Hydroxy Echinenone")
Carotenoidssupunsupboxplotmeanlut <- subset(Carotenoidssupunsupboxplotmeanse,Carotenoid == "Lutein Ester 1")

Apomeansup <- ggplot(Carotenoidssupunsupboxplotmeanapo, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,10)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=1.68, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=5.67, label= "x", fontface = "bold", size = 2)
Apomeansup

Canxanmeansup <- ggplot(Carotenoidssupunsupboxplotmeancanxan, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,40)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=26.15, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=21.29, label= "x", fontface = "bold", size = 2)
Canxanmeansup

Canxan1meansup <- ggplot(Carotenoidssupunsupboxplotmeancanxan1, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,100)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.7)))+
  annotate("text", x=0.85, y=28.17, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=56.55, label= "x", fontface = "bold", size = 2)
Canxan1meansup

Canxan2meansup <- ggplot(Carotenoidssupunsupboxplotmeancanxan2, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,100)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=56.40, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=76.67, label= "x", fontface = "bold", size = 2)
Canxan2meansup

Canxan3meansup <- ggplot(Carotenoidssupunsupboxplotmeancanxan3, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,40)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=22.29, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=30.86, label= "x", fontface = "bold", size = 2)
Canxan3meansup

Xanmeansup <- ggplot(Carotenoidssupunsupboxplotmeanxan, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,15)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=8.52, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=6.47, label= "x", fontface = "bold", size = 2)
Xanmeansup

Betameansup <- ggplot(Carotenoidssupunsupboxplotmeanbeta, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,250)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=167.83, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=195.08, label= "x", fontface = "bold", size = 2)
Betameansup

Echmeansup <- ggplot(Carotenoidssupunsupboxplotmeanech, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,15)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=8.46, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=4.76, label= "x", fontface = "bold", size = 2)
Echmeansup

Cisketmeansup <- ggplot(Carotenoidssupunsupboxplotmeancisket, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,15)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=7.29, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=8.30, label= "x", fontface = "bold", size = 2)
Cisketmeansup

Ket2meansup <- ggplot(Carotenoidssupunsupboxplotmeanket2, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,10)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=5.42, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=2.63, label= "x", fontface = "bold", size = 2)
Ket2meansup

Canmeansup <- ggplot(Carotenoidssupunsupboxplotmeancan, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,20)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=9.24, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=11.93, label= "x", fontface = "bold", size = 2)
Canmeansup

Canestmeansup <- ggplot(Carotenoidssupunsupboxplotmeancanest, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,60)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=14.58, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=50.64, label= "x", fontface = "bold", size = 2)
Canestmeansup

Xhemeansup <- ggplot(Carotenoidssupunsupboxplotmeancanxhe, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,60)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=25.19, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=47.31, label= "x", fontface = "bold", size = 2)
Xhemeansup

Lutmeansup <- ggplot(Carotenoidssupunsupboxplotmeanlut, aes(x = Carotenoid, color = Group)) +
  geom_errorbar(aes(ymax = Mean + SE, ymin = Mean - SE),
                position = "dodge", width = 0.6)+
  labs(color = "Frog Type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_color_manual(values=c("darkgrey","black"))+
  ylim(0,50)+
  theme(axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(),axis.text.x=element_text(size=rel(0.75)))+
  annotate("text", x=0.85, y=24.33, label= "x", fontface = "bold", size = 2)+
  annotate("text", x=1.15, y=34.76, label= "x", fontface = "bold", size = 2)
Lutmeansup

blank <- ggplot() + theme_void()
boxplotlegendmeansup <- as_ggplot(get_legend(Aposup))


figuremeansup <- ggarrange(Apomeansup,Betameansup,Echmeansup,Xhemeansup,
                           Xanmeansup, Canxanmeansup, Canxan1meansup, Canxan2meansup, 
                           Canxan3meansup,Cisketmeansup,Ket2meansup,Lutmeansup,
                           Canmeansup, Canestmeansup, boxplotlegendmeansup, 
                        ncol=5, nrow=3, legend = "none")
figuremeansup
annotate_figure(figuremeansup,
                left = text_grob("µg carotenoid per µg skin", rot = 90))


## PCA of data ####
PCAdata <- read.csv(file.choose())

## data normalization
install.packages("corrr")
library(corrr)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)

PCA_normalized <- scale(PCAdata)
head(PCA_normalized)

## PCA
data.pca <- princomp(PCA_normalized)
summary(data.pca)
## Looks like 4 components will explain almost 90% of data 
data.pca$loadings[, 1:4]
## all positive values seem relatively equal for component 1, so all frogs have a relative amount of each
## component 2 has high apocarotenoid,canary xanthophyll, canthaxanthin, can xan ester 1, keto 1.
## component 2 negatives higher for lutein, beta carotene, keto 2
## component 3 has high pos Xanthophyll, high neg canthan ester and X3HE ester
## component 4 has high post canthan ester and can xan ester 3

## Scree plot
fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")
## Canary xan, apocar, canthaxanthin, can xan ester 1 and keto 1 all grouped, like component 2
## X3HE ester, X3 hydrox, cis. keto and echinenone grouped
## beta, lutein and keto 2 grouped 

## Contribution to four components
fviz_cos2(data.pca, choice = "var", axes = 1:4)
## they all contribute to top 4, there really isn't any variable that doesn't except maybe echinenone
## if just first 2 components, steadily fall over variables but can argue can xan ester 2, keto 2, apo, x3 hydrox,
## lutein, can xan ester 1, beta carotene. Can make argument for additional keto 3, cis. keto, can xan, keto 1

## Combine graph and contribution
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

## High are apo, can xan ester 2, ket 2, can xan ester 1, lutein, beta, x3 hydr, 
## Mid high are can xan, keto 1, cis. keto and keto 3
## Mid are canthaxanthin, can xan est 3, xan and x3he ester
## Low is echinenone and canthax ester

## MANOVA work with 4 combined values ####

## 1.Xanthophyll, canary xanthophyll, canary xanthophyll ester 1, canary xanthophyll ester 2, canary xanthophyll ester 3 and lutein ester correlated, so summing (though lutein and canary xan not correlated)
## 2.Canthaxanthin, canthaxanthin ester, cis. ketocarotenoid, ketocarotenoid ester 1, ketocarotenoid ester 2 and ketocarotenoid ester 3 correlated, so summing
## 3.Echinenone, x3hE hydroxy echinenone and e3hE ester correlated, summing
## 4.Apocarotenoid and beta carotene
## Kevin says set 2 and 3 could be combined because all ketocarotenoids but echinenone and keto1 not correlated, so leaving like this for now

Carotenoidscombined <- read.csv(file.choose())
attach(Carotenoidscombined)
Carotenoidsoriginalcombo <- Carotenoidscombined[,c(5,6,7,8)]
Carotenoidssqrtcombo <- Carotenoidscombined[,c(9,10,11,12)]

## Independent observations: ICC
ICC(Carotenoidsoriginalcombo) ## Change columns to have all dependent variables
## Look at absolute correlation values
## 0.30, higher than before combine but not 
ICC(Carotenoidssqrtcombo)
## 0.07, fine

dependentcarotenoidscombo <- data.frame(Combo.1, Combo.2, Combo.3, Combo.4)
dependentcarotenoidssqrtcombo <- data.frame(Combo.1.Sqrt, Combo.2.Sqrt, Combo.3.Sqrt, Combo.4.Sqrt)
transpose_dependentcarotenoidscombo <- t(dependentcarotenoidscombo)
transpose_dependentcarotenoidssqrtcombo <- t(dependentcarotenoidssqrtcombo)

## Run Shapiro
mshapiro.test(transpose_dependentcarotenoidscombo)
## data JUST not normal according to this test
mshapiro.test(transpose_dependentcarotenoidssqrtcombo)
## data still JUST not normal

## checking qqplots, which are more important
ggqqplot(Carotenoidscombined, "Combo.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a couple control frogs seem to have an issue
ggqqplot(Carotenoidscombined, "Combo.1", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.2", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.2", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.3", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.3", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.4", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a couple infected frogs seem to have an issue
ggqqplot(Carotenoidscombined, "Combo.1", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.1.Sqrt", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.1.Sqrt", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.2.Sqrt", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.2.Sqrt", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.3.Sqrt", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.3.Sqrt", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.4.Sqrt", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined, "Combo.4.Sqrt", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine

## Sqrt helps the qq plots

## Assumptions: Variance/Covariance Positive Determinant: needs to be positive
Covdependentcarotenoidscombo <- cov(dependentcarotenoidscombo)
det(Covdependentcarotenoidscombo)
## very positive, looks good
Covdependentcarotenoidssqrtcombo <- cov(dependentcarotenoidssqrtcombo)
det(Covdependentcarotenoidssqrtcombo)
## very positive, good

## Assumptions: Equality of Variance Between Groups Control and Infected and Sex
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY!!!!!!!!!
Type <- c("Infected","Infected","Infected", "Control",
           "Control", "Infected","Infected","Infected","Infected","Infected",
           "Control", "Infected","Infected","Infected","Infected","Infected",
           "Infected","Infected","Infected","Infected","Infected",
           "Control","Infected","Infected","Infected",
           "Control", "Control", "Control", "Infected", "Control", "Infected",
          "Control", "Infected")
Sex <- c("F","F","F","F","F","F","F","F","F","F","F","M","M","M","M","M","M","M",
         "M","M","M","M","M","M","M","M","M","M","M","F","F","F","M")
factor(Type)
Type
factor(Sex)
Sex
## Run BoxM
boxM(dependentcarotenoidscombo, Type)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices
boxM(dependentcarotenoidscombo, Sex)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices
boxM(dependentcarotenoidssqrtcombo, Type)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices
boxM(dependentcarotenoidssqrtcombo, Sex)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices


## Outliers
## Subset by group

mahalanobis_distance(data = dependentcarotenoidscombo)$is.outlier
## No outliers in the data

mahalanobis_distance(data = dependentcarotenoidssqrtcombo)$is.outlier
## No outliers in sqrt data

## Assumptions: multicollinearity
cor.matoriginalcombo <- Carotenoidsoriginalcombo %>% cor_mat()
cor.matoriginalcombo

cor.matoriginal2combo <- Carotenoidssqrtcombo %>% cor_mat()
cor.matoriginal2combo

## p-values
cororiginalcombop <- rcorr(as.matrix(Carotenoidsoriginalcombo))
cororiginalcombop
cororiginalcombop$p

## MANOVA work with 3 combined values ####
## 1.Xanthophyll, canary xanthophyll, canary xanthophyll ester 1, canary xanthophyll ester 2, canary xanthophyll ester 3 and lutein ester correlated, so summing (though lutein and canary xan not correlated)
## 2.Canthaxanthin, canthaxanthin ester, cis. ketocarotenoid, ketocarotenoid ester 1, ketocarotenoid ester 2 and ketocarotenoid ester 3 correlated, so summing
## Echinenone, x3hE hydroxy echinenone and e3hE ester correlated
## 3.Apocarotenoid and beta carotene

Carotenoidscombined3 <- read.csv(file.choose())
attach(Carotenoidscombined3)
Carotenoidsoriginalcombo3 <- Carotenoidscombined3[,c(5,6,7)]
Carotenoidssqrtcombo3 <- Carotenoidscombined3[,c(8,9,10)]

## Independent observations: ICC
ICC(Carotenoidsoriginalcombo3) ## Change columns to have all dependent variables
## Look at absolute correlation values
## 0.74, not great
ICC(Carotenoidssqrtcombo3)
## -0.237, not bad

dependentcarotenoidscombo3 <- data.frame(Combo.1, Combo.2, Combo.3)
dependentcarotenoidssqrtcombo3 <- data.frame(Combo.1.Sqrt, Combo.2.Sqrt, Combo.3.Sqrt)
transpose_dependentcarotenoidscombo3 <- t(dependentcarotenoidscombo3)
transpose_dependentcarotenoidssqrtcombo3 <- t(dependentcarotenoidssqrtcombo3)

## Run Shapiro
mshapiro.test(transpose_dependentcarotenoidscombo3)
## data JUST normal according to this test
mshapiro.test(transpose_dependentcarotenoidssqrtcombo3)
## data still JUST not normal

## checking qqplots, which are more important
ggqqplot(Carotenoidscombined3, "Combo.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a couple control frogs seem to have an issue
ggqqplot(Carotenoidscombined3, "Combo.1", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.2", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.2", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.3", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a couple infected off
ggqqplot(Carotenoidscombined3, "Combo.3", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.1", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.1.Sqrt", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.1.Sqrt", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.2.Sqrt", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.2.Sqrt", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.3.Sqrt", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine
ggqqplot(Carotenoidscombined3, "Combo.3.Sqrt", facet.by = "Sex",
         ylab = "Frog.Type", ggtheme = theme_bw())
## fine


## Sqrt helps the qq plots

## Assumptions: Variance/Covariance Positive Determinant: needs to be positive
Covdependentcarotenoidscombo3 <- cov(dependentcarotenoidscombo3)
det(Covdependentcarotenoidscombo3)
## very positive, looks good
Covdependentcarotenoidssqrtcombo3 <- cov(dependentcarotenoidssqrtcombo3)
det(Covdependentcarotenoidssqrtcombo3)
## very positive, good

## Assumptions: Equality of Variance Between Groups Control and Infected and Sex
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY!!!!!!!!!
Type <- c("Infected","Infected","Infected", "Control",
          "Control", "Infected","Infected","Infected","Infected","Infected",
          "Control", "Infected","Infected","Infected","Infected","Infected",
          "Infected","Infected","Infected","Infected","Infected",
          "Control","Infected","Infected","Infected",
          "Control", "Control", "Control", "Infected", "Control", "Infected",
          "Control", "Infected")
Sex <- c("F","F","F","F","F","F","F","F","F","F","F","M","M","M","M","M","M","M",
         "M","M","M","M","M","M","M","M","M","M","M","F","F","F","M")
factor(Type)
Type
factor(Sex)
Sex
## Run BoxM
boxM(dependentcarotenoidscombo3, Type)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices
boxM(dependentcarotenoidscombo3, Sex)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices
boxM(dependentcarotenoidssqrtcombo3, Type)
## p value is significant
boxM(dependentcarotenoidssqrtcombo3, Sex)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices

## Outliers
## Subset by group

mahalanobis_distance(data = dependentcarotenoidscombo3)$is.outlier
## No outliers in the data

mahalanobis_distance(data = dependentcarotenoidssqrtcombo3)$is.outlier
## No outliers in sqrt data

## Assumptions: multicollinearity
cor.matoriginalcombo3 <- Carotenoidsoriginalcombo3 %>% cor_mat()
cor.matoriginalcombo3

cor.matoriginal2combo3 <- Carotenoidssqrtcombo3 %>% cor_mat()
cor.matoriginal2combo3

## p-values
cororiginalcombop3 <- rcorr(as.matrix(Carotenoidsoriginalcombo3))
cororiginalcombop3
cororiginalcombop$p

## still significant according to p-values but I've gotten them down and opinions say it doesn't 
## mean I can't use the MANOVA, and it's below 0.9 for all combos

MVNMardiafull <- mvn(data = Carotenoidsoriginalcombo3, mvnTest = "mardia", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNMardiafull$multivariateNormality
## definitely nonormal
MVNMardiafullsqrt <- mvn(data = Carotenoidssqrtcombo3, mvnTest = "mardia",multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNMardiafullsqrt$multivariateNormality
## definitely nonormal
MVNHzfull <- mvn(data = Carotenoidsoriginalcombo3, mvnTest = "hz", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNHzfull$multivariateNormality
## definitely nonormal
MVNHzfullsqrt <- mvn(data = Carotenoidssqrtcombo3, mvnTest = "hz", multivariatePlot = "qq")
MVNHzfullsqrt$multivariateNormality
## P is 0.02 so not normal
MVNroystonfull <- mvn(data = Carotenoidsoriginalcombo3, mvnTest = "royston", multivariatePlot = "qq")
MVNroystonfull$multivariateNormality
## definitely nonormal
MVNroystonfullsqrt <- mvn(data = Carotenoidssqrtcombo3, mvnTest = "royston", multivariatePlot = "qq")
MVNroystonfullsqrt$multivariateNormality
## definitely nonormal
MVNDhfull <- mvn(data = Carotenoidsoriginalcombo3, mvnTest = "dh", multivariatePlot = "qq")
MVNDhfull$multivariateNormality
## definitely nonormal
MVNDhfullsqrt <- mvn(data = Carotenoidssqrtcombo3, mvnTest = "dh", multivariatePlot = "qq")
MVNDhfullsqrt$multivariateNormality
## definitely nonormal
MVNenergyfull <- mvn(data = Carotenoidsoriginalcombo3, mvnTest = "energy", multivariatePlot = "qq")
MVNenergyfull$multivariateNormality
## definitely nonormal
MVNenergyfullsqrt <- mvn(data = Carotenoidssqrtcombo3, mvnTest = "energy", multivariatePlot = "qq")
MVNenergyfullsqrt$multivariateNormality
## definitely nonormal

## MANOVA with combined esters
Carotenoidscombinedester <- read.csv(file.choose())
attach(Carotenoidscombinedester)
Carotenoidsoriginalcomboester <- Carotenoidscombinedester[,c(5,7,9,11,13,15,17)]
Carotenoidssqrtcomboester <- Carotenoidscombinedester[,c(6,8,10,12,14,16,18)]

## Assumptions: multicollinearity
cor.matoriginalcomboester <- Carotenoidsoriginalcomboester %>% cor_mat()
cor.matoriginalcomboester

cor.matoriginal2combo3 <- Carotenoidssqrtcombo3 %>% cor_mat()
cor.matoriginal2combo3

## p-values
cororiginalcombop3 <- rcorr(as.matrix(Carotenoidsoriginalcombo3))
cororiginalcombop3
cororiginalcombop$p

## I want to keep beta carotene, so keeping that. I want to keep xanthophylls, so need to get rid of Apo
## Removing can xan 2 because fairly correlated with all 
## Can keep Echinenone, but need to remove the esters
## Removing ketocarotenoids entirely because correlation
## Removing canthaxanthin ester
## need to remove lutein because highly correlated with beta carotene 

## MANOVA Combo 5 ####
Carotenoidscombined5 <- read.csv(file.choose())
attach(Carotenoidscombined5)
Carotenoidsoriginalcombo5 <- Carotenoidscombined5[,c(5,7,9,11)]
Carotenoidssqrtcombo5 <- Carotenoidscombined5[,c(6,8,10,12)]

## Outliers
## Removed F2 and M3 from previous tests
## Additionally removed M21, F19, M17 and M11 to make sqrt tests normal

## Assumption: Independent observations: ICC
ICC(Carotenoidsoriginalcombo5) ## Change columns to have all dependent variables
## Look at absolute correlation values
## -0.128, good
ICC(Carotenoidssqrtcombo5)
## -0.206, good

dependentcarotenoidscombo5 <- data.frame(Xanthophylls, Canthaxanthins,Echinenone, BetaCarotene)
dependentcarotenoidssqrtcombo5 <- data.frame(SqrtXanthophylls, SqrtCanthaxanthins,SqrtEchinenone, SqrtBetaCarotene)
transpose_dependentcarotenoidscombo5 <- t(dependentcarotenoidscombo5)
transpose_dependentcarotenoidssqrtcombo5 <- t(dependentcarotenoidssqrtcombo5)

## Assumption: Univariate normality
## Run Shapiro
mshapiro.test(transpose_dependentcarotenoidscombo5)
## data JUST not normal
mshapiro.test(transpose_dependentcarotenoidssqrtcombo5)
## data normal

## Assumptions: Variance/Covariance Positive Determinant: needs to be positive
Covdependentcarotenoidscombo5 <- cov(dependentcarotenoidscombo5)
det(Covdependentcarotenoidscombo5)
## very positive, looks good
Covdependentcarotenoidssqrtcombo5 <- cov(dependentcarotenoidssqrtcombo5)
det(Covdependentcarotenoidssqrtcombo5)
## positive, good

## Outliers additional test

mahalanobis_distance(data = dependentcarotenoidscombo5)$is.outlier
## No outliers in the data

mahalanobis_distance(data = dependentcarotenoidssqrtcombo5)$is.outlier
## No outliers in sqrt data

## Assumptions: Equality of Variance Between Groups Control and Infected and Sex
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY!!!!!!!!!
Type <- c("Infected","Infected","Infected", "Control",
          "Control", "Infected","Infected","Infected","Infected","Infected",
          "Control", "Infected","Infected","Infected","Infected","Infected",
          "Infected","Infected","Infected",
          "Control","Infected","Infected","Infected",
          "Control", "Control", "Control", "Infected", "Control", "Infected")
          
Sex <- c("F","F","F","F","F","F","F","F","F","F","F","M","M","M","M","M",
         "M","M","M","M","M","M","M","M","M","M","M","F","F")
factor(Type)
Type
factor(Sex)
Sex
## Run BoxM
boxM(dependentcarotenoidscombo5, Type)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices
boxM(dependentcarotenoidscombo5, Sex)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices
boxM(dependentcarotenoidssqrtcombo5, Type)
## p value is significant
boxM(dependentcarotenoidssqrtcombo5, Sex)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices

## Assumptions: multicollinearity
cor.matoriginalcombo5 <- Carotenoidsoriginalcombo5 %>% cor_mat()
cor.matoriginalcombo5

cor.matoriginal2combo5 <- Carotenoidssqrtcombo5 %>% cor_mat()
cor.matoriginal2combo5
## correlation matrices  good without sqrt, with it they aren't as good but still within limits
## I am comfortable with so keeping as is

## Assumptions: multivariate normality
MVNHzfull <- mvn(data = Carotenoidsoriginalcombo5, mvnTest = "hz", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNHzfull$multivariateNormality
## normal
MVNHzfullsqrt <- mvn(data = Carotenoidssqrtcombo5, mvnTest = "hz", multivariatePlot = "qq")
MVNHzfullsqrt$multivariateNormality
## normalizes

## I can actually do the original data instead of square root transforming. The only thing that 
## isn't normal is the univariate analysis but it is in a place I am comfortable with 

## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY
Type = factor(Type)
## Bind dependent variables together into one vector
Xanthophylls <- Carotenoidscombined5$Xanthophylls
Canthaxanthins <- Carotenoidscombined5$Canthaxanthins
Echinenone <- Carotenoidscombined5$Echinenone
BetaCarotene <- Carotenoidscombined5$BetaCarotene

Y <- cbind(Xanthophylls,Canthaxanthins,Echinenone,BetaCarotene)
## Run MANOVA
MANOVA = manova(Y~Type)
## Run Tests on MANOVA, check if p values are below 0.05
summary(MANOVA, test = "Wilks")
summary(MANOVA, test = "Pillai")
summary(MANOVA, test = "Hotelling-Lawley")
summary(MANOVA, test = "Roy")

## nothing significant here between groups 

linearHypothesis(MANOVA, "grade.L") |> print(SSP = FALSE)

## Wilks Lambda partial eta squares values
## Take smaller of two values, number of outcome variables (here, 16) or df of group variable = b
## formula: 1-(0.63202)^(1/b)
## Answer will be percentage of variance between outcome variables is due to difference in group variable

## Post hoc tests
## Effect size
eta_squared(MANOVA) ## value is 0.08, not high enough to mean anything really which explains the non-significant p-value

MANOVA_lda <- lda(groupnew ~ Y, CV = F)
MANOVA_lda

MANOVA_df <- data.frame(
  species = Carotenoids[, "Frog.Type"],
  lda = predict(MANOVA_lda)$x
)
MANOVA_df


ggplot(MANOVA_df) +
  geom_point(aes(x = LD1, y = species), size = 4) +
  theme_classic()

## Factorial MANOVA 


FACTORIALSETUP <- lm(Y~Frog.Type*Sex, data = Carotenoidscombined5)
summary(FACTORIALSETUP)
Anova(FACTORIALSETUP, test.statistic="Roy")
Anova(update(FACTORIALSETUP, Xanthophylls ~ .))
Anova(update(FACTORIALSETUP, Canthaxanthins ~ .))
Anova(update(FACTORIALSETUP, Echinenone ~ .))
Anova(update(FACTORIALSETUP, BetaCarotene ~ .))

## interaction not significant so using type II
FACTORIALMANOVAwilks <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Wilks"))
FACTORIALMANOVApillai <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Pillai"))
FACTORIALMANOVAhotelling <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Hotelling-Lawley"))
FACTORIALMANOVAroy <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("III"), test=("Roy"))
## If interactions aren't significant, can use Type II
FACTORIALMANOVAwilks <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Wilks"))
FACTORIALMANOVAwilks
FACTORIALMANOVApillai <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Pillai"))
FACTORIALMANOVApillai
FACTORIALMANOVAhotelling <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Hotelling-Lawley"))
FACTORIALMANOVAhotelling
FACTORIALMANOVAroy <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Roy"))
FACTORIALMANOVAroy
## Frog type significant for all of these, but sex isn't and given the 

## Effect size of MANOVA

etasq(FACTORIALSETUP,test="Wilks") # Using Wilks to be consistent with above
## Not that significant, but sex explains more than frog type

## No significane in factorial manova

## Doing a simple students t-test for total carotenoids because can't include in MANOVA since clearly correlated
