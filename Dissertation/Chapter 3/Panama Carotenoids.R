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
## Load dataset, attach ####
Carotenoids <- read.csv(file.choose())
attach(Carotenoids)

## One-way MANOVA for Carotenoids: Assumptions, outliers ####
## Independent observations: ICC
ICC(Carotenoids[,3:19]) ## Change columns to have all dependent variables
## Look at absolute correlation values
## Value is 0.086, very close to zero and so correlation not an issue
## Normality: Shapiro-Wilks
## Separate out dependent variables into one dataframe and transpose
dependentcarotenoids <- data.frame(Carotenoids$Apocarotenoid, Carotenoids$Canary.xanthophyll, Carotenoids$Canthaxanthin,
                                   Carotenoids$Xanthophyll, Carotenoids$Echinenone, Carotenoids$cis.ketocarotenoid,
                                   Carotenoids$X3.hydroxy.echinenone, Carotenoids$lutein.ester.1, Carotenoids$canary.xanthophyll.ester.1,
                                   Carotenoids$beta.carotene, Carotenoids$canary.xanthophyll.ester.2, Carotenoids$ketocarotenoid.ester.1,
                                   Carotenoids$ketocarotenoid.ester.2, Carotenoids$canary.xanthophyll.ester.3, Carotenoids$canthaxanthin.ester,
                                   Carotenoids$X3HE.ester, Carotenoids$ketocarotenoid.ester.3)
transpose_dependentcarotenoids <- t(dependentcarotenoids)
## Run Shapiro
mshapiro.test(transpose_dependentcarotenoids)
## data are not normal
ggqqplot(Carotenoids, "Apocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "Canary.xanthophyll", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "Canthaxanthin", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## ## a bit of a tail, not bad
ggqqplot(Carotenoids, "Xanthophyll", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## ## a bit of a tail, not bad
ggqqplot(Carotenoids, "Echinenone", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## ## a bit of a tail, not bad
ggqqplot(Carotenoids, "lutein.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a bit of a tail, not bad
ggqqplot(Carotenoids, "cis.ketocarotenoid", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## ## a bit of a tail, not bad
ggqqplot(Carotenoids, "X3.hydroxy.echinenone", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "canary.xanthophyll.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## ## a bit of a tail, not bad
ggqqplot(Carotenoids, "beta.carotene", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## ## a bit of a tail, not bad
ggqqplot(Carotenoids, "canary.xanthophyll.ester.2", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## ## a bit of a tail, not bad
ggqqplot(Carotenoids, "ketocarotenoid.ester.1", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## a couple off points but mainly fine
ggqqplot(Carotenoids, "ketocarotenoid.ester.2", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "canary.xanthophyll.ester.3", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "canthaxanthin.ester", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "X3HE.ester", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good
ggqqplot(Carotenoids, "ketocarotenoid.ester.3", facet.by = "Frog.Type",
         ylab = "Frog.Type", ggtheme = theme_bw())
## good

## though shapiro test positive the qqplots look fine so going to move forward with that

## Variance/Covariance Positive Determinant: needs to be positive
Covdependentcarotenoids <- cov(dependentcarotenoids)
det(Covdependentcarotenoids)
## very positive, looks good

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
## Subset by group

mahalanobis_distance(data = dependentcarotenoids)$is.outlier
## No outliers in the data

## Linearity and correlation
library(GGally)
results1 <- Carotenoids %>%
  select(Apocarotenoid, Canary.xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results1
results1$plots
## correlated, above 0.9

results2 <- Carotenoids %>%
  select(Apocarotenoid, Canthaxanthin, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results2
results2$plots

results3 <- Carotenoids %>%
  select(Apocarotenoid, Xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results3
results3$plots

results4 <- Carotenoids %>%
  select(Apocarotenoid, Echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results4
results4$plots

results5 <- Carotenoids %>%
  select(Apocarotenoid, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results5
results5$plots

results6 <- Carotenoids %>%
  select(Apocarotenoid, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results6$plots

results7 <- Carotenoids %>%
  select(Apocarotenoid, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results7
results7$plots

results8 <- Carotenoids %>%
  select(Apocarotenoid, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results8$plots

results9 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results9$plots
## correlated

results10 <- Carotenoids %>%
  select(Apocarotenoid, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results10
results10$plots

results11 <- Carotenoids %>%
  select(Apocarotenoid, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results11
results11$plots

results12 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results12$plots

results13 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results13$plots

results14 <- Carotenoids %>%
  select(Apocarotenoid, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results14$plots

results15 <- Carotenoids %>%
  select(Apocarotenoid, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results15

results16 <- Carotenoids %>%
  select(Apocarotenoid, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results16

##+15

results17 <- Carotenoids %>%
  select(Canary.xanthophyll, Canthaxanthin, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results17

results18 <- Carotenoids %>%
  select(Canary.xanthophyll, Xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results18

results19 <- Carotenoids %>%
  select(Canary.xanthophyll, Echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results19

results20 <- Carotenoids %>%
  select(Canary.xanthophyll, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results20

results21 <- Carotenoids %>%
  select(Canary.xanthophyll, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results21

results22 <- Carotenoids %>%
  select(Canary.xanthophyll, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results22

results23 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results23

results24 <- Carotenoids %>%
  select(Canary.xanthophyll, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results24

results25 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results25

results26 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results26

results27 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results27

results28 <- Carotenoids %>%
  select(Canary.xanthophyll, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results28

results29 <- Carotenoids %>%
  select(Canary.xanthophyll, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results29

results30 <- Carotenoids %>%
  select(Canary.xanthophyll, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results30

results31 <- Carotenoids %>%
  select(Canary.xanthophyll, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results31

results32 <- Carotenoids %>%
  select(Canthaxanthin, Xanthophyll, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results32

results33 <- Carotenoids %>%
  select(Canthaxanthin, Echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results33

results34 <- Carotenoids %>%
  select(Canthaxanthin, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results34

results35 <- Carotenoids %>%
  select(Canthaxanthin, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results35

results36 <- Carotenoids %>%
  select(Canthaxanthin, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results36

results37 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results37

results38 <- Carotenoids %>%
  select(Canthaxanthin, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results38

results39 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results39

results40 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results40

results41 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results41

results42 <- Carotenoids %>%
  select(Canthaxanthin, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results42

results43 <- Carotenoids %>%
  select(Canthaxanthin, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results43

results44 <- Carotenoids %>%
  select(Canthaxanthin, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results44

results45 <- Carotenoids %>%
  select(Canthaxanthin, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results45
## +13

results46 <- Carotenoids %>%
  select(Xanthophyll, Echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results46

results47 <- Carotenoids %>%
  select(Xanthophyll, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results47

results48 <- Carotenoids %>%
  select(Xanthophyll, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results48

results49 <- Carotenoids %>%
  select(Xanthophyll, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results49

results50 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results50

results51 <- Carotenoids %>%
  select(Xanthophyll, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results51

results52 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results52

results53 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results53

results54 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results54

results55 <- Carotenoids %>%
  select(Xanthophyll, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results55

results56 <- Carotenoids %>%
  select(Xanthophyll, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results56

results57 <- Carotenoids %>%
  select(Xanthophyll, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results57

results58 <- Carotenoids %>%
  select(Xanthophyll, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results58

## +12

results59 <- Carotenoids %>%
  select(Echinenone, cis.ketocarotenoid, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results59

results60 <- Carotenoids %>%
  select(Echinenone, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results60

results61 <- Carotenoids %>%
  select(Echinenone, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results61

results62 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results62

results63 <- Carotenoids %>%
  select(Echinenone, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results63

results64 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results64

results65 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results65

results66 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results66

results67 <- Carotenoids %>%
  select(Echinenone, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results67

results68 <- Carotenoids %>%
  select(Echinenone, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results68

results69 <- Carotenoids %>%
  select(Echinenone, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results69

results70 <- Carotenoids %>%
  select(Echinenone, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results70

## +11

results71 <- Carotenoids %>%
  select(cis.ketocarotenoid, X3.hydroxy.echinenone, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results71

results72 <- Carotenoids %>%
  select(cis.ketocarotenoid, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results72

results73 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results73

results74 <- Carotenoids %>%
  select(cis.ketocarotenoid, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results74

results75 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results75

results76 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results76

results77 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results77

results78 <- Carotenoids %>%
  select(cis.ketocarotenoid, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results78

results79 <- Carotenoids %>%
  select(cis.ketocarotenoid, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results79

results80 <- Carotenoids %>%
  select(cis.ketocarotenoid, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results80

results81 <- Carotenoids %>%
  select(cis.ketocarotenoid, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results81
## +10

results82 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, lutein.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results82

results83 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results83

results84 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results84

results85 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results85

results86 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results86

results87 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results87

results88 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results88

results89 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results89

results90 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results90

results91 <- Carotenoids %>%
  select(X3.hydroxy.echinenone, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results91

## +9

results92 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results92

results93 <- Carotenoids %>%
  select(lutein.ester.1, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results93

results94 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results94

results95 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results95

results96 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results96

results97 <- Carotenoids %>%
  select(lutein.ester.1, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results97

results98 <- Carotenoids %>%
  select(lutein.ester.1, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results98

results99 <- Carotenoids %>%
  select(lutein.ester.1, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results99

results100 <- Carotenoids %>%
  select(lutein.ester.1, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results100

results101 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, beta.carotene, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results101

results102 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results102

results103 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results103

results104 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results104

results105 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results105

results106 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results106

results107 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results107

results108 <- Carotenoids %>%
  select(canary.xanthophyll.ester.1, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results108

results109 <- Carotenoids %>%
  select(beta.carotene, canary.xanthophyll.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results109

results110 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results110

results111 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results111

results112 <- Carotenoids %>%
  select(beta.carotene, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results112

results113 <- Carotenoids %>%
  select(beta.carotene, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results113

results114 <- Carotenoids %>%
  select(beta.carotene, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results114

results115 <- Carotenoids %>%
  select(beta.carotene, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results115

results116 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.1, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results116

results117 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results117

results118 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results118

results119 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results119

results120 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results120

results121 <- Carotenoids %>%
  select(canary.xanthophyll.ester.2, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results121

results122 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, ketocarotenoid.ester.2, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results122

results123 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results123

results124 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results124

results125 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results125

results126 <- Carotenoids %>%
  select(ketocarotenoid.ester.1, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results126

results127 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, canary.xanthophyll.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results127

results128 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results128

results129 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results129

results130 <- Carotenoids %>%
  select(ketocarotenoid.ester.2, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results130

results131 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, canthaxanthin.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results131

results132 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results132

results133 <- Carotenoids %>%
  select(canary.xanthophyll.ester.3, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results133

results134 <- Carotenoids %>%
  select(canthaxanthin.ester, X3HE.ester, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results134

results135 <- Carotenoids %>%
  select(canthaxanthin.ester, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results135

results136 <- Carotenoids %>%
  select(X3HE.ester, ketocarotenoid.ester.3, Frog.Type) %>%
  group_by(Frog.Type) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results136

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
