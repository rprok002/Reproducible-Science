## Chapter 3 Carotenoids Clean Rcode

## NOTE: when attach dataset, the next time you have you attach a different dataset
## you have to restart Rstudio and not save the R environment, otherwise it will not 
## attach the new dataset properly

## Packages ####
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
library(ggplot2)

## Frog Removal Notes from Oophaga skin carotenoids full raw dataset ####
## Removed WM1, M5, M8, and F25 because of issues infecting and inconsistensies with rest of frogs in dataset
## Removed M17, M3, M28 and M24 because of small skin size, M11, M14, M19 because not infected at 15 days after initial exposure

## Leaves 29 frogs for initial dataset

## PCA of 29 frogs ####
## Data file
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

## PCA doesn't really seem to help with picking variables to continue exploring, so moving onto MANOVAs


## MANOVA assumptions with 4 combined carotenoids ####

## 1.Xanthophyll, canary xanthophyll, canary xanthophyll ester 1, canary xanthophyll ester 2, canary xanthophyll ester 3 and lutein ester correlated, so summing (though lutein and canary xan not correlated)
## 2.Canthaxanthin, canthaxanthin ester, cis. ketocarotenoid, ketocarotenoid ester 1, ketocarotenoid ester 2 and ketocarotenoid ester 3 correlated, so summing
## 3.Echinenone, x3hE hydroxy echinenone and e3hE ester correlated, summing
## 4.Apocarotenoid and beta carotene
## Kevin says set 2 and 3 could be combined because all ketocarotenoids but echinenone and keto1 not correlated, so leaving like this for now

Carotenoidscombined <- read.csv(file.choose())
Carotenoidsoriginalcombo <- Carotenoidscombined[,c(5,6,7,8)]

## Assumptions: multicollinearity
cor.matoriginalcombo <- Carotenoidsoriginalcombo %>% cor_mat()
cor.matoriginalcombo

## Multicollinearity too high between groups, looking for below 0.7 if possible so trying again

## MANOVA assumptions with 3 combined carotenoids ####
## 1.Xanthophyll, canary xanthophyll, canary xanthophyll ester 1, canary xanthophyll ester 2, canary xanthophyll ester 3 and lutein ester correlated, so summing (though lutein and canary xan not correlated)
## 2.Canthaxanthin, canthaxanthin ester, cis. ketocarotenoid, ketocarotenoid ester 1, ketocarotenoid ester 2, ketocarotenoid ester 3, Echinenone, x3hE hydroxy echinenone and e3hE ester correlated correlated, so summing
## 3.Apocarotenoid and beta carotene

Carotenoidscombined3 <- read.csv(file.choose())
Carotenoidsoriginalcombo3 <- Carotenoidscombined3[,c(5,6,7)]

## Assumptions: multicollinearity
cor.matoriginalcombo3 <- Carotenoidsoriginalcombo3 %>% cor_mat()
cor.matoriginalcombo3

## Multicollinearity too high between groups, looking for below 0.7 if possible so trying again

## MANOVA assumptions with combined esters ####
## Apocarotenoid: Apocarotenoid
## Xanthophylls: Xanthophyll, Can Xan, Can Xan Est 1, Can Xan Est 2, Can Xan Est 3
## Canthaxanthins: canthaxanthin and canthaxanthin ester
## Echinenones: Echinenone, X3HE ester, X3 hydroxy
## Ketocarotenoids: cis.keto, keto 1, keto 2, keto 3
## Lutein: lutein ester 1
## Beta Carotene: beta carotene

Carotenoidscombinedester <- read.csv(file.choose())
Carotenoidsoriginalcomboester <- Carotenoidscombinedester[,c(5,7,9,11,13,15,17)]

## Assumptions: multicollinearity
cor.matoriginalcomboester <- Carotenoidsoriginalcomboester %>% cor_mat()
cor.matoriginalcomboester

## Multicollinearity too high between groups, looking for below 0.7 if possible so trying again

## I want to keep beta carotene, so keeping that. I want to keep xanthophylls, so need to get rid of Apo
## Removing can xan 2 because fairly correlated with all 
## Can keep Echinenone, but need to remove the esters
## Removing ketocarotenoids entirely because correlation
## Removing canthaxanthin but keeping ester
## need to remove lutein because highly correlated with beta carotene 

## MANOVA assumptions with final carotenoid categories try first with 29 frogs ####

## Final carotenoid categories:
## Xanthophylls: Xanthophyll, Can Xan, Can Xan Est 1, Can Xan Est 3
## Echinenone: Echinenone
## Canthaxanthins: canthaxanthin ester
## Beta carotene: beta carotene

Carotenoidscombined5 <- read.csv(file.choose())
attach(Carotenoidscombined5)
Carotenoidsoriginalcombo5 <- Carotenoidscombined5[,c(5,7,9,11)]

## Assumption: Independent observations: ICC
ICC(Carotenoidsoriginalcombo5) ## Change columns to have all dependent variables
## Look at absolute correlation values
## -0.078, good

dependentcarotenoidscombo5 <- data.frame(Xanthophylls, Canthaxanthins,Echinenone, BetaCarotene)
transpose_dependentcarotenoidscombo5 <- t(dependentcarotenoidscombo5)

## Assumptions: Variance/Covariance Positive Determinant: needs to be positive
Covdependentcarotenoidscombo5 <- cov(dependentcarotenoidscombo5)
det(Covdependentcarotenoidscombo5)
## very positive, looks good

## Outliers additional test
mahalanobis_distance(data = dependentcarotenoidscombo5)$is.outlier
## No outliers in the data according to this test

## Assumptions: Equality of Variance Between Groups Control and Infected and Sex
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY!!!!!!!!!
Type <- c("Infected","Infected","Infected", "Control",
          "Control", "Infected","Infected","Infected","Infected","Infected",
          "Control", "Infected","Infected","Infected","Infected","Infected",
          "Infected", "Control","Infected", "Infected",
          "Control", "Control", "Control", "Infected", "Control", "Infected",
          "Infected", "Control", "Infected")

Sex <- c("F","F","F","F","F","F","F","F","F","F","F","M","M","M",
         "M","M","M","M","M","M","M","M","M","M","F","F","M", "F", "F")
factor(Type)
Type
factor(Sex)
Sex
## Run BoxM
boxM(dependentcarotenoidscombo5, Type)
## suggests normality and p-value is significant so no homogeneity of covariance matrices
boxM(dependentcarotenoidscombo5, Sex)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices

## Assumptions: multicollinearity
cor.matoriginalcombo5 <- Carotenoidsoriginalcombo5 %>% cor_mat()
cor.matoriginalcombo5

## correlation matrix ranges from 0.37-0.67 so that is ok

## Assumptions: multivariate normality
MVNHzfull <- mvn(data = Carotenoidsoriginalcombo5, mvnTest = "hz", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNHzfull$multivariateNormality
## not normal

## no homogeneity of covariance matrices and not normal
## To fix, additionally removed F1 and F19 for outliers to achieve multivariate normality

## MANOVA assumptions with final carotenoid categories with final 27 frogs ####

## Final carotenoid categories:
## Xanthophylls: Xanthophyll, Can Xan, Can Xan Est 1, Can Xan Est 3
## Echinenone: Echinenone
## Canthaxanthins: canthaxanthin ester
## Beta carotene: beta carotene

Carotenoidscombined5final <- read.csv(file.choose())
attach(Carotenoidscombined5final)
Carotenoidsoriginalcombined5final <- Carotenoidscombined5final[,c(5,7,9,11)]

## Assumption: Independent observations: ICC
ICC(Carotenoidsoriginalcombined5final) ## Change columns to have all dependent variables
## Look at absolute correlation values
## -0.117, good

dependentcarotenoidscombo5final <- data.frame(Xanthophylls, Canthaxanthins,Echinenone, BetaCarotene)
transpose_dependentcarotenoidscombo5final <- t(dependentcarotenoidscombo5final)

## Assumptions: Variance/Covariance Positive Determinant: needs to be positive
Covdependentcarotenoidscombo5final <- cov(dependentcarotenoidscombo5final)
det(Covdependentcarotenoidscombo5final)
## very positive, looks good

## Outliers additional test
mahalanobis_distance(data = dependentcarotenoidscombo5final)$is.outlier
## No outliers in the data according to this test

## Assumptions: Equality of Variance Between Groups Control and Infected and Sex
## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY!!!!!!!!!
Type <- c("Infected","Infected","Infected", "Control",
          "Control", "Infected","Infected","Infected","Infected","Infected",
          "Control", "Infected","Infected","Infected","Infected","Infected",
          "Infected", "Control","Infected", "Infected",
          "Control", "Control", "Control", "Infected", "Control", "Infected",
          "Infected")

Sex <- c("F","F","F","F","F","F","F","F","F","F","F","M","M","M",
         "M","M","M","M","M","M","M","M","M","M","F","F","M")
factor(Type)
Type
factor(Sex)
Sex
## Run BoxM
boxM(dependentcarotenoidscombo5final, Type)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices
boxM(dependentcarotenoidscombo5final, Sex)
## suggests normality and p-value is nonsignificant so homogeneity of covariance matrices

## Assumptions: multicollinearity
cor.matoriginalcombo5final <- Carotenoidsoriginalcombined5final %>% cor_mat()
cor.matoriginalcombo5final

## correlation matrix ranges from 0.51-0.73 so that is ok

## Assumptions: multivariate normality
MVNHzfull <- mvn(data = Carotenoidsoriginalcombined5final, mvnTest = "hz", multivariatePlot = "qq", multivariateOutlierMethod = "quan")
MVNHzfull$multivariateNormality
## normal

## met assumptions for making a MANOVA

## Factorial MANOVA with final dataset of 27 frogs ####

## MAKE SURE GROUP MATCHES WITH DATASET CORRECTLY

## Bind dependent variables together into one vector
Xanthophylls <- Carotenoidscombined5final$Xanthophylls
Canthaxanthins <- Carotenoidscombined5final$Canthaxanthins
Echinenone <- Carotenoidscombined5final$Echinenone
BetaCarotene <- Carotenoidscombined5final$BetaCarotene

Y <- cbind(Xanthophylls,Canthaxanthins,Echinenone,BetaCarotene)
## Run with interaction first
FACTORIALSETUP <- lm(Y~Frog.Type*Sex, data = Carotenoidscombined5final)
summary(FACTORIALSETUP)
Anova(FACTORIALSETUP, test.statistic="Roy")
## no interaction, so taking out and making sure
## using Type II test because no interaction 

FACTORIALSETUP <- lm(Y~Frog.Type+Sex, data = Carotenoidscombined5final)
summary(FACTORIALSETUP)
Anova(FACTORIALSETUP, test.statistic="Roy")

FACTORIALMANOVAroy <-Manova(FACTORIALSETUP, multivariate = TRUE, type = c("II"), test=("Roy"))
FACTORIALMANOVAroy
## Neither frog type nor sex significant

## Breakdown by carotenoid category
Anova(update(FACTORIALSETUP, Xanthophylls ~ .))
Anova(update(FACTORIALSETUP, Canthaxanthins ~ .))
Anova(update(FACTORIALSETUP, Echinenone ~ .))
Anova(update(FACTORIALSETUP, BetaCarotene ~ .))

## Effect size of MANOVA

etasq(FACTORIALSETUP,test="Wilks") # Using Wilks to be consistent with above
## sex explains more than frog type but not high

## Two-way ANOVA test for sum of all 17 carotenoid concentrations for 27 frogs ####

## Using SqrtSum of carotenoid concentrations to negate effect of outliers

# Two-way ANOVA with interaction
# save model
mod <- aov(SumSqrt ~ Frog.Type * Sex,
           data = Carotenoidscombined5final
)
mod
# print results
summary(mod)
anova(mod)
## no interaction, so removing

mod <- aov(SumSqrt ~ Frog.Type + Sex,
           data = Carotenoidscombined5final
)
mod
# print results
summary(mod)
anova(mod)

plot(mod, which = 2)
## residuals have a couple off but not bad

hist(mod$residuals)
## residuals seem a bit skewed

# normality test
shapiro.test(mod$residuals)
## fits normality, so good

## Homogeneity of variance
plot(mod, which = 3)
## looks pretty good

# boxplots by sex
ggplot(Carotenoidscombined5final) +
  aes(x = Frog.Type, y = SumSqrt) +
  geom_boxplot()
## no outliers here 

# boxplots by frog type
ggplot(Carotenoidscombined5final) +
  aes(x = Sex, y = SumSqrt) +
  geom_boxplot()
## might have a couple outliers in females, but not extreme enough to worry

## MANOVA and two way anova diagrams ####
colors = c("red", "darkblue", "darkgreen", "brown")
heplot(FACTORIALSETUP, size="evidence", 
       col=colors, cex=1.25,
       fill=TRUE, fill.alpha=0.1)
heplot(FACTORIALSETUP, size="effect", 
       add=TRUE, lwd=5, term.labels=FALSE, col=colors)

pairs(FACTORIALSETUP)

## sum two way anova
ggplot(Carotenoidscombined5final, aes(x=Frog.Type, y=SumSqrt, fill=Sex)) + 
  geom_boxplot()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values=c("white", "darkgrey")) +
  theme(axis.title.x=element_blank(),axis.text.x=element_text(size=rel(1.2)),
        legend.position = "top")+
  ylim(0,30)+
  ylab("Sum of SQRT(µg carotenoids per g skin)")+
  annotate("text", x=0.81, y=20.49, label= "x", fontface = "bold", size = 4)+
  annotate("text", x=1.19, y=19.97, label= "x", fontface = "bold", size = 4)+
  annotate("text", x=1.81, y=17.78, label= "x", fontface = "bold", size = 4)+
  annotate("text", x=2.19, y=19.44, label= "x", fontface = "bold", size = 4)

