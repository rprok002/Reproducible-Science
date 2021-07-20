## Preliminary Trial 2
## Preliminary Trial 1
## Remember for this that groups are not independent, so will never 
## meet assumptions for ttest and ANOVA, need to use nonparametric tests instead
## One-way ANOVA
prelim2 <- read.csv(file.choose())
# Boxplot for normality
boxplot(prelim2$Location_A_Proportion, prelim2$Location_B_Proportion, prelim2$Location_C_Proportion)
## Shapiro-Wilk test for normality
shapiro.test(prelim2$Location_A_Proportion)
shapiro.test(prelim2$Location_B_Proportion)
shapiro.test(prelim2$Location_C_Proportion)
## All normal according to Shapiro-Wilks
## load data in form for test of variances and ANOVA
prelim2_ANOVA <- read.csv(file.choose())
## test of equal variance
library(car)
bartlett.test(Weight ~ Group, data = prelim2_ANOVA)
## Equal variance according to Bartlett test
## pvalue is greater than 0.05, so have homogeneity of variances
## ANOVA and Kruskal
prelim2aov <- aov(Weight ~ Group, data = prelim2_ANOVA)
summary(prelim2aov)
library(dplyr)
kruskal.test(Weight~Group, data = prelim2_ANOVA)
## Tukey test and pairwise wilcox
TukeyHSD(prelim2aov)
pairwise.wilcox.test(prelim2_ANOVA$Weight, prelim2_ANOVA$Group,
                     p.adjust.method = "BH")
## Sig difference in time between treatment areas (A and C) and null area (B)
## Spent sig less time in null area than treatment areas, good for comparing treatments
## and verifying that null area isn't really preferred