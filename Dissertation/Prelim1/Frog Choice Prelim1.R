## Preliminary Trial 1
## Remember for this that groups are not independent, so will never 
## meet assumptions for ttest and ANOVA, need to use nonparametric tests instead
## One-way ANOVA
prelim1 <- read.csv(file.choose())
## Boxplot for normality
boxplot(prelim1$Location_A_Proportion, prelim1$Location_B_Proportion, prelim1$Location_C_Proportion)
## Shapiro-Wilk test for normality
shapiro.test(prelim1$Location_A_Proportion)
shapiro.test(prelim1$Location_B_Proportion)
shapiro.test((prelim1$Location_C_Proportion))
## location A and B are normal, location C is not normal
## Boxplot for square-root transformed
sqrtA <- sqrt(prelim1$Location_A_Proportion)
sqrtB <- sqrt(prelim1$Location_B_Proportion)
sqrtC <- sqrt(prelim1$Location_C_Proportion)
boxplot(sqrtA, sqrtB, sqrtC)
shapiro.test(sqrtA)
shapiro.test(sqrtB)
shapiro.test(sqrtC)
## locations A and C are not normal, not a good transformation
## remove MV3 and try again 
prelim1noMV3 <- prelim1[-c(5),]
## Boxplot for normality 
boxplot(prelim1noMV3$Location_A_Proportion, prelim1noMV3$Location_B_Proportion, prelim1noMV3$Location_C_Proportion)
## Shapiro-Wilk test for normality
shapiro.test(prelim1noMV3$Location_A_Proportion)
shapiro.test(prelim1noMV3$Location_B_Proportion)
shapiro.test((prelim1noMV3$Location_C_Proportion))
## load data in form for test of variances and ANOVA
prelim1_ANOVA <- read.csv(file.choose())
## test of equal variance
library(car)
bartlett.test(Weight ~ Group, data = prelim1_ANOVA)
## pvalue is greater than 0.05, so have homogeneity of variances
## ANOVA and Kruskal
prelim1aov <- aov(Weight ~ Group, data = prelim1_ANOVA)
summary(prelim1aov)
library(dplyr)
kruskal.test(Weight~Group, data = prelim1_ANOVA)
## Tukey test and pairwise wilcox
TukeyHSD(prelim1aov)
pairwise.wilcox.test(prelim1_ANOVA$Weight, prelim1_ANOVA$Group,
                     p.adjust.method = "BH")
## Only sig diff is between A and B
## Pairwise comparisons (anova and t-test)
prelim1pwc <- compare_means(Weight~Group, data = prelim1_ANOVA)
prelim1pwc
prelim1comparisons <- list(c("locaA", "locaB"), c("locaA", "locaC"), c("locaB", "locaC"))
## Graph
library(ggpubr)
ggboxplot(prelim1_ANOVA, x = "Group", y = "Weight", col = c("darkblue", "cyan3", "mediumorchid3"), xlab = "Location", ylab = "Proportion of Time")+
  stat_compare_means(comparisons = prelim1comparisons)+
  stat_compare_means(method = "anova", label.y = 1.0)
ggboxplot(prelim1_ANOVA, x = "Group", y = "Weight", col = c("darkblue", "cyan3", "mediumorchid3"), xlab = "Location", ylab = "Proportion of Time")+
  stat_compare_means(comparisons = prelim1comparisons)+
  stat_compare_means(method = "kruskal.test", label.y = 1.0)
## T-test for corners
## Boxplot for normality
boxplot(prelim1$Yes_proportion, prelim1$No_proportion)
## Shapiro-Wilk test for normality with paired data
shapiro.test(prelim1$Yes_proportion)
shapiro.test(prelim1$No_proportion)
## Both normally distributed 
## load data in form for test of variances and Ttest
prelim1_Ttest <- read.csv(file.choose())
## test of equal variance
library(car)
bartlett.test(Weight ~ Group, data = prelim1_Ttest)
var.test(Weight~Group, data = prelim1_Ttest, alternative = "two.sided")
##Not sure if test of equal variance for paired ttest is needed but ran in case
## Paired wilcox test because nonparametric
prelim1no <- prelim1$No_proportion
prelim1yes <- prelim1$Yes_proportion
prelim1wilcox <- wilcox.test(prelim1no, prelim1yes, paired = TRUE)
prelim1wilcox
## Graph
prelim1pwcttest <- compare_means(Weight~Group, data = prelim1_Ttest)
prelim1pwcttest
prelim1ttestcomparisons <- list(c("Yescorners", "Nocorners"))
ggboxplot(prelim1_Ttest, x = "Group", y = "Weight", col = c("darkorange", "darkred"), xlab = "Not in Corner vs in Corner", ylab = "Proportion of Time")+
  stat_compare_means(comparisons = prelim1ttestcomparisons)+
  stat_compare_means(method = "wilcox.test", label.y = 1.1)
