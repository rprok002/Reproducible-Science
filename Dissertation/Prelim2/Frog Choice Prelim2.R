## Preliminary Trial 2
## Remember for this that groups are not independent, so will never 
## meet assumptions for ttest and ANOVA, need to use nonparametric tests instead
## One-way ANOVA
prelim2 <- read.csv(file.choose())
# Boxplot for normality
boxplot(prelim2$Location_A_Proportion, prelim2$Location_B_Proportion, prelim2$Location_C_Proportion,  
        main = "Preliminary 2 Locations", xlab = "Location (A,B,C)", ylab = "Proportion of Time",
        names = c("A", "B", "C"), border = c("darkblue", "cyan3", "mediumorchid3"), col = c("white", "white", "white"))
legend("topleft", legend = c("Treatment 1", "Neutral", "Treatment 2"), text.col = c("darkblue", "cyan3", "mediumorchid3"),
       cex = 0.8)
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
## Pairwise comparisons (anova and t-test)
prelim2pwc <- compare_means(Weight~Group, data = prelim2_ANOVA)
prelim2pwc
prelim2comparisons <- list(c("Treatment 1 (A)", "Neutral (B)"), c("Treatment 1 (A)", "Treatment 2 (C)"), c("Neutral (B)", "Treatment 2 (C)"))
## Graph
library(ggpubr)
ggboxplot(prelim2_ANOVA, x = "Group", y = "Weight", col = c("darkblue", "cyan3", "mediumorchid3"), main = "Preliminary 2 ANOVA", xlab = "Location", ylab = "Proportion of Time")+
  stat_compare_means(comparisons = prelim2comparisons)+
  stat_compare_means(method = "anova", label.y = 1.2)
ggboxplot(prelim2_ANOVA, x = "Group", y = "Weight", col = c("darkblue", "cyan3", "mediumorchid3"), main = "Preliminary 2 Kruskal", xlab = "Location", ylab = "Proportion of Time")+
  stat_compare_means(comparisons = prelim2comparisons)+
  stat_compare_means(method = "kruskal.test", label.y = 1.2)
## T-test for corners
## Boxplot for normality
boxplot(prelim2$Yes_proportion, prelim2$No_proportion,  
        main = "Preliminary 2 Corners", xlab = "In Corner vs Not", ylab = "Proportion of Time",
        names = c("Yes", "No"), border = c("darkorange", "darkred"), col = c("white", "white"))
## Shapiro-Wilk test for normality with paired data
shapiro.test(prelim2$Yes_proportion)
shapiro.test(prelim2$No_proportion)
## Both normally distributed
## load data in form for test of variances and Ttest
prelim2_Ttest <- read.csv(file.choose())
## test of equal variance
library(car)
bartlett.test(Weight ~ Group, data = prelim2_Ttest)
var.test(Weight~Group, data = prelim2_Ttest, alternative = "two.sided")
##Not sure if test of equal variance for paired ttest is needed but ran in case
## Paired t-test for parametric
prelim2no <- prelim2$No_proportion
prelim2yes <- prelim2$Yes_proportion
## Paired t-test for parametric
prelim2ttestcorners <- t.test(prelim2no, prelim2yes, paired = TRUE)
prelim2ttestcorners
## Paired wilcox test because nonparametric
prelim2wilcox <- wilcox.test(prelim2no, prelim2yes, paired = TRUE)
prelim2wilcox
## Graph
ggboxplot(prelim2_Ttest, x = "Group", y = "Weight", 
          main = "Preliminary 2 Corners", col = c("darkorange", "darkred"), 
          xlab = "In Corner vs Not", ylab = "Proportion of Time")+
  stat_compare_means(method = "t.test", label.y = 1.1)
ggboxplot(prelim2_Ttest, x = "Group", y = "Weight", 
          main = "Preliminary 2 Corners", col = c("darkorange", "darkred"), 
          xlab = "In Corner vs Not", ylab = "Proportion of Time")+
  stat_compare_means(method = "wilcox.test", label.y = 1.1)
## T-test for hide vs not
## Boxplot for normality
boxplot(prelim2$Hide_proportion, prelim2$No_Hide_proportion, 
        main = "Preliminary 2 Hide", xlab = "Hide vs Not", ylab = "Proportion of Time",
        names = c("Yes", "No"), border = c("deeppink", "deeppink4"), col = c("white", "white"))
## Shapiro-Wilk test for normality with paired data
shapiro.test(prelim2$No_Hide_proportion)
shapiro.test(prelim2$Hide_proportion)
## Both normally distributed
## load data in form for test of variances and Ttest
prelim2_Hide <- read.csv(file.choose())
## test of equal variance
library(car)
bartlett.test(Weight ~ Group, data = prelim2_Hide)
var.test(Weight~Group, data = prelim2_Hide, alternative = "two.sided")
##Not sure if test of equal variance for paired ttest is needed but ran in case
prelim2nohide <- prelim2$No_Hide_proportion
prelim2yeshide <- prelim2$Hide_proportion
## Paired t-test for parametric
prelim2ttesthide <- t.test(prelim2nohide, prelim2yeshide, paired = TRUE)
prelim2ttesthide
## Paired wilcox test because nonparametric
prelim2hidewilcox <- wilcox.test(prelim2nohide, prelim2yeshide, paired = TRUE)
prelim2hidewilcox
## Significant difference between hide and no hide when don't consider neutral area
## Graph
ggboxplot(prelim2_Hide, x = "Group", y = "Weight", col = c("deeppink", "deeppink4"), main = "Preliminary 2 Hide", 
          xlab = "Hide vs No Hide", ylab = "Proportion of Time")+
  stat_compare_means(method = "t.test", label.y = 1.0)
ggboxplot(prelim2_Hide, x = "Group", y = "Weight", col = c("deeppink", "deeppink4"), main = "Preliminary 2 Hide", 
          xlab = "Hide vs No Hide", ylab = "Proportion of Time")+
  stat_compare_means(method = "wilcox.test", label.y = 1.0)
