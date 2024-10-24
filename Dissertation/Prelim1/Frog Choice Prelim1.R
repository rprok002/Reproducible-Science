## Preliminary Trial 1
## Remember for this that groups are not independent, but if meets normality and
## equal variance assumptions for ttest and ANOVA, can argue for use
install.packages("ggplot2")
install.packages("ggrepel")
## One-way ANOVA
prelim1 <- read.csv(file.choose())
## Boxplot for normality
boxplot(prelim1$Location_A_Proportion, prelim1$Location_B_Proportion, prelim1$Location_C_Proportion, 
        main = "Preliminary 1 Locations", xlab = "Location (A,B,C)", ylab = "Proportion of Time",
        names = c("A", "B", "C"), border = c("darkblue", "cyan3", "mediumorchid3"), col = c("white", "white", "white"))
legend("topleft", legend = c("Treatment 1", "Neutral", "Treatment 2"), text.col = c("darkblue", "cyan3", "mediumorchid3"),
       cex = 0.8)
## Shapiro-Wilk test for normality
shapiro.test(prelim1$Location_A_Proportion)
shapiro.test(prelim1$Location_B_Proportion)
shapiro.test((prelim1$Location_C_Proportion))
## location A and B are normal, location C is not normal
## Boxplot for square-root transformed
sqrtA <- sqrt(prelim1$Location_A_Proportion)
sqrtB <- sqrt(prelim1$Location_B_Proportion)
sqrtC <- sqrt(prelim1$Location_C_Proportion)
boxplot(sqrtA, sqrtB, sqrtC,main = "Preliminary 1 Locations", xlab = "Location (A,B,C)", ylab = "Proportion of Time",
        names = c("A", "B", "C"), border = c("darkblue", "cyan3", "mediumorchid3"), col = c("white", "white", "white"))
legend("top", legend = c("Treatment 1", "Neutral", "Treatment 2"), text.col = c("darkblue", "cyan3", "mediumorchid3"),
       cex = 0.8)
shapiro.test(sqrtA)
shapiro.test(sqrtB)
shapiro.test(sqrtC)
## locations A and C are not normal, not a good transformation
## remove MV3 and try again 
prelim1noMV3 <- prelim1[-c(5),]
## Boxplot for normality 
boxplot(prelim1noMV3$Location_A_Proportion, prelim1noMV3$Location_B_Proportion, prelim1noMV3$Location_C_Proportion, 
        main = "Space Use Choice Trial 1", xlab = "Location (A,B,C)", ylab = "Proportion of Time",
        names = c("A", "B", "C"), border = c("darkblue", "cyan3", "mediumorchid3"), col = c("white", "white", "white")) 
legend("topright", legend = c("Treatment 1", "Neutral", "Treatment 2"), text.col = c("darkblue", "cyan3", "mediumorchid3"),
       cex = 0.8)
text(x=2, y=0.6, labels="n=10")
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
tukeyprelim1 <- tukey_hsd(prelim1aov)
tukeyprelim1
pairwise.wilcox.test(prelim1_ANOVA$Weight, prelim1_ANOVA$Group,
                     p.adjust.method = "BH")
## Only sig diff is between A and B
## Pairwise comparisons (anova and t-test) NOTE THAT THESE AREN'T THE SAME AS THE TUKEY AND WILCOXON FROM ABOVE,
## SO NOT THE BEST TO USE ESPECIALLY IF THE SIG VALUES ARE THIS CLOSE 0.05
prelim1pwc <- compare_means(Weight~Group, data = prelim1_ANOVA)
prelim1pwc
prelim1comparisons <- list(c("Treatment 1 (A)", "Neutral (B)"), c("Treatment 1 (A)", "Treatment 2 (C)"), c("Neutral (B)", "Treatment 2 (C)"))
## Graph
library(ggpubr)
ggboxplot(prelim1_ANOVA, x = "Group", y = "Weight", col = c("darkblue", "cyan3", "mediumorchid3"), main = "Space Use Choice Trial 1 ANOVA", xlab = "Location", ylab = "Proportion of Time")+
  stat_pvalue_manual(tukeyprelim1, label = "p.adj", y.position = c(0.8,0.9,1.0))+
  stat_compare_means(method = "anova", label.y = 1.1)+
  geom_text(x=3, y=1.1, label= "n=10")
ggboxplot(prelim1_ANOVA, x = "Group", y = "Weight", col = c("darkblue", "cyan3", "mediumorchid3"), main = "Space Use Choice Trial 1 Kruskal", xlab = "Location", ylab = "Proportion of Time")+
  stat_compare_means(comparisons = prelim1comparisons)+
  stat_compare_means(method = "kruskal.test", label.y = 1.0)+
  geom_text(x=3, y=1.0, label= "n=10")
## T-test for corners
## Boxplot for normality
boxplot(prelim1noMV3$Yes_proportion, prelim1noMV3$No_proportion, 
        main = "Preliminary 1 Corners", xlab = "In Corner vs Not", ylab = "Proportion of Time",
        names = c("Yes", "No"), border = c("darkorange", "darkred"), col = c("white", "white"))
## Shapiro-Wilk test for normality with paired data
dfprelim1 <- prelim1noMV3$Yes_proportion-prelim1noMV3$No_proportion
shapiro.test(dfprelim1)
## Very close to being normally distributed
## load data in form for test of variances and Ttest
prelim1_Ttest <- read.csv(file.choose())
## test of equal variance
library(car)
bartlett.test(Weight ~ Group, data = prelim1_Ttest)
var.test(Weight~Group, data = prelim1_Ttest, alternative = "two.sided")
##Not sure if test of equal variance for paired ttest is needed but ran in case
## Paired wilcox test because nonparametric
prelim1no <- prelim1noMV3$No_proportion
prelim1yes <- prelim1noMV3$Yes_proportion
prelim1wilcox <- wilcox.test(prelim1no, prelim1yes, paired = TRUE)
prelim1wilcox
## Graph
ggboxplot(prelim1_Ttest, x = "Group", y = "Weight", main = "Space Use Choice Trial 1 Corners", col = c("darkorange", "darkred"), xlab = "In Corner vs Not", ylab = "Proportion of Time")+
  stat_compare_means(method = "wilcox.test", label.y = 1.1)+
  geom_text(x=1, y=1.0, label= "n=10")
ggboxplot(prelim1_Ttest, x = "Group", y = "Weight", main = "Space Use Choice Trial 1 Corners", col = c("darkorange", "darkred"), xlab = "In Corner vs Not", ylab = "Proportion of Time")+
  stat_compare_means(method = "t.test", label.y = 1.1)+
  geom_text(x=1, y=1.0, label= "n=10")


