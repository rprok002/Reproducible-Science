## Preliminary Trial 1
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
## load data in form for test of variancea and ANOVA
prelim1_ANOVA <- read.csv(file.choose())
## test of equal variance
