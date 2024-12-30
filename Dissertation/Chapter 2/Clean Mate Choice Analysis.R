## Chapter 2 R code: Clean Version

## Packages ####
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)
library(lmtest)
library(car)

## Mate Choice Experiment Data Analyses ####

## Female Choice
## Data File
MateChoiceAnalysisInfected <- read.csv(file.choose())
## Check distribution
ggdensity(MateChoiceAnalysisInfected$Weight_Seconds_Female, main = "Density Plot of Weight Seconds", xlab = " Weight Seconds")
ggqqplot(MateChoiceAnalysisInfected$Weight_Seconds_Female)
## Distribution of Weight Seconds Female doesn't seem to indicate lmer good idea

## Female Mate Choice LMER
InfectedTrialLMER <- lmer(Weight_Seconds_Female~Group+(1|Male_Pair_Letter)+(1|Frog_Number) , data = MateChoiceAnalysisInfected, 
                            weights = wt)
anova(InfectedTrialLMER)
## Check assumptions 
## Linearity
WeightSecondsFemale <- MateChoiceAnalysisInfected$Weight_Seconds_Female
InfectedTrialLMER.Linearity<-plot(resid(InfectedTrialLMER),WeightSecondsFemale) ## violates assumption
## Homogeneity of Variance
MateChoiceAnalysisInfected$Model.Res <- residuals(InfectedTrialLMER)
MateChoiceAnalysisInfected$Abs.Model.Res <- abs(MateChoiceAnalysisInfected$Model.Res)
MateChoiceAnalysisInfected$Model.Res2 <- MateChoiceAnalysisInfected$Abs.Model.Res^2
Levene.Model <- lm(Model.Res2 ~ Frog_Number, data=MateChoiceAnalysisInfected) 
anova(Levene.Model)
## homoscedasticity not met
## Visual model
plot(InfectedTrialLMER)
## not randomly distributed

## overall doesn't seem an lmer is a good idea for female choice, so doing glmer


## Female Mate Choice GLMER
InfectedTrialGLMER <- glmer(Weight_Seconds_Female~Group+(1|Male_Pair_Letter)+(1|Frog_Number) , data = MateChoiceAnalysisInfected, 
                            weights = wt, family = "poisson")
anova(InfectedTrialGLMER)
summary(InfectedTrialGLMER)
plot(InfectedTrialGLMER)

## Effect of Male Movement/Placement Data on Difference between female time near Control vs Infected male
## Data File
MateChoiceAnalysisInfectedDiff <- read.csv(file.choose())
## check data distribution
ggdensity(MateChoiceAnalysisInfectedDiff$Difference_Seconds_Female, main = "Density Plot of Total Trial Time Seconds", xlab = " Total Trial Time Seconds")
ggqqplot(MateChoiceAnalysisInfectedDiff$Difference_Seconds_Female)
## normal distribution
## Distribution of Difference Seconds Female seem to indicate lmer good idea

## ale Movement/Placement Data on Difference between female time near Control vs Infected male LMER

InfectedTrialDiffLMER <- lmer(Difference_Seconds_Female~Clean_Male_Front+Infected_Male_Front+Clean_Male_Wander+
                                Infected_Male_Wander+(1|Male_Pair_Letter)+(1|Frog_Number) , data = MateChoiceAnalysisInfectedDiff, weights = wt)
anova(InfectedTrialDiffLMER)
summary(InfectedTrialDiffLMER)
plot(InfectedTrialDiffLMER)
emmeans(InfectedTrialDiffLMER, list (pairwise~Infected_Male_Wander), lmer.df = "satterthwaite")
emmeans(InfectedTrialDiffLMER, list (pairwise~Clean_Male_Wander), lmer.df = "satterthwaite")
emmeans(InfectedTrialDiffLMER, list (pairwise~Infected_Male_Front), lmer.df = "satterthwaite")
emmeans(InfectedTrialDiffLMER, list (pairwise~Clean_Male_Front), lmer.df = "satterthwaite")

## Check assumptions 
## Linearity
DiffSecondsFemale <- MateChoiceAnalysisInfectedDiff$Difference_Seconds_Female
InfectedTrialDiffLMER.Linearity<-plot(resid(InfectedTrialDiffLMER),DiffSecondsFemale) ## has a pattern but not too bad
## Homogeneity of Variance
MateChoiceAnalysisInfectedDiff$Diff.Res <- residuals(InfectedTrialDiffLMER)
MateChoiceAnalysisInfectedDiff$Abs.Diff.Res <- abs(MateChoiceAnalysisInfectedDiff$Diff.Res)
MateChoiceAnalysisInfectedDiff$Diff.Res2 <- MateChoiceAnalysisInfectedDiff$Abs.Diff.Res^2
Levene.Diff <- lm(Diff.Res2 ~ Frog_Number, data=MateChoiceAnalysisInfectedDiff) 
anova(Levene.Diff)
## homoscedasticity met
## Visual model
plot(InfectedTrialDiffLMER)
## randomly distributed

## seems to meet assumptions for LMER

## Female Mate Choice Boxplot
MateChoiceAnalysisInfected$Group <- factor(MateChoiceAnalysisInfected$Group, levels = c("Control", "Infected"))
mc1 <- ggboxplot(MateChoiceAnalysisInfected, x = "Group", y = "Weight_Seconds_Female",  ylab = " Time (seconds)", xlab = "Male",
                 ylim = c(0, 1100), fill = "Group", palette = c("snow3", "snow4")) + 
  scale_y_continuous(breaks=seq(0,900,by=100))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.title.x = element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  annotate("text", x=1, y=277, label="x", size = 7)+
  annotate("text", x=2, y=336, label="x", size = 7)+
  geom_signif(y_position = c(1000), xmin = c(1), xmax = c(2),
              annotation = c("***"), tip_length = 0, textsize = 5)
mc1
