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
install.packages("DHARMa")
library(DHARMa)
library(patchwork)

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

## Male Movement/Placement Data on Difference between female time near Control vs Infected male LMER

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

## Female Mate Choice Boxplot ####
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

## Picture Data Analysis ####
## Data Files
FrogImageData <- read.csv(file.choose())
FrogImageDataDorsal <- subset(FrogImageData, Dorsal_Ventral == "Dorsal")
FrogImageDataDorsalMale <- subset(FrogImageDataDorsal, Sex == "Male")

## Check distribution
ggdensity(FrogImageDataDorsalMale$Average.Brightness)
ggqqplot(FrogImageDataDorsalMale$Average.Brightness)
## fairly normal, have a bit of a hump

ggdensity(FrogImageDataDorsalMale$Redness.score)
ggqqplot(FrogImageDataDorsalMale$Redness.score)
## normal

ggdensity(FrogImageDataDorsalMale$Greeness.score)
ggqqplot(FrogImageDataDorsalMale$Greeness.score)
## normal

ggdensity(FrogImageDataDorsalMale$Blueness.score)
ggqqplot(FrogImageDataDorsalMale$Blueness.score)
## normal

## Picture Analysis LMERs for Day

BrightnessDorsalDay <- lmer(Average.Brightness~Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BrightnessDorsalDay)
## Interaction not significant, removing
BrightnessDorsalDay <- lmer(Average.Brightness~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BrightnessDorsalDay)
summary(BrightnessDorsalDay)
emmeans(BrightnessDorsalDay, list (pairwise~Day), lmer.df = "satterthwaite")
emmeans(BrightnessDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Brightness decreases over days for both control and infected frogs and not different between frogs

## Check assumptions 
## Linearity
AverageBrightness <- FrogImageDataDorsalMale$Average.Brightness
BrightnessDorsalDay.Linearity<-plot(resid(BrightnessDorsalDay),AverageBrightness) ## has a pattern but not too bad
## Homogeneity of Variance
FrogImageDataDorsalMale$Bright.Res <- residuals(BrightnessDorsalDay)
FrogImageDataDorsalMale$Abs.Bright.Res <- abs(FrogImageDataDorsalMale$Bright.Res)
FrogImageDataDorsalMale$Bright.Res2 <- FrogImageDataDorsalMale$Abs.Bright.Res^2
Levene.Bright <- lm(Bright.Res2 ~ Frog_Number, data=FrogImageDataDorsalMale) 
anova(Levene.Bright)
## homoscedasticity met
## Visual model
plot(BrightnessDorsalDay)
## randomly distributed

## Assumptions seem to support LMER

## DHARMA to check for outliers
simsBrightnessDorsalDay <- simulateResiduals(BrightnessDorsalDay)
plot(simsBrightnessDorsalDay, quantreg = FALSE)
## No significant tests, so not detecting outliers

RednessDorsalDay <- lmer(Redness.score~Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(RednessDorsalDay)
## no interaction, so removing
RednessDorsalDay <- lmer(Redness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(RednessDorsalDay)
summary(RednessDorsalDay)
emmeans(RednessDorsalDay, list (pairwise~Day), lmer.df = "satterthwaite")
emmeans(RednessDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## neither Day nor frog type is significant, overall redness score next to the

## Check assumptions 
## Linearity
Redness <- FrogImageDataDorsalMale$Redness.score
RednessDorsalDay.Linearity<-plot(resid(RednessDorsalDay),Redness) ## has a pattern but not too bad
## Homogeneity of Variance
FrogImageDataDorsalMale$Red.Res <- residuals(RednessDorsalDay)
FrogImageDataDorsalMale$Abs.Red.Res <- abs(FrogImageDataDorsalMale$Red.Res)
FrogImageDataDorsalMale$Red.Res2 <- FrogImageDataDorsalMale$Abs.Red.Res^2
Levene.Red <- lm(Red.Res2 ~ Frog_Number, data=FrogImageDataDorsalMale) 
anova(Levene.Red)
## homoscedasticity met
## Visual model
plot(RednessDorsalDay)
## randomly distributed

## Assumptions seem to support LMER

## DHARMA to check for outliers
simsRednessDorsalDay <- simulateResiduals(RednessDorsalDay)
plot(simsRednessDorsalDay, quantreg = FALSE)
## No significant tests, so not detecting outliers
