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
## neither Day nor frog type is significant

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

GreenessDorsalDay <- lmer(Greeness.score~Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(GreenessDorsalDay)
## No interaction, so removing
GreenessDorsalDay <- lmer(Greeness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(GreenessDorsalDay)
summary(GreenessDorsalDay)
emmeans(GreenessDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
emmeans(GreenessDorsalDay, list (pairwise~Day), lmer.df = "satterthwaite")
## Greeness increases over day for both control and infected frogs

## Check assumptions 
## Linearity
Greeness <- FrogImageDataDorsalMale$Greeness.score
GreenessDorsalDay.Linearity<-plot(resid(GreenessDorsalDay),Greeness) ## has a pattern but not too bad
## Homogeneity of Variance
FrogImageDataDorsalMale$Green.Res <- residuals(GreenessDorsalDay)
FrogImageDataDorsalMale$Abs.Green.Res <- abs(FrogImageDataDorsalMale$Green.Res)
FrogImageDataDorsalMale$Green.Res2 <- FrogImageDataDorsalMale$Abs.Green.Res^2
Levene.Green <- lm(Green.Res2 ~ Frog_Number, data=FrogImageDataDorsalMale) 
anova(Levene.Green)
## homoscedasticity just barely not met statistically but the qqplot earlier looked ok
## Visual model
plot(GreenessDorsalDay)
## randomly distributed

## Assumptions seem to support LMER

## DHARMA to check for outliers
simsGreenessDorsalDay <- simulateResiduals(GreenessDorsalDay)
plot(simsGreenessDorsalDay, quantreg = FALSE)
## No significant tests, so not detecting outliers

BluenessDorsalDay <- lmer(Blueness.score~Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BluenessDorsalDay)
## No interaction, so removing
BluenessDorsalDay <- lmer(Blueness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BluenessDorsalDay)
summary(BluenessDorsalDay)
emmeans(BluenessDorsalDay, list (pairwise~Day), lmer.df = "satterthwaite")
emmeans(BluenessDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Decrease over day

## Check assumptions 
## Linearity
Blueness <- FrogImageDataDorsalMale$Blueness.score
BluenessDorsalDay.Linearity<-plot(resid(BluenessDorsalDay),Blueness) ## has a pattern but not too bad
## Homogeneity of Variance
FrogImageDataDorsalMale$Blue.Res <- residuals(BluenessDorsalDay)
FrogImageDataDorsalMale$Abs.Blue.Res <- abs(FrogImageDataDorsalMale$Blue.Res)
FrogImageDataDorsalMale$Blue.Res2 <- FrogImageDataDorsalMale$Abs.Blue.Res^2
Levene.Blue <- lm(Blue.Res2 ~ Frog_Number, data=FrogImageDataDorsalMale) 
anova(Levene.Blue)
## homoscedasticity met
## Visual model
plot(BluenessDorsalDay)
## randomly distributed

## Assumptions seem to support LMER

## DHARMA to check for outliers
simsBluenessDorsalDay <- simulateResiduals(BluenessDorsalDay)
plot(simsBluenessDorsalDay, quantreg = FALSE)
## No significant tests, so not detecting outliers

## Picture Analysis LMERs for Log Infection

BrightnessInfectionDorsal <- lmer(Average.Brightness~Frog_Type+Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BrightnessInfectionDorsal)
summary(BrightnessInfectionDorsal)
emmeans(BrightnessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
emmeans(BrightnessInfectionDorsal, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Brightness decreases as infection load increases, but not different between control and infected frogs


## Check assumptions 
## Linearity
AverageBrightness <- FrogImageDataDorsalMale$Average.Brightness
BrightnessInfection.Linearity<-plot(resid(BrightnessInfectionDorsal),AverageBrightness) ## has a pattern but not too bad
## Homogeneity of Variance
FrogImageDataDorsalMale$Brightlog.Res <- residuals(BrightnessInfectionDorsal)
FrogImageDataDorsalMale$Abs.Brightlog.Res <- abs(FrogImageDataDorsalMale$Brightlog.Res)
FrogImageDataDorsalMale$Brightlog.Res2 <- FrogImageDataDorsalMale$Abs.Brightlog.Res^2
Levene.Brightlog <- lm(Brightlog.Res2 ~ Frog_Number, data=FrogImageDataDorsalMale) 
anova(Levene.Brightlog)
## homoscedasticity met
## Visual model
plot(BrightnessInfectionDorsal)
## randomly distributed

## Assumptions seem to support LMER

## DHARMA to check for outliers
simsBrightnessInfectionDorsal <- simulateResiduals(BrightnessInfectionDorsal)
plot(simsBrightnessInfectionDorsal, quantreg = FALSE)
## No significant tests, so not detecting outliers

RednessInfectionDorsal <- lmer(Redness.score~Frog_Type+Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(RednessInfectionDorsal)
summary(RednessInfectionDorsal)
emmeans(RednessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
emmeans(RednessInfectionDorsal, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Redness score not significant for either variable

## Check assumptions 
## Linearity
Redness <- FrogImageDataDorsalMale$Redness.score
RednessInfectionDorsal.Linearity<-plot(resid(RednessInfectionDorsal),Redness) ## has a pattern but not too bad
## Homogeneity of Variance
FrogImageDataDorsalMale$Redlog.Res <- residuals(RednessInfectionDorsal)
FrogImageDataDorsalMale$Abs.Redlog.Res <- abs(FrogImageDataDorsalMale$Redlog.Res)
FrogImageDataDorsalMale$Redlog.Res2 <- FrogImageDataDorsalMale$Abs.Redlog.Res^2
Levene.Redlog <- lm(Redlog.Res2 ~ Frog_Number, data=FrogImageDataDorsalMale) 
anova(Levene.Redlog)
## homoscedasticity met
## Visual model
plot(RednessInfectionDorsal)
## randomly distributed

## Assumptions seem to support LMER

## DHARMA to check for outliers
simsRednessInfectionDorsal <- simulateResiduals(RednessInfectionDorsal)
plot(simsRednessInfectionDorsal, quantreg = FALSE)
## No significant tests, so not detecting outliers

GreenessInfectionDorsal <- lmer(Greeness.score~Frog_Type+Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(GreenessInfectionDorsal)
summary(GreenessInfectionDorsal)
emmeans(GreenessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
emmeans(GreenessInfectionDorsal, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## not significant for either variable


## Check assumptions 
## Linearity
Greeness <- FrogImageDataDorsalMale$Greeness.score
GreenessInfectionDorsal.Linearity<-plot(resid(GreenessInfectionDorsal),Greeness) ## has a pattern but not too bad
## Homogeneity of Variance
FrogImageDataDorsalMale$Greenlog.Res <- residuals(GreenessInfectionDorsal)
FrogImageDataDorsalMale$Abs.Greenlog.Res <- abs(FrogImageDataDorsalMale$Greenlog.Res)
FrogImageDataDorsalMale$Greenlog.Res2 <- FrogImageDataDorsalMale$Abs.Greenlog.Res^2
Levene.Greenlog <- lm(Greenlog.Res2 ~ Frog_Number, data=FrogImageDataDorsalMale) 
anova(Levene.Greenlog)
## homoscedasticity not met but qqplot earlier was ok
## Visual model
plot(GreenessInfectionDorsal)
## randomly distributed

## Assumptions not great for LMER but could make an argument to keep 

## DHARMA to check for outliers
simsGreenessInfectionDorsal <- simulateResiduals(GreenessInfectionDorsal)
plot(simsGreenessInfectionDorsal, quantreg = FALSE)
## No significant tests, so not detecting outliers

BluenessInfectionDorsal <- lmer(Blueness.score~Frog_Type+Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BluenessInfectionDorsal)
summary(BluenessInfectionDorsal)
emmeans(BluenessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
emmeans(BluenessInfectionDorsal, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## not significant for either variable


## Check assumptions 
## Linearity
Blueness <- FrogImageDataDorsalMale$Blueness.score
BluenessInfectionDorsal.Linearity<-plot(resid(BluenessInfectionDorsal),Blueness) ## has a pattern but not too bad
## Homogeneity of Variance
FrogImageDataDorsalMale$Bluelog.Res <- residuals(BluenessInfectionDorsal)
FrogImageDataDorsalMale$Abs.Bluelog.Res <- abs(FrogImageDataDorsalMale$Bluelog.Res)
FrogImageDataDorsalMale$Bluelog.Res2 <- FrogImageDataDorsalMale$Abs.Bluelog.Res^2
Levene.Bluelog <- lm(Bluelog.Res2 ~ Frog_Number, data=FrogImageDataDorsalMale) 
anova(Levene.Bluelog)
## homoscedasticity met
## Visual model
plot(BluenessInfectionDorsal)
## randomly distributed


## Assumptions seem to support LMER

## DHARMA to check for outliers
simsBluenessInfectionDorsal <- simulateResiduals(BluenessInfectionDorsal)
plot(simsBluenessInfectionDorsal, quantreg = FALSE)
## No significant tests, so not detecting outliers

## Graphs for Picture Analysis ####
