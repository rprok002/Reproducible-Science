## Data Analysis Panama Chapter 1
NaiveControl= read.csv(file.choose())
NaiveDead= read.csv(file.choose())
NaiveLive = read.csv(file.choose())
LearnedControl = read.csv(file.choose())
LearnedDead = read.csv(file.choose())
LearnedLive = read.csv(file.choose())

##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

## Naive Control
ggdensity(NaiveControl$Side_A_Fraction, main = "Density Plot of Side A", xlab = " Side A")
ggqqplot(NaiveControl$Side_A_Fraction)
ggdensity(NaiveControl$Side_C_Fraction, main = "Density Plot of Side C", xlab = " Side C")
ggqqplot(NaiveControl$Side_C_Fraction)
ggdensity(NaiveControl$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveControl$Neutral_Fraction)
ggdensity(NaiveControl$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveControl$Total_Fraction)

## Learned Control
ggdensity(LearnedControl$Side_A_Fraction, main = "Density Plot of Side A", xlab = "Side A")
ggqqplot(LearnedControl$Side_A_Fraction)
ggdensity(LearnedControl$Side_C_Fraction, main = "Density Plot of Side C", xlab = "Side C")
ggqqplot(LearnedControl$Side_C_Fraction)
ggdensity(LearnedControl$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(LearnedControl$Neutral_Fraction)
ggdensity(LearnedControl$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(LearnedControl$Total_Fraction)

## Naive Dead
ggdensity(NaiveDead$Control_Fraction, main = "Density Plot of Control", xlab = "Control")
ggqqplot(NaiveDead$Control_Fraction)
ggdensity(NaiveDead$Experiment_Fraction, main = "Density Plot of Dead", xlab = "Dead")
ggqqplot(NaiveDead$Experiment_Fraction)
ggdensity(NaiveDead$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveDead$Neutral_Fraction)
ggdensity(NaiveDead$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveDead$Total_Fraction)

## Learned Dead
ggdensity(LearnedDead$Control_Fraction, main = "Density Plot of Control", xlab = "Control")
ggqqplot(LearnedDead$Control_Fraction)
ggdensity(LearnedDead$Experiment_Fraction, main = "Density Plot of Dead", xlab = "Dead")
ggqqplot(LearnedDead$Experiment_Fraction)
ggdensity(LearnedDead$Neutral_Fraction, main = "Density Plot of  Neutral", xlab = " Neutral")
ggqqplot(LearnedDead$Neutral_Fraction)
ggdensity(LearnedDead$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(LearnedDead$Total_Fraction)

## Naive Live
ggdensity(NaiveLive$Control_Fraction, main = "Density Plot of Contro,", xlab = "Control")
ggqqplot(NaiveLive$Control_Fraction)
ggdensity(NaiveLive$Experiment_Fraction, main = "Live", xlab = "Live")
ggqqplot(NaiveLive$Experiment_Fraction)
ggdensity(NaiveLive$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveLive$Neutral_Fraction)
ggdensity(NaiveLive$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveLive$Total_Fraction)

## Learned Live
ggdensity(LearnedLive$Control_Fraction, main = "Density Plot of Control", xlab = "Control")
ggqqplot(LearnedLive$Control_Fraction)
ggdensity(LearnedLive$Experiment_Fraction, main = "Density Plot of Live", xlab = "Live")
ggqqplot(LearnedLive$Experiment_Fraction)
ggdensity(LearnedLive$Neutral_Fraction, main = "Density Plot of  Neutral", xlab = "Neutral")
ggqqplot(LearnedLive$Neutral_Fraction)
ggdensity(LearnedLive$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(LearnedLive$Total_Fraction)

## Shapiro-Wilks test
## Null: normal: Alt: not normal
shapiro.test(NaiveControl$Side_A_Fraction)
## not normal
shapiro.test(NaiveControl$Side_C_Fraction)
## not normal
shapiro.test(NaiveControl$Neutral_Fraction)
## normal
shapiro.test(NaiveControl$Total_Fraction)
## normal

shapiro.test(LearnedControl$Side_A_Fraction)
## normal
shapiro.test(LearnedControl$Side_C_Fraction)
## slightly not normal
shapiro.test(LearnedControl$Neutral_Fraction)
## normal
shapiro.test(LearnedControl$Total_Fraction)
## normal

shapiro.test(NaiveDead$Control_Fraction)
## normal
shapiro.test(NaiveDead$Experiment_Fraction)
## normal
shapiro.test(NaiveDead$Neutral_Fraction)
## normal
shapiro.test(NaiveDead$Total_Fraction)
## normal

shapiro.test(LearnedDead$Control_Fraction)
## not normal
shapiro.test(LearnedDead$Experiment_Fraction)
## normal
shapiro.test(LearnedDead$Neutral_Fraction)
## normal
shapiro.test(LearnedDead$Total_Fraction)
## normal

shapiro.test(NaiveLive$Control_Fraction)
## normal
shapiro.test(NaiveLive$Experiment_Fraction)
## not normal
shapiro.test(NaiveLive$Neutral_Fraction)
## normal
shapiro.test(NaiveLive$Total_Fraction)
## normal

shapiro.test(LearnedLive$Control_Fraction)
## normal
shapiro.test(LearnedLive$Experiment_Fraction)
## slightly normal
shapiro.test(LearnedLive$Neutral_Fraction)
## slightly normal
shapiro.test(LearnedLive$Total_Fraction)
## normal

## Naive Control: not normal
## Learned Control: slightly not normal
## Naive Dead: normal
## Learned Dead: not normal
## Naive live: not normal
## Learned live: normal

## Naive seems less normal than learned, but overall not normally distributed

## Levene's test for equal variance
## Null: all pop var are equal; Alt: at least two are different
library(car)
NaiveControlAnalysis = read.csv(file.choose())
NaiveDeadAnalysis = read.csv(file.choose())
NaiveLiveAnalysis = read.csv(file.choose())
LearnedControlAnalysis = read.csv(file.choose())
LearnedDeadAnalysis = read.csv(file.choose())
LearnedLiveAnalysis = read.csv(file.choose())

NaiveControlVar = leveneTest(Weight~Group, NaiveControlAnalysis)
NaiveControlVar
## equal variance
NaiveDeadVar = leveneTest(Weight~Group, NaiveDeadAnalysis)
NaiveDeadVar
## equal variance
NaiveLiveVar = leveneTest(Weight~Group, NaiveLiveAnalysis)
NaiveLiveVar
## equal variance
LearnedControlVar = leveneTest(Weight~Group, LearnedControlAnalysis)
LearnedControlVar
## non-equal variance
LearnedDeadVar = leveneTest(Weight~Group, LearnedDeadAnalysis)
LearnedDeadVar
## equal variance
LearnedLiveVar = leveneTest(Weight~Group, LearnedLiveAnalysis)
LearnedLiveVar
## equal variance

## mostly equal variance across the board, except learned control 

## GLMs
install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)

require(lmerTest)
require(lme4)
library(emmeans)


## Naive Control
NaiveControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (Trial.Order) , data = NaiveControlAnalysis)
summary(NaiveControlGLM)
car::Anova(NaiveControlGLM, type="3")
emmeans(NaiveControlGLM, list (pairwise~Group), lmer.df = "satterthwaite")

NaiveControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveControlAnalysis)
summary(NaiveControlGLM)
car::Anova(NaiveControlGLM, type="3")
emmeans(NaiveControlGLM, list (pairwise~(Group)), lmer.df = "satterthwaite")

## no variation in frog number, liquid amount or trial order 
## sig diff between time spent in A versus C, more time in C I think
## Naive Dead

NaiveDeadGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveDeadAnalysis)
summary(NaiveDeadGLM)
car::Anova(NaiveDeadGLM, type="3")
emmeans(NaiveDeadGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## more in dead than control

## Naive Live
NaiveLiveGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveLiveAnalysis)
summary(NaiveLiveGLM)
car::Anova(NaiveLiveGLM, type="3")
emmeans(NaiveLiveGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Learned Control
LearnedControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedControlAnalysis)
summary(LearnedControlGLM)
car::Anova(LearnedControlGLM, type="3")
emmeans(LearnedControlGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Learned Dead
LearnedDeadGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedDeadAnalysis)
summary(LearnedDeadGLM)
car::Anova(LearnedDeadGLM, type="3")
emmeans(LearnedDeadGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Learned Live
LearnedLiveGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedLiveAnalysis)
summary(LearnedLiveGLM)
car::Anova(LearnedLiveGLM, type="3")
emmeans(LearnedLiveGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Control
AllControlAnalysis <- read.csv(file.choose())
AllControlGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order), data = AllControlAnalysis)
summary(AllControlGLM)
car::Anova(AllControlGLM, type="3")
emmeans(AllControlGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Dead
AllDeadAnalysis <- read.csv(file.choose())
AllDeadGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order), data = AllDeadAnalysis)
summary(AllDeadGLM)
car::Anova(AllDeadGLM, type="3")
emmeans(AllDeadGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")

## Time of Learned in Con compared to time of Naive in Dead is sig diff, but not relavent to data

## Live
AllLiveAnalysis <- read.csv(file.choose())
AllLiveGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order), data = AllLiveAnalysis)
summary(AllLiveGLM)
car::Anova(AllLiveGLM, type="3")
emmeans(AllLiveGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
