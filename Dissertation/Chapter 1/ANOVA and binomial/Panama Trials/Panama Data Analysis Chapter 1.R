## Data Analysis Panama Chapter 1
## First try with LM
NaiveControlPanama= read.csv(file.choose())
NaiveDeadPanama= read.csv(file.choose())
NaiveLivePanama = read.csv(file.choose())
LearnedControlPanama = read.csv(file.choose())
LearnedDeadPanama = read.csv(file.choose())
LearnedLivePanama = read.csv(file.choose())
NaiveControlFIUlive= read.csv(file.choose())
NaiveControlFIUdead= read.csv(file.choose())
NaiveDeadFIU= read.csv(file.choose())
NaiveLiveFIU = read.csv(file.choose())
NaiveControlFIUPanama = read.csv(file.choose())
NaiveDeadFIUPanama = read.csv(file.choose())
NaiveLiveFIUPanama = read.csv(file.choose())
AllControlFIUPanama = read.csv(file.choose())
AllDeadFIUPanama = read.csv(file.choose())
AllLiveFIUPanama = read.csv(file.choose())

##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

## Naive Control
ggdensity(NaiveControlPanama$Side_A_Fraction, main = "Density Plot of Side A", xlab = " Side A")
ggqqplot(NaiveControlPanama$Side_A_Fraction)
ggdensity(NaiveControlPanama$Side_C_Fraction, main = "Density Plot of Side C", xlab = " Side C")
ggqqplot(NaiveControlPanama$Side_C_Fraction)
ggdensity(NaiveControlPanama$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveControlPanama$Neutral_Fraction)
ggdensity(NaiveControlPanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveControlPanama$Total_Fraction)
## A and C don't look very normal at first glance, neutral looks ok

## Learned Control
ggdensity(LearnedControlPanama$Side_A_Fraction, main = "Density Plot of Side A", xlab = "Side A")
ggqqplot(LearnedControlPanama$Side_A_Fraction)
ggdensity(LearnedControlPanama$Side_C_Fraction, main = "Density Plot of Side C", xlab = "Side C")
ggqqplot(LearnedControlPanama$Side_C_Fraction)
ggdensity(LearnedControlPanama$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(LearnedControlPanama$Neutral_Fraction)
ggdensity(LearnedControlPanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(LearnedControlPanama$Total_Fraction)
## Doesn't really look normal to me

## Naive Dead
ggdensity(NaiveDeadPanama$Control_Fraction, main = "Density Plot of Control", xlab = "Control")
ggqqplot(NaiveDeadPanama$Control_Fraction)
ggdensity(NaiveDeadPanama$Experiment_Fraction, main = "Density Plot of Dead", xlab = "Dead")
ggqqplot(NaiveDeadPanama$Experiment_Fraction)
ggdensity(NaiveDeadPanama$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveDeadPanama$Neutral_Fraction)
ggdensity(NaiveDeadPanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveDeadPanama$Total_Fraction)
## Con and Dead don't look very normal but neutral and total do

## Learned Dead
ggdensity(LearnedDeadPanama$Control_Fraction, main = "Density Plot of Control", xlab = "Control")
ggqqplot(LearnedDeadPanama$Control_Fraction)
ggdensity(LearnedDeadPanama$Experiment_Fraction, main = "Density Plot of Dead", xlab = "Dead")
ggqqplot(LearnedDeadPanama$Experiment_Fraction)
ggdensity(LearnedDeadPanama$Neutral_Fraction, main = "Density Plot of  Neutral", xlab = " Neutral")
ggqqplot(LearnedDeadPanama$Neutral_Fraction)
ggdensity(LearnedDeadPanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(LearnedDeadPanama$Total_Fraction)
## not really normal all around

## Naive Live
ggdensity(NaiveLivePanama$Control_Fraction, main = "Density Plot of Control,", xlab = "Control")
ggqqplot(NaiveLivePanama$Control_Fraction)
ggdensity(NaiveLivePanama$Experiment_Fraction, main = "Live", xlab = "Live")
ggqqplot(NaiveLivePanama$Experiment_Fraction)
ggdensity(NaiveLivePanama$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveLivePanama$Neutral_Fraction)
ggdensity(NaiveLivePanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveLivePanama$Total_Fraction)
## Con and live look not normal but neutral looks ok

## Learned Live
ggdensity(LearnedLivePanama$Control_Fraction, main = "Density Plot of Control", xlab = "Control")
ggqqplot(LearnedLivePanama$Control_Fraction)
ggdensity(LearnedLivePanama$Experiment_Fraction, main = "Density Plot of Live", xlab = "Live")
ggqqplot(LearnedLivePanama$Experiment_Fraction)
ggdensity(LearnedLivePanama$Neutral_Fraction, main = "Density Plot of  Neutral", xlab = "Neutral")
ggqqplot(LearnedLivePanama$Neutral_Fraction)
ggdensity(LearnedLivePanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(LearnedLivePanama$Total_Fraction)

## Naive Control FIU live
ggdensity(NaiveControlFIUlive$Side_A_Fraction, main = "Density Plot of Side A", xlab = " Side A")
ggqqplot(NaiveControlFIUlive$Side_A_Fraction)
ggdensity(NaiveControlFIUlive$Side_C_Fraction, main = "Density Plot of Side C", xlab = " Side C")
ggqqplot(NaiveControlFIUlive$Side_C_Fraction)
ggdensity(NaiveControlFIUlive$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveControlFIUlive$Neutral_Fraction)
ggdensity(NaiveControlFIUlive$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveControlFIUlive$Total_Fraction)

## Naive Control FIU dead
ggdensity(NaiveControlFIUdead$Side_A_Fraction, main = "Density Plot of Side A", xlab = " Side A")
ggqqplot(NaiveControlFIUdead$Side_A_Fraction)
ggdensity(NaiveControlFIUdead$Side_C_Fraction, main = "Density Plot of Side C", xlab = " Side C")
ggqqplot(NaiveControlFIUdead$Side_C_Fraction)
ggdensity(NaiveControlFIUdead$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveControlFIUdead$Neutral_Fraction)
ggdensity(NaiveControlFIUdead$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveControlFIUdead$Total_Fraction)

## Naive Dead FIU
ggdensity(NaiveDeadFIU$Control_Fraction, main = "Density Plot of Control", xlab = "Control")
ggqqplot(NaiveDeadFIU$Control_Fraction)
ggdensity(NaiveDeadFIU$Experiment_Fraction, main = "Density Plot of Dead", xlab = "Dead")
ggqqplot(NaiveDeadFIU$Experiment_Fraction)
ggdensity(NaiveDeadFIU$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveDeadFIU$Neutral_Fraction)
ggdensity(NaiveDeadFIU$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveDeadFIU$Total_Fraction)

## Naive Live FIU
ggdensity(NaiveLiveFIU$Control_Fraction, main = "Density Plot of Control,", xlab = "Control")
ggqqplot(NaiveLiveFIU$Control_Fraction)
ggdensity(NaiveLiveFIU$Experiment_Fraction, main = "Live", xlab = "Live")
ggqqplot(NaiveLiveFIU$Experiment_Fraction)
ggdensity(NaiveLiveFIU$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveLiveFIU$Neutral_Fraction)
ggdensity(NaiveLiveFIU$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveLiveFIU$Total_Fraction)

## Naive Control FIU Panama
ggdensity(NaiveControlFIUPanama$Side_A_Fraction, main = "Density Plot of Side A", xlab = " Side A")
ggqqplot(NaiveControlFIUPanama$Side_A_Fraction)
ggdensity(NaiveControlFIUPanama$Side_C_Fraction, main = "Density Plot of Side C", xlab = " Side C")
ggqqplot(NaiveControlFIUPanama$Side_C_Fraction)
ggdensity(NaiveControlFIUPanama$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveControlFIUPanama$Neutral_Fraction)
ggdensity(NaiveControlFIUPanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveControlFIUPanama$Total_Fraction)

## Naive Dead FIU Panama
ggdensity(NaiveDeadFIUPanama$Control_Fraction, main = "Density Plot of Control", xlab = "Control")
ggqqplot(NaiveDeadFIUPanama$Control_Fraction)
ggdensity(NaiveDeadFIUPanama$Experiment_Fraction, main = "Density Plot of Dead", xlab = "Dead")
ggqqplot(NaiveDeadFIUPanama$Experiment_Fraction)
ggdensity(NaiveDeadFIUPanama$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveDeadFIUPanama$Neutral_Fraction)
ggdensity(NaiveDeadFIUPanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveDeadFIUPanama$Total_Fraction)

## Naive Live FIU Panama
ggdensity(NaiveLiveFIUPanama$Control_Fraction, main = "Density Plot of Control,", xlab = "Control")
ggqqplot(NaiveLiveFIUPanama$Control_Fraction)
ggdensity(NaiveLiveFIUPanama$Experiment_Fraction, main = "Live", xlab = "Live")
ggqqplot(NaiveLiveFIUPanama$Experiment_Fraction)
ggdensity(NaiveLiveFIUPanama$Neutral_Fraction, main = "Density Plot of Neutral", xlab = "Neutral")
ggqqplot(NaiveLiveFIUPanama$Neutral_Fraction)
ggdensity(NaiveLiveFIUPanama$Total_Fraction, main = "Density Plot of Total", xlab = "Total")
ggqqplot(NaiveLiveFIUPanama$Total_Fraction)


## Shapiro-Wilks test
## Null: normal: Alt: not normal
shapiro.test(NaiveControlPanama$Side_A_Fraction)
## not normal
shapiro.test(NaiveControlPanama$Side_C_Fraction)
## not normal
shapiro.test(NaiveControlPanama$Neutral_Fraction)
## normal
shapiro.test(NaiveControlPanama$Total_Fraction)
## normal

shapiro.test(LearnedControlPanama$Side_A_Fraction)
## not normal
shapiro.test(LearnedControlPanama$Side_C_Fraction)
## not normal
shapiro.test(LearnedControlPanama$Neutral_Fraction)
## not normal
shapiro.test(LearnedControlPanama$Total_Fraction)
## not normal

shapiro.test(NaiveDeadPanama$Control_Fraction)
## not normal
shapiro.test(NaiveDeadPanama$Experiment_Fraction)
## not normal
shapiro.test(NaiveDeadPanama$Neutral_Fraction)
## normal
shapiro.test(NaiveDeadPanama$Total_Fraction)
## normal

shapiro.test(LearnedDeadPanama$Control_Fraction)
## not normal
shapiro.test(LearnedDeadPanama$Experiment_Fraction)
## not normal
shapiro.test(LearnedDeadPanama$Neutral_Fraction)
## not normal
shapiro.test(LearnedDeadPanama$Total_Fraction)
## not normal

shapiro.test(NaiveLivePanama$Control_Fraction)
## normal
shapiro.test(NaiveLivePanama$Experiment_Fraction)
## not normal
shapiro.test(NaiveLivePanama$Neutral_Fraction)
## normal
shapiro.test(NaiveLivePanama$Total_Fraction)
## normal

shapiro.test(LearnedLivePanama$Control_Fraction)
## not normal
shapiro.test(LearnedLivePanama$Experiment_Fraction)
## not normal
shapiro.test(LearnedLivePanama$Neutral_Fraction)
## normal
shapiro.test(LearnedLivePanama$Total_Fraction)
## not normal

shapiro.test(NaiveControlFIUlive$Side_A_Fraction)
## not normal
shapiro.test(NaiveControlFIUlive$Side_C_Fraction)
## normal
shapiro.test(NaiveControlFIUlive$Neutral_Fraction)
## normal
shapiro.test(NaiveControlFIUlive$Total_Fraction)
## normal

shapiro.test(NaiveControlFIUdead$Side_A_Fraction)
## slightly not normal
shapiro.test(NaiveControlFIUdead$Side_C_Fraction)
## not normal
shapiro.test(NaiveControlFIUdead$Neutral_Fraction)
## normal
shapiro.test(NaiveControlFIUdead$Total_Fraction)
## normal

shapiro.test(NaiveDeadFIU$Control_Fraction)
## normal
shapiro.test(NaiveDeadFIU$Experiment_Fraction)
## normal
shapiro.test(NaiveDeadFIU$Neutral_Fraction)
## normal
shapiro.test(NaiveDeadFIU$Total_Fraction)
## normal

shapiro.test(NaiveLiveFIU$Control_Fraction)
## not normal
shapiro.test(NaiveLiveFIU$Experiment_Fraction)
## normal
shapiro.test(NaiveLiveFIU$Neutral_Fraction)
## normal
shapiro.test(NaiveLiveFIU$Total_Fraction)
## not normal

## Combined FIU and Panama Naive

shapiro.test(NaiveControlFIUPanama$Side_A_Fraction)
## not normal
shapiro.test(NaiveControlFIUPanama$Side_C_Fraction)
## not normal
shapiro.test(NaiveControlFIUPanama$Neutral_Fraction)
## normal
shapiro.test(NaiveControlFIUPanama$Total_Fraction)
## normal

shapiro.test(NaiveDeadFIUPanama$Control_Fraction)
## not normal
shapiro.test(NaiveDeadFIUPanama$Experiment_Fraction)
## not normal
shapiro.test(NaiveDeadFIUPanama$Neutral_Fraction)
## normal
shapiro.test(NaiveDeadFIUPanama$Total_Fraction)
## normal

shapiro.test(NaiveLiveFIUPanama$Control_Fraction)
## not normal
shapiro.test(NaiveLiveFIUPanama$Experiment_Fraction)
## not normal
shapiro.test(NaiveLiveFIUPanama$Neutral_Fraction)
## normal
shapiro.test(NaiveLiveFIUPanama$Total_Fraction)
## normal

## Naive Control Panama: A and C not normal, neutral and total normal
## Learned Control Panama:  not normal
## Naive Dead Panama: Con and Dead not noarmal, neutral and total normal
## Learned Dead Panama: not normal
## Naive Live Panama: Live not normal, rest normal
## Learned Live Panama: neutral normal, rest not normal
## Naive Control FIU Live: A not normal, rest normal
## Naive Control FIU Dead: A and C not normal, neutral and total normal
## Naive Dead FIU: normal
## Naive Live FIU: Con and Total not normal, Live and neutral normal
## Naive Control FIU Panama: A and C not normal, neutral and total normal
## Naive Dead FIU Panama: Con and Dead not normal, neutral and total normal
## Naive Live FIU Panama: Con and Live not normal, neutral and total normal

## Most are not normal, but GLM can run on nonnormal data so will run. 
## Combining FIU and Panama doesn't affect Control or Dead, turns Con for Live not normal

## Levene's test for equal variance
## Null: all pop var are equal; Alt: at least two are different
library(car)
NaiveControlPanamaAnalysis = read.csv(file.choose())
NaiveDeadPanamaAnalysis = read.csv(file.choose())
NaiveLivePanamaAnalysis = read.csv(file.choose())
LearnedControlPanamaAnalysis = read.csv(file.choose())
LearnedDeadPanamaAnalysis = read.csv(file.choose())
LearnedLivePanamaAnalysis = read.csv(file.choose())
NaiveControlFIULiveAnalysis = read.csv(file.choose())
NaiveControlFIUDeadAnalysis = read.csv(file.choose())
NaiveDeadFIUAnalysis = read.csv(file.choose())
NaiveLiveFIUAnalysis = read.csv(file.choose())
NaiveControlFIUPanamaAnalysis = read.csv(file.choose())
NaiveDeadFIUPanamaAnalysis = read.csv(file.choose())
NaiveLiveFIUPanamaAnalysis = read.csv(file.choose())
AllControlPanamaAnalysis = read.csv(file.choose())
AllDeadFIUPanamaAnalysis = read.csv(file.choose())
AllLiveFIUPanamaAnalysis = read.csv(file.choose())
AllControlFIUPanamaAnalysis = read.csv(file.choose())

NaiveControlPanamaVar = leveneTest(Weight~Group, NaiveControlPanamaAnalysis)
NaiveControlPanamaVar
## equal variance
NaiveDeadPanamaVar = leveneTest(Weight~Group, NaiveDeadPanamaAnalysis)
NaiveDeadPanamaVar
## equal variance
NaiveLivePanamaVar = leveneTest(Weight~Group, NaiveLivePanamaAnalysis)
NaiveLivePanamaVar
## equal variance
LearnedControlPanamaVar = leveneTest(Weight~Group, LearnedControlPanamaAnalysis)
LearnedControlPanamaVar
## equal variance
LearnedDeadPanamaVar = leveneTest(Weight~Group, LearnedDeadPanamaAnalysis)
LearnedDeadPanamaVar
## equal variance
LearnedLivePanamaVar = leveneTest(Weight~Group, LearnedLivePanamaAnalysis)
LearnedLivePanamaVar
## equal variance
NaiveControlFIULiveVar = leveneTest(Weight~Group, NaiveControlFIULiveAnalysis)
NaiveControlFIULiveVar
## equal variance
NaiveControlFIUDeadVar = leveneTest(Weight~Group, NaiveControlFIUDeadAnalysis)
NaiveControlFIUDeadVar
## equal variance
NaiveDeadFIUVar = leveneTest(Weight~Group, NaiveDeadFIUAnalysis)
NaiveDeadFIUVar
## equal variance
NaiveLiveFIUVar = leveneTest(Weight~Group, NaiveLiveFIUAnalysis)
NaiveLiveFIUVar
## equal variance
NaiveControlFIUPanamaVar = leveneTest(Weight~Group, NaiveControlFIUPanamaAnalysis)
NaiveControlFIUPanamaVar
##non-equal variance. Naive Control Panama was just barely equal variance and this pushed it over the edge
NaiveDeadFIUPanamaVar = leveneTest(Weight~Group, NaiveDeadFIUPanamaAnalysis)
NaiveDeadFIUPanamaVar
## equal variance
NaiveLiveFIUPanamaVar = leveneTest(Weight~Group, NaiveLiveFIUPanamaAnalysis)
NaiveLiveFIUPanamaVar
## equal variance
AllControlPanamaVar = leveneTest(Weight~Group, AllControlPanamaAnalysis)
AllControlPanamaVar
## non-equal variance
AllDeadFIUPanamaVar = leveneTest(Weight~Group, AllDeadFIUPanamaAnalysis)
AllDeadFIUPanamaVar
## equal variance
AllLiveFIUPanamaVar = leveneTest(Weight~Group, AllLiveFIUPanamaAnalysis)
AllLiveFIUPanamaVar
## equal variance
AllControlFIUPanamaVar = leveneTest(Weight~Group, AllControlFIUPanamaAnalysis)
AllControlFIUPanamaVar
## nonequal variance

## All equal variance except when combine control frogs between FIU and Panama
## Moving forward with just Panama control frogs
## seeing if need to use vcovCR for control of naive and learned since heterogeneity of variance
## http://cran.nexr.com/web/packages/clubSandwich/clubSandwich.pdf

## Total trial time normal, variance and GLMs for trial type
AllTotalsAnalysis = read.csv(file.choose())
ggdensity(AllTotalsAnalysis$Total.Trial.Time, main = "Density Plot of Total", xlab = "Total")
ggqqplot(AllTotalsAnalysis$Total.Trial.Time)
shapiro.test(AllTotalsAnalysis$Total.Trial.Time)
## not normal
AllTotalsSexVar = leveneTest(Total.Trial.Time~Sex, AllTotalsAnalysis)
AllTotalsSexVar
## eaual variance
AllTotalsTrialTypeVar = leveneTest(Total.Trial.Time~Trial, AllTotalsAnalysis)
AllTotalsTrialTypeVar
## equal variance
AllTotalsTrialOrderVar = leveneTest(Total.Trial.Time~Trial.Order, AllTotalsAnalysis)
AllTotalsTrialOrderVar
## equal variance
AllTotalsVar = leveneTest(Total.Trial.Time~Type*Trial.Order*Sex*Trial, AllTotalsAnalysis)
AllTotalsVar
## equal variance
AllTotalsGLM <- lmer(Total.Trial.Time~ (1|Frog_Number) + (1|Liquid.Amount) + Trial.Order+ Type*Trial +Sex , data = AllTotalsAnalysis)
summary(AllTotalsGLM)
emmeans(AllTotalsGLM, pairwise~Trial*Type, lmer.df = "satterthwaite")$contrasts

## No significant difference in total trial time between any combos when Type and Trial are interaction factors, so not using total trial time in final glms
## Using interaction between Type and Trial because trials arent just control, dead and live. Trials are control naive, control learned, dead naive, dead learned, etc.
## GLMs
install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)

require(lmerTest)
require(lme4)
library(emmeans)
library(multcomp)

## Naive Control Panama
NaiveControlPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveControlPanamaAnalysis)
summary(NaiveControlPanamaGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlPanamaGLM)
## contrasts
emmeans(NaiveControlPanamaGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts
## trial order and liquid amount don't do anything as random factors, trying as predictors

NaiveControlPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) + Liquid.Amount+ Trial.Order, data = NaiveControlPanamaAnalysis)
summary(NaiveControlPanamaGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlPanamaGLM)
## contrasts
emmeans(NaiveControlPanamaGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts
## trial order and liquid amount also don't do anything as predictors, removing from model


NaiveControlPanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number), data = NaiveControlPanamaAnalysis)
summary(NaiveControlPanamaGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlPanamaGLM)
## contrasts
emmeans(NaiveControlPanamaGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts

## significant difference between time spent in neutral and C, but not between A and C
## total trial time is significant so keeping in model

## Naive Dead FIU Panama
NaiveDeadFIUPanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveDeadFIUPanamaAnalysis)
summary(NaiveDeadFIUPanamaGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveDeadFIUPanamaGLM)
emmeans(NaiveDeadFIUPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")
## trial order and liquid amount don't do anything as random factors, trying as predictors

NaiveDeadFIUPanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number) + Liquid.Amount + Trial.Order , data = NaiveDeadFIUPanamaAnalysis)
summary(NaiveDeadFIUPanamaGLM)
## trial order and liquid amount also don't do anything as predictors, removing from model

NaiveDeadFIUPanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number) , data = NaiveDeadFIUPanamaAnalysis)
summary(NaiveDeadFIUPanamaGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveDeadFIUPanamaGLM)
emmeans(NaiveDeadFIUPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## no significance between time spent in any of the areas

## Naive Live FIU Panama
NaiveLiveFIUPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveLiveFIUPanamaAnalysis)
summary(NaiveLiveFIUPanamaGLM)
anova(NaiveLiveFIUPanamaGLM)
emmeans(NaiveLiveFIUPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")
## trial order and liquid amount don't do anything as random factors, trying as predictors

NaiveLiveFIUPanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number) + Liquid.Amount + Trial.Order, data = NaiveLiveFIUPanamaAnalysis)
summary(NaiveLiveFIUPanamaGLM)
anova(NaiveLiveFIUPanamaGLM)
emmeans(NaiveLiveFIUPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")
## trial order and liquid amount also don't do anything as predictors, removing from model

NaiveLiveFIUPanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number), data = NaiveLiveFIUPanamaAnalysis)
summary(NaiveLiveFIUPanamaGLM)
anova(NaiveLiveFIUPanamaGLM)
emmeans(NaiveLiveFIUPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## spend more time in neutral than either Con or Live but not different between Con and Live
## different between Con and neutral is barely significant
## total trial time sig diff

## Learned Control Panama
LearnedControlPanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedControlPanamaAnalysis)
summary(LearnedControlPanamaGLM)
anova(LearnedControlPanamaGLM)
car::Anova(LearnedControlGLM, type="3")
emmeans(LearnedControlPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")
## trial order and liquid amount don't do anything as random factors, trying as predictors

LearnedControlPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = LearnedControlPanamaAnalysis)
summary(LearnedControlPanamaGLM)
## trial order and liquid amount also don't do anything as predictors, removing from model

LearnedControlPanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number) , data = LearnedControlPanamaAnalysis)
summary(LearnedControlPanamaGLM)
anova(LearnedControlPanamaGLM)
emmeans(LearnedControlPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## no significance between any area

## Learned Dead
LearnedDeadPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedDeadPanamaAnalysis)
summary(LearnedDeadPanamaGLM)
anova(LearnedDeadPanamaGLM)
car::Anova(LearnedDeadGLM, type="3")
emmeans(LearnedDeadPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")
## trial order and liquid amount don't do anything as random factors, trying as predictors

LearnedDeadPanamaGLM <- lmer(Weight~Group+ Sex+(1|Frog_Number) + Liquid.Amount + Trial.Order, data = LearnedDeadPanamaAnalysis)
summary(LearnedDeadPanamaGLM)
## trial order and liquid amount also don't do anything as predictors, removing from model

LearnedDeadPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) , data = LearnedDeadPanamaAnalysis)
summary(LearnedDeadPanamaGLM)
anova(LearnedDeadPanamaGLM)
car::Anova(LearnedDeadPanamaGLM, type="3")
emmeans(LearnedDeadPanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")
## no significance

## Learned Panama Live
LearnedLivePanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedLivePanamaAnalysis)
summary(LearnedLivePanamaGLM)
anova(LearnedLivePanamaGLM)
car::Anova(LearnedLivePanamaGLM, type="3")
emmeans(LearnedLivePanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
LearnedLivePanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number) + Liquid.Amount + Trial.Order , data = LearnedLivePanamaAnalysis)
summary(LearnedLivePanamaGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
LearnedLivePanamaGLM <- lmer(Weight~Group+Sex+ (1|Frog_Number), data = LearnedLivePanamaAnalysis)
summary(LearnedLivePanamaGLM)
anova(LearnedLivePanamaGLM)
car::Anova(LearnedLivePanamaGLM, type="3")
emmeans(LearnedLivePanamaGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Naive Control FIU
NaiveControlFIUDeadGLM <- lmer(Weight~Group+Sex+(1|Frog_Number)  + (1|Trial.Order) , data = NaiveControlFIUDeadAnalysis)
summary(NaiveControlFIUDeadGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlFIUDeadGLM)
## contrasts
emmeans(NaiveControlFIUDeadGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts
## trial order doesn't do anything as random factors, trying as predictors

NaiveControlFIUDeadGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) + Trial.Order, data = NaiveControlFIUDeadAnalysis)
summary(NaiveControlFIUDeadGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlFIUDeadGLM)
## contrasts
emmeans(NaiveControlFIUDeadGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts
## trial order also doesn't do anything as predictors, removing from model

NaiveControlFIUDeadGLM <- lmer(Weight~Group+Sex+(1|Frog_Number), data = NaiveControlFIUDeadAnalysis)
summary(NaiveControlFIUDeadGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlFIUDeadGLM)
## contrasts
emmeans(NaiveControlFIUDeadGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts
## trial order also doesn't do anything as predictors, removing from model

## no significance 

NaiveControlFIULiveGLM <- lmer(Weight~Group+Sex+(1|Frog_Number)  + (1|Trial.Order) , data = NaiveControlFIULiveAnalysis)
summary(NaiveControlFIULiveGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlFIULiveGLM)
## contrasts
emmeans(NaiveControlFIULiveGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts
## trial order doesn't do anything as random factors, trying as predictors

## significance between ConA and Neutral but not different bewteen two Cons. Adding all control trials together

NaiveControlFIUPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) + (1|Liquid.Amount)+(1|Trial.Order), data = NaiveControlFIUPanamaAnalysis)
summary(NaiveControlFIUPanamaGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlFIUPanamaGLM)
## contrasts
emmeans(NaiveControlFIUPanamaGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts
## trial order and liquid amount not doing anything as random, redoing as predictors

NaiveControlFIUPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number) + Liquid.Amount+Trial.Order, data = NaiveControlFIUPanamaAnalysis)
summary(NaiveControlFIUPanamaGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlFIUPanamaGLM)
## contrasts
emmeans(NaiveControlFIUPanamaGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts
## trial order and liquid amount not doing anything as predictors, removing from model

NaiveControlFIUPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number), data = NaiveControlFIUPanamaAnalysis)
summary(NaiveControlFIUPanamaGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlFIUPanamaGLM)
## contrasts
emmeans(NaiveControlFIUPanamaGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts


## just slightly no significance
## nonequal variance so adding vocvCR
install.packages("clubSandwich")
library(clubSandwich)
NaiveControlFIUPanamaGLM <- lmer(Weight~Group+Sex+(1|Frog_Number), data = NaiveControlFIUPanamaAnalysis)
summary(NaiveControlFIUPanamaGLM)
vcovCR(NaiveControlFIUPanamaGLM, type = "CR2")
## not sure what to do, asking Christian

## Control
AllControlGLM <- lmer(Weight~Group*Type+Sex+(1|Frog_Number)+ (1|Trial.Order)+ (1|Liquid.Amount), data = AllControlFIUPanamaAnalysis)
summary(AllControlGLM)
anova(AllControlGLM)
emmeans(AllControlGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
## trial order and liquid amount not doing anything as random, redoing as predictors
AllControlGLM <- lmer(Weight~Group*Type+Sex+(1|Frog_Number)+ Trial.Order+ Liquid.Amount, data = AllControlFIUPanamaAnalysis)
summary(AllControlGLM)
anova(AllControlGLM)
emmeans(AllControlGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
## trial order and liquid amount not doing anything as predictors, removing from model
AllControlGLM <- lmer(Weight~Group*Type+Sex+(1|Frog_Number), data = AllControlFIUPanamaAnalysis)
summary(AllControlGLM)
anova(AllControlGLM)
emmeans(AllControlGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")

## no significance
## nonequal variance so waiting for Christian answer to see if need to run anything differently

## Dead
AllDeadGLM <- lmer(Weight~Group*Type+Sex+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order), data = AllDeadFIUPanamaAnalysis)
summary(AllDeadGLM)
anova(AllDeadGLM)
emmeans(AllDeadGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
## trial order and liquid amount don't do anything as random factors, trying as predictors

AllDeadGLM <- lmer(Weight~Group*Type+Sex+ (1|Frog_Number) + Liquid.Amount + Trial.Order, data = AllDeadFIUPanamaAnalysis)
summary(AllDeadGLM)
anova(AllDeadGLM)
emmeans(AllDeadGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
## trial order and liquid amount also don't do anything as predictors, removing from model

AllDeadGLM <- lmer(Weight~Group*Type+Sex+ (1|Frog_Number), data = AllDeadFIUPanamaAnalysis)
summary(AllDeadGLM)
anova(AllDeadGLM)
emmeans(AllDeadGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
## no significance

## Live
AllLiveGLM <- lmer(Weight~Group*Type+Sex+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order), data = AllLiveFIUPanamaAnalysis)
summary(AllLiveGLM)
anova(AllLiveGLM)
emmeans(AllLiveGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
AllLiveGLM <- lmer(Weight~Group*Type+Sex+ (1|Frog_Number) + Liquid.Amount + Trial.Order, data = AllLiveFIUPanamaAnalysis)
summary(AllLiveGLM)
anova(AllLiveGLM)
emmeans(AllLiveGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
## trial order and liquid amount also don't do anything as predictors, removing from model

AllLiveGLM <- lmer(Weight~Group*Type+Sex+ (1|Frog_Number), data = AllLiveFIUPanamaAnalysis)
summary(AllLiveGLM)
anova(AllLiveGLM)
emmeans(AllLiveGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")

## significance between Con and Neutral learned, which doesn't matter here
## significance between Con learned and Live naive, doesn't matter here
## signifiance between Neutral learned and Live naive, doesn't matter here
## significance between live naive and neutral naive, doesn't matter here



## Boxplot
library(ggplot2)
library(multcompView)
library(dplyr)
library(graphics)
library(ggsignif)

ggboxplot(NaiveControlFIUPanamaAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          ylim = c(0, 115), title = "Naive Control Scent") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Left","Right", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=110, label= "SS: 1571.16 ; DF: 2,113 ; p = 0.06")

ggboxplot(NaiveDeadFIUPanamaAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "grey80", ylim = c(0, 115), title = "Naive Dead Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Broth","Dead Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=110, label= "SS: 385.13 ; DF: 2,158 ; p = 0.34")
  

ggboxplot(NaiveLiveFIUPanamaAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "grey40", ylim = c(0, 115), title = "Naive Live Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Control","Live Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=110, label= "SS: 1300.4 ; DF: 2,17 ; p = 0.03")+
  geom_signif(comparisons = list(c("Con", "Neutral")), annotations = "*", textsize = 8, map_signif_level = TRUE, y_position = 90)+
  geom_signif(comparisons = list(c("Live", "Neutral")), annotations = "***", textsize = 8, map_signif_level = TRUE, y_position = 80)

ggboxplot(LearnedControlPanamaAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
           ylim = c(0, 115), title = "Learned Control Scent") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Left","Right", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=110, label= "SS: 1027.20 ; DF: 2,68 ; p = 0.23")
  

ggboxplot(LearnedDeadPanamaAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "grey80", ylim = c(0, 115), title = "Learned Dead Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Broth","Dead Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=110, label= "SS: 799.21 ; DF: 2,62 ; p = 0.42")
  

ggboxplot(LearnedLivePanamaAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "grey40", ylim = c(0, 115), title = "Learned Live Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Broth","Live Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=110, label= "SS: 1911.39 ; DF: 2,62 ; p = 0.03")

ggboxplot(AllControlFIUPanamaAnalysis, x = "Group", y = "Weight",  ylab = " Time (minutes)", xlab = "Location",
          color = "Type", ylim = c(0, 115), title = "All Control Scent") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Left","Right", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.8, y=110, label= "SS: 1039.94 ; DF: 2,182 ; p = 0.18")+
  scale_color_manual(values=c("black", "grey60"))

ggboxplot(AllDeadFIUPanamaAnalysis, x = "Group", y = "Weight", fill = "grey80", ylab = " Time (minutes)", xlab = "Location",
          color = "Type", ylim = c(0, 115), title = "All Dead Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Broth","Dead Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.8, y=110, label= "SS: 421.35 ; DF: 2,221 ; p = 0.43")+
  scale_color_manual(values=c("black", "grey60"))

ggboxplot(AllLiveFIUPanamaAnalysis, x = "Group", y = "Weight", fill = "grey40", ylab = " Time (minutes)", xlab = "Location",
          color = "Type", ylim = c(0, 115), title = "All Live Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Broth","Live Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.8, y=110, label= "SS: 605.7 ; DF: 2,215 ; p = 0.20")+
  scale_color_manual(values=c("black", "grey60"))

## Second try: Looking at glm stuff

## Weight distribution per data set
## Using seconds
## Naive Control Panama
ggdensity(NaiveControlPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "Naive Control Panama")
ggdensity(NaiveControlPanamaAnalysis$Total_Seconds_Fixed, main = "Density Plot", xlab = "Naive Control Panama Total")
ggqqplot(NaiveControlPanamaAnalysis$Seconds_Fixed)
ggqqplot(NaiveControlPanamaAnalysis$Total_Seconds_Fixed)
## Poisson distribution, skewed right for seconds and slightly skewed left for total seconds

## Naive Control Panama Square Root
ggdensity(NaiveControlPanamaAnalysis$Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Control Panama")
ggdensity(NaiveControlPanamaAnalysis$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Control Panama Total")
ggqqplot(NaiveControlPanamaAnalysis$Seconds_Fixed_SR)
ggqqplot(NaiveControlPanamaAnalysis$Total_Seconds_Fixed)
## moving to Gaussian

## Naive Control FIU Panama
ggdensity(NaiveControlFIUPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "Naive Control FIU Panama")
ggdensity(NaiveControlFIUPanamaAnalysis$Seconds_Total_Fixed, main = "Density Plot", xlab = "Naive Control FIU Panama Total")
ggqqplot(NaiveControlFIUPanamaAnalysis$Seconds_Fixed)
ggqqplot(NaiveControlFIUPanamaAnalysis$Seconds_Total_Fixed)
## Poisson distribution, skewed right for seconds and slightly skewed left for total seconds

## Naive Dead FIU Panama
ggdensity(NaiveDeadFIUPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "Naive Dead FIU Panama")
ggdensity(NaiveDeadFIUPanamaAnalysis$Total_Seconds_Fixed, main = "Density Plot", xlab = "Naive Dead FIU Panama Total")
ggqqplot(NaiveDeadFIUPanamaAnalysis$Seconds_Fixed)
ggqqplot(NaiveDeadFIUPanamaAnalysis$Total_Seconds_Fixed)
## Poisson distribution, skewed for both

## Naive Dead FIU Panama Square Root
ggdensity(NaiveDeadFIUPanamaAnalysis$Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Dead FIU Panama")
ggdensity(NaiveDeadFIUPanamaAnalysis$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Dead FIU Panama Total")
ggqqplot(NaiveDeadFIUPanamaAnalysis$Seconds_Fixed_SR)
ggqqplot(NaiveDeadFIUPanamaAnalysis$Total_Seconds_Fixed)
## moving to Gaussian

## Naive Live FIU Panama
ggdensity(NaiveLiveFIUPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "Naive Live FIU Panama")
ggdensity(NaiveLiveFIUPanamaAnalysis$Seconds_Total_Fixed, main = "Density Plot", xlab = "Naive Live FIU Panama Total")
ggqqplot(NaiveLiveFIUPanamaAnalysis$Seconds_Fixed)
ggqqplot(NaiveLiveFIUPanamaAnalysis$Seconds_Total_Fixed)
## Poisson distribution, skewed right for seconds and maybe normal for total

## Naive Live FIU Panama Square Root
ggdensity(NaiveLiveFIUPanamaAnalysis$Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Live FIU Panama")
ggdensity(NaiveLiveFIUPanamaAnalysis$Seconds_Total_Fixed_SR, main = "Density Plot", xlab = "Naive Live FIU Panama Total")
ggqqplot(NaiveLiveFIUPanamaAnalysis$Seconds_Fixed_SR)
ggqqplot(NaiveLiveFIUPanamaAnalysis$Seconds_Total_Fixed_SR)
## moving to Gaussian

## Learned Control Panama
ggdensity(LearnedControlPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "Learned Control Panama")
ggdensity(LearnedControlPanamaAnalysis$Total_Seconds_Fixed, main = "Density Plot", xlab = "Learned Control Panama Total")
ggqqplot(LearnedControlPanamaAnalysis$Seconds_Fixed)
ggqqplot(LearnedControlPanamaAnalysis$Total_Seconds_Fixed)
## Poisson distribution, skewed right for both

## Learned Control Panama Square Root
ggdensity(LearnedControlPanamaAnalysis$Seconds_Fixed_SR, main = "Density Plot", xlab = "Learned Control Panama")
ggdensity(LearnedControlPanamaAnalysis$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "Learned Control Panama Total")
ggqqplot(LearnedControlPanamaAnalysis$Seconds_Fixed_SR)
ggqqplot(LearnedControlPanamaAnalysis$Total_Seconds_Fixed_SR)

## Learned Dead Panama
ggdensity(LearnedDeadPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "Learned Dead Panama")
ggdensity(LearnedDeadPanamaAnalysis$Total_Seconds_Fixed, main = "Density Plot", xlab = "Learned Dead Panama Total")
ggqqplot(LearnedDeadPanamaAnalysis$Seconds_Fixed)
ggqqplot(LearnedDeadPanamaAnalysis$Total_Seconds_Fixed)
## Poisson distribution, skewed right for seconds and looks inverted normal for total

## Learned Live Panama
ggdensity(LearnedLivePanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "Learned Live Panama")
ggdensity(LearnedLivePanamaAnalysis$Total_Seconds_Fixed, main = "Density Plot", xlab = "Learned Live Panama Total")
ggqqplot(LearnedLivePanamaAnalysis$Seconds_Fixed)
ggqqplot(LearnedLivePanamaAnalysis$Total_Seconds_Fixed)
## Poisson distribution, skewed right for both

## All Control FIU Panama
ggdensity(AllControlFIUPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "All Control FIU Panama")
ggdensity(AllControlFIUPanamaAnalysis$Seconds_Total_Fixed, main = "Density Plot", xlab = "All Control FIU Panama Total")
ggqqplot(AllControlFIUPanamaAnalysis$Seconds_Fixed)
ggqqplot(AllControlFIUPanamaAnalysis$Seconds_Total_Fixed)
## Poisson distribution, skewed right for seconds but not really for total

## All Dead FIU Panama
ggdensity(AllDeadFIUPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "All Dead FIU Panama")
ggdensity(AllDeadFIUPanamaAnalysis$Total_Seconds_Fixed, main = "Density Plot", xlab = "All Dead FIU Panama Total")
ggqqplot(AllDeadFIUPanamaAnalysis$Seconds_Fixed)
ggqqplot(AllDeadFIUPanamaAnalysis$Total_Seconds_Fixed)
## Poisson distribution, skewed right for seconds but not really for total

## All Live FIU Panama
ggdensity(AllLiveFIUPanamaAnalysis$Seconds_Fixed, main = "Density Plot", xlab = "All Live FIU Panama")
ggdensity(AllLiveFIUPanamaAnalysis$Seconds_Total_Fixed, main = "Density Plot", xlab = "All Live FIU Panama Total")
ggqqplot(AllLiveFIUPanamaAnalysis$Seconds_Fixed)
ggqqplot(AllLiveFIUPanamaAnalysis$Seconds_Total_Fixed)
## Poisson distribution, skewed right for seconds but normal for total

## Install needed packages
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)

## GLMMs with seconds
## Naive Control Panama
## Using type II SS when no interaction, Type III when interaction effect
NaiveControlPanamaGLMM <- glmer(Seconds_Fixed~Group+Sex+Total_Seconds_Fixed+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveControlPanamaAnalysis, family = poisson)
summary(NaiveControlPanamaGLMM)
car::Anova(NaiveControlPanamaGLMM, type="2")
emmeans(NaiveControlPanamaGLMM, pairwise~Group)$contrasts
emmeans(NaiveControlPanamaGLMM, pairwise~Sex)$contrasts
residuals_plot_NaiveControlPanamaGLMM <- ggplot(data = NaiveControlPanamaAnalysis, aes(x = fitted(NaiveControlPanamaGLMM), y = resid(NaiveControlPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot)
## most models won't converge, and residuals are hetero so going to square root the count data

## Naive Control Panama Square Root Seconds
NaiveControlPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveControlPanamaAnalysis)
anova(NaiveControlPanamaGLMM)
summary(NaiveControlPanamaGLMM)
car::Anova(NaiveControlPanamaGLMM, type="2")
emmeans(NaiveControlPanamaGLMM, pairwise~Group)$contrasts
## Liquid amount may need to be a fixed effect
NaiveControlPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + (1|Trial.Order) , data = NaiveControlPanamaAnalysis)
anova(NaiveControlPanamaGLMM)
summary(NaiveControlPanamaGLMM)
## going to leave liquid amount as fixed but don't need trial order as random
NaiveControlPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount , data = NaiveControlPanamaAnalysis)
anova(NaiveControlPanamaGLMM)
summary(NaiveControlPanamaGLMM)
emmeans(NaiveControlPanamaGLMM, pairwise~Group)$contrasts
residuals_plot_NaiveControlPanamaGLMM <- ggplot(data = NaiveControlPanamaAnalysis, aes(x = fitted(NaiveControlPanamaGLMM), y = resid(NaiveControlPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot)
## one outlier making residuals slightly off
resid(NaiveControlPanamaGLMM)
fitted(NaiveControlPanamaGLMM)
## M3 seems to be an outlier on line 17 of original dataset, removing
NaiveControlPanamaAnalysisNoOutliers = read.csv(file.choose())
NaiveControlPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + (1|Trial.Order) , data = NaiveControlPanamaAnalysisNoOutliers)
anova(NaiveControlPanamaGLMM)
summary(NaiveControlPanamaGLMM)
## going to leave liquid amount as fixed but don't need trial order as random
NaiveControlPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount , data = NaiveControlPanamaAnalysisNoOutliers)
anova(NaiveControlPanamaGLMM)
summary(NaiveControlPanamaGLMM)
emmeans(NaiveControlPanamaGLMM, pairwise~Group,lmer.df = "satterthwaite")$contrasts
residuals_plot_NaiveControlPanamaGLMM <- ggplot(data = NaiveControlPanamaAnalysisNoOutliers, aes(x = fitted(NaiveControlPanamaGLMM), y = resid(NaiveControlPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_NaiveControlPanamaGLMM)
## better but now total trial time as a covariate is significant, see with Christian if that is eventually an issue

NaiveDeadFIUPanamaGLMM <- glmer(Seconds_Fixed~Group+Sex+Total_Seconds_Fixed+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveDeadFIUPanamaAnalysis, family = poisson)
residuals_plot_NaiveDeadFIUPanamaGLMM <- ggplot(data = NaiveDeadFIUPanamaAnalysis, aes(x = fitted(NaiveDeadFIUPanamaGLMM), y = resid(NaiveDeadFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_NaiveDeadFIUPanamaGLMM)
## residuals are hetero, going to square root
NaiveDeadFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveDeadFIUPanamaAnalysis)
anova(NaiveDeadFIUPanamaGLMM)
summary(NaiveDeadFIUPanamaGLMM)
## liquid and trial number don't seem to be amything, try as fixed
NaiveDeadFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order, data = NaiveDeadFIUPanamaAnalysis)
anova(NaiveDeadFIUPanamaGLMM)
summary(NaiveDeadFIUPanamaGLMM)
## removing liquid and trial number from model
NaiveDeadFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number), data = NaiveDeadFIUPanamaAnalysis)
anova(NaiveDeadFIUPanamaGLMM)
summary(NaiveDeadFIUPanamaGLMM)
emmeans(NaiveDeadFIUPanamaGLMM, pairwise~Group)$contrasts
residuals_plot_NaiveDeadFIUPanamaGLMM <- ggplot(data = NaiveDeadFIUPanamaAnalysis, aes(x = fitted(NaiveDeadFIUPanamaGLMM), y = resid(NaiveDeadFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_NaiveDeadFIUPanamaGLMM)
## data still hetero
boxplot(NaiveDeadFIUPanamaAnalysis$Seconds_Fixed_SR)
## Removing M3 C and F19 B and F8 B and M6 A 
NaiveDeadFIUPanamaAnalysisNoOutliers = read.csv(file.choose())
NaiveDeadFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number), data = NaiveDeadFIUPanamaAnalysisNoOutliers)
anova(NaiveDeadFIUPanamaGLMM)
summary(NaiveDeadFIUPanamaGLMM)
emmeans(NaiveDeadFIUPanamaGLMM, pairwise~Group,lmer.df = "satterthwaite")$contrasts
residuals_plot_NaiveDeadFIUPanamaGLMM <- ggplot(data = NaiveDeadFIUPanamaAnalysisNoOutliers, aes(x = fitted(NaiveDeadFIUPanamaGLMM), y = resid(NaiveDeadFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_NaiveDeadFIUPanamaGLMM)
fitted(NaiveDeadFIUPanamaGLMM)
resid(NaiveDeadFIUPanamaGLMM)
boxplot(NaiveDeadFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
boxplot(NaiveDeadFIUPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR)
## boxplot still has outlirs on top and bottom but won't remove just because frog decided not to stay in a quadrant at all during a trial
## still hetero, can ask for help from Christian
## Dead and neutral significantly different

NaiveLiveFIUPanamaGLMM <- glmer(Seconds_Fixed~Group+Sex+Seconds_Total_Fixed+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveLiveFIUPanamaAnalysis, family = poisson)
residuals_plot_NaiveLiveFIUPanamaGLMM <- ggplot(data = NaiveLiveFIUPanamaAnalysis, aes(x = fitted(NaiveLiveFIUPanamaGLMM), y = resid(NaiveLiveFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_NaiveLiveFIUPanamaGLMM)
## residuals are hetero, going to square root

## Naive Live FIU Panama Square Root
NaiveLiveFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Seconds_Total_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveLiveFIUPanamaAnalysis)
anova(NaiveLiveFIUPanamaGLMM)
summary(NaiveLiveFIUPanamaGLMM)
## no random effects are a thing, trying as fixed
NaiveLiveFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Seconds_Total_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = NaiveLiveFIUPanamaAnalysis)
anova(NaiveLiveFIUPanamaGLMM)
summary(NaiveLiveFIUPanamaGLMM)
## nothing as fixed, removing liquid and trial order from model
ggdensity(NaiveLiveFIUPanamaAnalysis$Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Live FIU Panama")
ggqqplot(NaiveLiveFIUPanamaAnalysis$Seconds_Fixed_SR)
NaiveLiveFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Seconds_Total_Fixed_SR+(1|Frog_Number), data = NaiveLiveFIUPanamaAnalysis)
anova(NaiveLiveFIUPanamaGLMM)
summary(NaiveLiveFIUPanamaGLMM)
emmeans(NaiveLiveFIUPanamaGLMM, pairwise~Group)$contrasts
residuals_plot_NaiveLiveFIUPanamaGLMM <- ggplot(data = NaiveLiveFIUPanamaAnalysis, aes(x = fitted(NaiveLiveFIUPanamaGLMM), y = resid(NaiveLiveFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_NaiveLiveFIUPanamaGLMM)
## still hetero residuals plot, removing some outliers
fitted(NaiveLiveFIUPanamaGLMM)
## Removing F19 C and F8 C and Kevin C and Randy A
NaiveLiveFIUPanamaAnalysisNoOutliers = read.csv(file.choose())
NaiveLiveFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Seconds_Total_Fixed_SR+(1|Frog_Number), data = NaiveLiveFIUPanamaAnalysisNoOutliers)
anova(NaiveLiveFIUPanamaGLMM)
summary(NaiveLiveFIUPanamaGLMM)
emmeans(NaiveLiveFIUPanamaGLMM, pairwise~Group,lmer.df = "satterthwaite")$contrasts
residuals_plot_NaiveLiveFIUPanamaGLMM <- ggplot(data = NaiveLiveFIUPanamaAnalysisNoOutliers, aes(x = fitted(NaiveLiveFIUPanamaGLMM), y = resid(NaiveLiveFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_NaiveLiveFIUPanamaGLMM)
boxplot(NaiveLiveFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
## still hetero

## Learned Control Square Root No Outliers
## Still not normal and clear outliers, removing M8 C and M5 C and F25 C and M5 B and F25 B and F2 B and M20C
LearnedControlPanamaAnalysisNoOutliers = read.csv(file.choose())
ggdensity(LearnedControlPanamaAnalysisNoOutliers$Seconds_Fixed_SR, main = "Density Plot", xlab = "Learned Control Panama")
ggdensity(LearnedControlPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "Learned Control Panama Total")
ggqqplot(LearnedControlPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
ggqqplot(LearnedControlPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR)

LearnedControlPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedControlPanamaAnalysisNoOutliers)
anova(LearnedControlPanamaGLMM)
summary(LearnedControlPanamaGLMM)
## no random effects are a thing, trying as fixed
LearnedControlPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = LearnedControlPanamaAnalysisNoOutliers)
anova(LearnedControlPanamaGLMM)
summary(LearnedControlPanamaGLMM)
## do nothing as fixed either, removing liquid and trial order from model
LearnedControlPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number), data = LearnedControlPanamaAnalysisNoOutliers)
anova(LearnedControlPanamaGLMM)
summary(LearnedControlPanamaGLMM)
emmeans(LearnedControlPanamaGLMM, pairwise~Group,lmer.df = "satterthwaite")$contrasts
residuals_plot_LearnedControlPanamaGLMM <- ggplot(data = LearnedControlPanamaAnalysisNoOutliers, aes(x = fitted(LearnedControlPanamaGLMM), y = resid(LearnedControlPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_LearnedControlPanamaGLMM)
## no outliers but still hetero

## Learned Dead Panama Square Root No Outliers
## Removing outliers M20 A and F10 B and F10 C and F2 A and M8 A and F2 C and F25 C
LearnedDeadPanamaAnalysisNoOutliers = read.csv(file.choose())
ggdensity(LearnedDeadPanamaAnalysisNoOutliers$Seconds_Fixed_SR, main = "Density Plot", xlab = "Learned Dead Panama")
ggdensity(LearnedDeadPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "Learned Dead Panama Total")
ggqqplot(LearnedDeadPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
ggqqplot(LearnedDeadPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR)

LearnedDeadPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedDeadPanamaAnalysisNoOutliers)
anova(LearnedDeadPanamaGLMM)
summary(LearnedDeadPanamaGLMM)
## no random effects are a thing, trying as fixed
LearnedDeadPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = LearnedDeadPanamaAnalysisNoOutliers)
anova(LearnedDeadPanamaGLMM)
summary(LearnedDeadPanamaGLMM)
## do nothing as fixed either, removing liquid and trial order from model
LearnedDeadPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number), data = LearnedDeadPanamaAnalysisNoOutliers)
anova(LearnedDeadPanamaGLMM)
summary(LearnedDeadPanamaGLMM)
emmeans(LearnedDeadPanamaGLMM, pairwise~Group,lmer.df = "satterthwaite")$contrasts
residuals_plot_LearnedDeadPanamaGLMM <- ggplot(data = LearnedDeadPanamaAnalysisNoOutliers, aes(x = fitted(LearnedDeadPanamaGLMM), y = resid(LearnedDeadPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_LearnedDeadPanamaGLMM)
boxplot(LearnedDeadPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
## still hetero

## Learned Live Panama Square Root
## Removing outliers F10 A and F10 C and F25 A and M5 A and M8 B and F25 C 
LearnedLivePanamaAnalysisNoOutliers = read.csv(file.choose())
ggdensity(LearnedLivePanamaAnalysisNoOutliers$Seconds_Fixed_SR, main = "Density Plot", xlab = "Learned Live Panama")
ggdensity(LearnedLivePanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "Learned Live Panama Total")
ggqqplot(LearnedLivePanamaAnalysisNoOutliers$Seconds_Fixed_SR)
ggqqplot(LearnedLivePanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR)

LearnedLivePanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedLivePanamaAnalysisNoOutliers)
anova(LearnedLivePanamaGLMM)
summary(LearnedLivePanamaGLMM)
## no random effects are a thing, trying as fixed
LearnedLivePanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = LearnedLivePanamaAnalysisNoOutliers)
anova(LearnedLivePanamaGLMM)
summary(LearnedLivePanamaGLMM)
## do nothing as fixed either, removing liquid and trial order from model
LearnedLivePanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) , data = LearnedLivePanamaAnalysisNoOutliers)
anova(LearnedLivePanamaGLMM)
summary(LearnedLivePanamaGLMM)
emmeans(LearnedLivePanamaGLMM, pairwise~Group,lmer.df = "satterthwaite")$contrasts
residuals_plot_LearnedLivePanamaGLMM <- ggplot(data = LearnedLivePanamaAnalysisNoOutliers, aes(x = fitted(LearnedLivePanamaGLMM), y = resid(LearnedLivePanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_LearnedLivePanamaGLMM)
fitted(LearnedLivePanamaGLMM)
resid(LearnedLivePanamaGLMM)
## looks like F25 B is an outlier but not frog move around during trial so actually not
## still hetero

## Naive Control FIU Panama Square Root
NaiveControlFIUPanamaAnalysisNoOutliers = read.csv(file.choose())
ggdensity(NaiveControlFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Control FIU Panama")
ggdensity(NaiveControlFIUPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Control FIU Panama Total")
ggqqplot(NaiveControlFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
ggqqplot(NaiveControlFIUPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR)

NaiveControlFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveControlFIUPanamaAnalysisNoOutliers)
anova(NaiveControlFIUPanamaGLMM)
summary(NaiveControlFIUPanamaGLMM)
## no random effects are a thing, trying as fixed
NaiveControlFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = NaiveControlFIUPanamaAnalysisNoOutliers)
anova(NaiveControlFIUPanamaGLMM)
summary(NaiveControlFIUPanamaGLMM)
## do nothing as fixed either, removing liquid and trial order from model
NaiveControlFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) , data = NaiveControlFIUPanamaAnalysisNoOutliers)
anova(NaiveControlFIUPanamaGLMM)
summary(NaiveControlFIUPanamaGLMM)
emmeans(NaiveControlFIUPanamaGLMM, pairwise~Group,lmer.df = "satterthwaite")$contrasts
residuals_plot_NaiveControlFIUPanamaGLMM <- ggplot(data = NaiveControlFIUPanamaAnalysisNoOutliers, aes(x = fitted(NaiveControlFIUPanamaGLMM), y = resid(NaiveControlFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_NaiveControlFIUPanamaGLMM)
## slightly hetero perhaps but not bad

## All Control Square Root No Outliers
AllControlFIUPanamaAnalysisNoOutliers = read.csv(file.choose())
ggdensity(AllControlFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Control FIU Panama")
ggdensity(AllControlFIUPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "Naive Control FIU Panama Total")
ggqqplot(AllControlFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
ggqqplot(AllControlFIUPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR)

AllControlFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = AllControlFIUPanamaAnalysisNoOutliers)
anova(AllControlFIUPanamaGLMM)
summary(AllControlFIUPanamaGLMM)
## no random effects are a thing, trying as fixed
AllControlFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = AllControlFIUPanamaAnalysisNoOutliers)
anova(AllControlFIUPanamaGLMM)
summary(AllControlFIUPanamaGLMM)
## do nothing as fixed either, removing liquid and trial order from model
AllControlFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number), data = AllControlFIUPanamaAnalysisNoOutliers)
anova(AllControlFIUPanamaGLMM)
summary(AllControlFIUPanamaGLMM)
emmeans(AllControlFIUPanamaGLMM, pairwise~Group*Type,lmer.df = "satterthwaite")$contrasts
residuals_plot_AllControlFIUPanamaGLMM<- ggplot(data = AllControlFIUPanamaAnalysisNoOutliers, aes(x = fitted(AllControlFIUPanamaGLMM), y = resid(AllControlFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_AllControlFIUPanamaGLMM)
## not really hetero

## All Dead FIU Panama Square Root No Outliers
AllDeadFIUPanamaAnalysisNoOutliers = read.csv(file.choose())
ggdensity(AllDeadFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR, main = "Density Plot", xlab = "All Dead FIU Panama")
ggdensity(AllDeadFIUPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR, main = "Density Plot", xlab = "All Dead FIU Panama Total")
ggqqplot(AllDeadFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
ggqqplot(AllDeadFIUPanamaAnalysisNoOutliers$Total_Seconds_Fixed_SR)

AllDeadFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = AllDeadFIUPanamaAnalysisNoOutliers)
anova(AllDeadFIUPanamaGLMM)
summary(AllDeadFIUPanamaGLMM)
## no random effects are a thing, trying as fixed
AllDeadFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = AllDeadFIUPanamaAnalysisNoOutliers)
anova(AllDeadFIUPanamaGLMM)
summary(AllDeadFIUPanamaGLMM)
## do nothing as fixed either, removing liquid and trial order from model
AllDeadFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Total_Seconds_Fixed_SR+(1|Frog_Number), data = AllDeadFIUPanamaAnalysisNoOutliers)
anova(AllDeadFIUPanamaGLMM)
summary(AllDeadFIUPanamaGLMM)
emmeans(AllDeadFIUPanamaGLMM, pairwise~Group*Type,lmer.df = "satterthwaite")$contrasts
## only significance is Dead Naive Neutral Naive which we already knew
residuals_plot_AllDeadFIUPanamaGLMM<- ggplot(data = AllDeadFIUPanamaAnalysisNoOutliers, aes(x = fitted(AllDeadFIUPanamaGLMM), y = resid(AllDeadFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_AllDeadFIUPanamaGLMM)
## still hetero

## All Live FIU Panama Square Root No Outliers
AllLiveFIUPanamaAnalysisNoOutliers = read.csv(file.choose())
ggdensity(AllLiveFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR, main = "Density Plot", xlab = "All Live FIU Panama")
ggdensity(AllLiveFIUPanamaAnalysisNoOutliers$Seconds_Total_Fixed_SR, main = "Density Plot", xlab = "All Dead Live Panama Total")
ggqqplot(AllLiveFIUPanamaAnalysisNoOutliers$Seconds_Fixed_SR)
ggqqplot(AllLiveFIUPanamaAnalysisNoOutliers$Seconds_Total_Fixed_SR)

AllLiveFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Seconds_Total_Fixed_SR+(1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = AllLiveFIUPanamaAnalysisNoOutliers)
anova(AllLiveFIUPanamaGLMM)
summary(AllLiveFIUPanamaGLMM)
## no random effects are a thing, trying as fixed
AllLiveFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Seconds_Total_Fixed_SR+(1|Frog_Number) + Liquid.Amount + Trial.Order , data = AllLiveFIUPanamaAnalysisNoOutliers)
anova(AllLiveFIUPanamaGLMM)
summary(AllLiveFIUPanamaGLMM)
## do nothing as fixed either, removing liquid and trial order from model
AllLiveFIUPanamaGLMM <- lmer(Seconds_Fixed_SR~Group*Type+Sex+Seconds_Total_Fixed_SR+(1|Frog_Number), data = AllLiveFIUPanamaAnalysisNoOutliers)
anova(AllLiveFIUPanamaGLMM)
summary(AllLiveFIUPanamaGLMM)
emmeans(AllLiveFIUPanamaGLMM, pairwise~Group*Type,lmer.df = "satterthwaite")$contrasts
## nothing significant between types
residuals_plot_AllLiveFIUPanamaGLMM<- ggplot(data = AllLiveFIUPanamaAnalysisNoOutliers, aes(x = fitted(AllLiveFIUPanamaGLMM), y = resid(AllLiveFIUPanamaGLMM))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
print(residuals_plot_AllLiveFIUPanamaGLMM)
## still hetero

## Boxplots
ggboxplot(NaiveControlFIUPanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR", ylab = " Time (square root seconds)", xlab = "Location",
          ylim = c(0, 100), color = "grey60", title = "Naive Control Scent") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Left","Right", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=95, label= "SS: 595.95 ; DF: 2,133 ; p = 0.24")+
  annotate("text", x=2.8, y=100, label= "Location")

ggboxplot(NaiveDeadFIUPanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR", ylab = " Time (square root seconds)", xlab = "Location",
          fill = "grey80", color = "grey60", ylim = c(0, 100), title = "Naive Dead Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Broth","Dead Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=95, label= "SS: 1253.19 ; DF: 2,145 ; p < 0.01")+
  geom_signif(comparisons = list(c("Con", "Neutral")), annotations = "**", textsize = 8, map_signif_level = TRUE, y_position = 70)+
  annotate("text", x=2.8, y=100, label= "Location")
  

ggboxplot(NaiveLiveFIUPanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR", ylab = " Time (square root seconds)", xlab = "Location",
          fill = "grey40", color = "grey60", ylim = c(0, 100), title = "Naive Live Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Control","Live Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=95, label= "SS: 5442.2 ; DF: 2,139 ; p <0.0001")+
  geom_signif(comparisons = list(c("Con", "Neutral")), annotations = "****", textsize = 8, map_signif_level = TRUE, y_position = 80)+
  geom_signif(comparisons = list(c("Live", "Neutral")), annotations = "****", textsize = 8, map_signif_level = TRUE, y_position = 70)+
  annotate("text", x=2.8, y=100, label= "Location")
  

ggboxplot(LearnedControlPanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR", ylab = " Time (square root seconds)", xlab = "Location",
          ylim = c(0, 100), title = "Learned Control Scent") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Left","Right", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=95, label= "SS: 34.097 ; DF: 2,46 ; p = 0.94")+
  annotate("text", x=2.8, y=100, label= "Location")

ggboxplot(LearnedDeadPanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR", ylab = " Time (square root seconds)", xlab = "Location",
          fill = "grey80", ylim = c(0, 100), title = "Learned Dead Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Broth","Dead Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=95, label= "SS: 532.81 ; DF: 2,40 ; p = 0.39")+
  annotate("text", x=2.8, y=100, label= "Location")

ggboxplot(LearnedLivePanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR", ylab = " Time (square root seconds)", xlab = "Location",
          fill = "grey40", ylim = c(0, 100), title = "Learned Live Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Broth","Live Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.85, y=95, label= "SS: 591.19 ; DF: 2,43 ; p = 0.29")+
  annotate("text", x=2.8, y=100, label= "Location")

ggboxplot(AllControlFIUPanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR",  ylab = " Time (square root seconds)", xlab = "Location",
          color = "Type", ylim = c(0, 80), title = "All Control Scent") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Left","Right", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.8, y=75, label= "SS: 165.96 ; DF: 2,181 ; p = 0.69")+
  annotate("text", x=2.8, y=80, label= "Interaction Location/Type")+
  scale_color_manual(values=c("black", "grey60"))

ggboxplot(AllDeadFIUPanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR", fill = "grey80", ylab = " Time (square root seconds)", xlab = "Location",
          color = "Type", ylim = c(0, 80), title = "All Dead Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Broth","Dead Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.8, y=75, label= "SS: 18.90 ; DF: 2,187 ; p = 0.94")+
  annotate("text", x=2.8, y=80, label= "Interaction Location/Type")+
  scale_color_manual(values=c("black", "grey60"))

ggboxplot(AllLiveFIUPanamaAnalysisNoOutliers, x = "Group", y = "Seconds_Fixed_SR", fill = "grey40", ylab = " Time (square root seconds)", xlab = "Location",
          color = "Type", ylim = c(0, 80), title = "All Live Bd Scent") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Broth","Live Bd", "Neutral"))+
  scale_y_continuous(breaks=seq(0,80,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=2.8, y=75, label= "SS: 480.42 ; DF: 2,184 ; p = 0.19")+
  scale_color_manual(values=c("black", "grey60"))+
  annotate("text", x=2.8, y=80, label= "Interaction Location/Type")
