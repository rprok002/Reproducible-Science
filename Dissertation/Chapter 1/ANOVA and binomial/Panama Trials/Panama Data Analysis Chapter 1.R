## Data Analysis Panama Chapter 1
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
library(multcomp)

## Naive Control
NaiveControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveControlAnalysis)
summary(NaiveControlGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlGLM)
## contrasts
emmeans(NaiveControlGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts

## trial order and liquid amount don't do anything as random factors, trying as predictors
NaiveControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount+ Trial.Order, data = NaiveControlAnalysis)
summary(NaiveControlGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
NaiveControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number), data = NaiveControlAnalysis)
summary(NaiveControlGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveControlGLM)
## contrasts
emmeans(NaiveControlGLM, pairwise~Group, lmer.df = "satterthwaite")$contrasts

## going to stick with emmeans, seems when there are only one comparison possibility then should use fixed
## effects instead of doing the emmeans, it isn't necessary to go deeper
## no variation in frog number, liquid amount or trial order 
## 0.05 for group, so just not significant

## Naive Dead
NaiveDeadGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveDeadAnalysis)
summary(NaiveDeadGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveDeadGLM)
emmeans(NaiveDeadGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
NaiveDeadGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order , data = NaiveDeadAnalysis)
summary(NaiveDeadGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
NaiveDeadGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) , data = NaiveDeadAnalysis)
summary(NaiveDeadGLM)
## Type III Analysis of Variance with Satterthwaite
anova(NaiveDeadGLM)
emmeans(NaiveDeadGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## more in dead than control

## Naive Live
NaiveLiveGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = NaiveLiveAnalysis)
summary(NaiveLiveGLM)
anova(NaiveLiveGLM)
emmeans(NaiveLiveGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
NaiveLiveGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order, data = NaiveLiveAnalysis)
summary(NaiveLiveGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
NaiveLiveGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number), data = NaiveLiveAnalysis)
summary(NaiveLiveGLM)
anova(NaiveLiveGLM)
emmeans(NaiveLiveGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Learned Control
LearnedControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedControlAnalysis)
summary(LearnedControlGLM)
anova(LearnedControlGLM)
car::Anova(LearnedControlGLM, type="3")
emmeans(LearnedControlGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
LearnedControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order , data = LearnedControlAnalysis)
summary(LearnedControlGLM)

## trial order and liquid affect (ish) as predictors, keeping in model
LearnedControlGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order , data = LearnedControlAnalysis)
summary(LearnedControlGLM)
anova(LearnedControlGLM)
emmeans(LearnedControlGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Learned Dead
LearnedDeadGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedDeadAnalysis)
summary(LearnedDeadGLM)
anova(LearnedDeadGLM)
car::Anova(LearnedDeadGLM, type="3")
emmeans(LearnedDeadGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
LearnedDeadGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order, data = LearnedDeadAnalysis)
summary(LearnedDeadGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
LearnedDeadGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) , data = LearnedDeadAnalysis)
summary(LearnedDeadGLM)
anova(LearnedDeadGLM)
car::Anova(LearnedDeadGLM, type="3")
emmeans(LearnedDeadGLM, list (pairwise~Group), lmer.df = "satterthwaite")
## no significance

## Learned Live
LearnedLiveGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order) , data = LearnedLiveAnalysis)
summary(LearnedLiveGLM)
anova(LearnedLiveGLM)
car::Anova(LearnedLiveGLM, type="3")
emmeans(LearnedLiveGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
LearnedLiveGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order , data = LearnedLiveAnalysis)
summary(LearnedLiveGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
LearnedLiveGLM <- lmer(Weight~Group+Total.Trial.Time+ (1|Frog_Number), data = LearnedLiveAnalysis)
summary(LearnedLiveGLM)
anova(LearnedLiveGLM)
car::Anova(LearnedLiveGLM, type="3")
emmeans(LearnedLiveGLM, list (pairwise~Group), lmer.df = "satterthwaite")

## Control
AllControlAnalysis <- read.csv(file.choose())
AllControlGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order), data = AllControlAnalysis)
summary(AllControlGLM)
anova(AllControlGLM)

## Group:Type is 0.05 when rounding and no emmeans are significant to calling it nonsignificant, also emmeans that is the most significant
## is between two levels that aren't important
car::Anova(AllControlGLM, type="3")
emmeans(AllControlGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
AllControlGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order, data = AllControlAnalysis)
summary(AllControlGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
AllControlGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number), data = AllControlAnalysis)
summary(AllControlGLM)
anova(AllControlGLM)
emmeans(AllControlGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
emmeans(AllControlGLM, list (pairwise~Group), lmer.df = "satterthwaite")



## significance driven by difference between ConA and ConC of naive frogs, no
## sig diff between groups


## Dead
AllDeadAnalysis <- read.csv(file.choose())
AllDeadGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order), data = AllDeadAnalysis)
summary(AllDeadGLM)
anova(AllDeadGLM)
car::Anova(AllDeadGLM, type="3")
emmeans(AllDeadGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")

## Time of Learned in Con compared to time of Naive in Dead is sig diff, but not relavent to data

## trial order and liquid amount don't do anything as random factors, trying as predictors
AllDeadGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order, data = AllDeadAnalysis)
summary(AllDeadGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
AllDeadGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number), data = AllDeadAnalysis)
summary(AllDeadGLM)
anova(AllDeadGLM)
emmeans(AllDeadGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")


## Live
AllLiveAnalysis <- read.csv(file.choose())
AllLiveGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + (1|Liquid.Amount) + (1|Trial.Order), data = AllLiveAnalysis)
summary(AllLiveGLM)
anova(AllLiveGLM)
car::Anova(AllLiveGLM, type="3")
emmeans(AllLiveGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")

## trial order and liquid amount don't do anything as random factors, trying as predictors
AllLiveGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number) + Liquid.Amount + Trial.Order, data = AllLiveAnalysis)
summary(AllLiveGLM)

## trial order and liquid amount also don't do anything as predictors, removing from model
AllLiveGLM <- lmer(Weight~Group*Type+Total.Trial.Time+ (1|Frog_Number), data = AllLiveAnalysis)
summary(AllLiveGLM)
anova(AllLiveGLM)
emmeans(AllLiveGLM, list (pairwise~Group*Type), lmer.df = "satterthwaite")
emmeans(AllLiveGLM, list (pairwise~Group), lmer.df = "satterthwaite")


## Boxplot
library(ggplot2)
library(multcompView)
library(dplyr)
library(graphics)
library(ggsignif)

ggboxplot(NaiveControlAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "grey80", ylim = c(0, 95), title = "Naive Control") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Side A","Side C", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 1967.67 ; DF: 2,41 ; p = 0.0479")

ggboxplot(NaiveDeadAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "deepskyblue1", ylim = c(0, 95), title = "Naive Dead") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Control","Dead", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 1608.98 ; DF: 2,29 ; p = 0.0515")+
  geom_signif(comparisons = list(c("Con", "Dead")), annotations = "*", textsize = 8, map_signif_level = TRUE, y_position = 88)

ggboxplot(NaiveLiveAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "darkviolet", ylim = c(0, 95), title = "Naive Live") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Control","Live", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 1300.4 ; DF: 2,17 ; p = 0.03")+
  geom_signif(comparisons = list(c("Con", "Neutral")), annotations = "*", textsize = 8, map_signif_level = TRUE, y_position = 88)

ggboxplot(LearnedControlAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "grey80", ylim = c(0, 95), title = "Learned Control") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Side A","Side C", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 1172.85 ; DF: 2,30 ; p = 0.02")+
  geom_signif(comparisons = list(c("ConA", "Neutral")), annotations = "*", textsize = 8, map_signif_level = TRUE, y_position = 88)

ggboxplot(LearnedDeadAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "deepskyblue1", ylim = c(0, 95), title = "Learned Dead") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Control","Dead", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 1094.25 ; DF: 2,26 ; p = 0.14")

ggboxplot(LearnedLiveAnalysis, x = "Group", y = "Weight", ylab = " Time (minutes)", xlab = "Location",
          fill = "darkviolet", ylim = c(0, 95), title = "Learned Live") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Control","Live", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 449.25 ; DF: 2,26 ; p = 0.28")

ggboxplot(AllControlAnalysis, x = "Group", y = "Weight", fill = "grey80", ylab = " Time (minutes)", xlab = "Location",
          color = "Type", ylim = c(0, 95), title = "All Control") + 
  scale_x_discrete(breaks=c("ConA","ConC","Neutral"), labels=c("Side A","Side C", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 1438.33 ; DF: 2,74 ; p = 0.0453")+
  scale_color_manual(values=c("black", "grey60"))

ggboxplot(AllDeadAnalysis, x = "Group", y = "Weight", fill = "deepskyblue1", ylab = " Time (minutes)", xlab = "Location",
          color = "Type", ylim = c(0, 95), title = "All Dead") + 
  scale_x_discrete(breaks=c("Con","Dead","Neutral"), labels=c("Control","Dead", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 2310.96 ; DF: 2,56 ; p = 0.01")+
  scale_color_manual(values=c("black", "grey60"))

ggboxplot(AllLiveAnalysis, x = "Group", y = "Weight", fill = "darkviolet", ylab = " Time (minutes)", xlab = "Location",
          color = "Type", ylim = c(0, 95), title = "All Live") + 
  scale_x_discrete(breaks=c("Con","Live","Neutral"), labels=c("Control","Live", "Neutral"))+
  scale_y_continuous(breaks=seq(0,95,by=10))+
  theme(plot.title=element_text(hjust=0.5))+
  annotate("text", x=3, y=65, label= "SS: 241.50 ; DF: 2,44 ; p = 0.47")+
  scale_color_manual(values=c("black", "grey60"))
  
