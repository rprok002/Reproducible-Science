## Data Analysis Dead Naive Bd Volatiles
DeadBdCon= read.csv(file.choose())
DeadBdExp= read.csv(file.choose())
DeadBdside = read.csv(file.choose())

##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

## Control
ggdensity(DeadBdCon$A.Standardized.Average, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(DeadBdCon$A.Standardized.Average)
ggdensity(DeadBdCon$C.Standardized.Average, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(DeadBdCon$C.Standardized.Average)
ggdensity(DeadBdCon$Neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(DeadBdCon$Neutral)
ggdensity(DeadBdCon$Total, main = "Density Plot of Average Total", xlab = "Average Total")
ggqqplot(DeadBdCon$Total)

## Experiment
ggdensity(DeadBdExp$Control.Average, main = "Density Plot of Average Control", xlab = "Average Control")
ggqqplot(DeadBdExp$Control.Average)
ggdensity(DeadBdExp$Experiment.Average, main = "Density Plot of Average Experiment", xlab = "Average Experiment")
ggqqplot(DeadBdExp$Experiment.Average)
ggdensity(DeadBdExp$Neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(DeadBdExp$Neutral)
ggdensity(DeadBdExp$Total.Average, main = "Density Plot of Average Total", xlab = "Average Total")
ggqqplot(DeadBdExp$Total.Average)

## Sides
ggdensity(DeadBdside$Standardized.A.Average, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(DeadBdside$Standardized.A.Average)
ggdensity(DeadBdside$Standardized.C.Average, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(DeadBdside$Standardized.C.Average)
ggdensity(DeadBdside$Neutral.Average, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(DeadBdside$Neutral.Average)
ggdensity(DeadBdside$Average.Total, main = "Density Plot of Average Total", xlab = "Average Total")
ggqqplot(DeadBdside$Average.Total)

## Shapiro-Wilks test
## Null: normal: Alt: not normal
shapiro.test(DeadBdCon$A.Standardized.Average)
## normal
shapiro.test(DeadBdCon$C.Standardized.Average)
## not normal
shapiro.test(DeadBdCon$Neutral)
## normal
shapiro.test(DeadBdCon$Total)
## normal
shapiro.test(DeadBdExp$Control.Average)
## normal
shapiro.test(DeadBdExp$Experiment.Average)
## normal
shapiro.test(DeadBdExp$Neutral)
## normal
shapiro.test(DeadBdExp$Total.Average)
## normal
shapiro.test(DeadBdside$Standardized.A.Average)
## normal
shapiro.test(DeadBdside$Standardized.C.Average)
## not normal
shapiro.test(DeadBdside$Neutral.Average)
## normal
shapiro.test(DeadBdside$Average.Total)
## normal

## Levene's test for equal variance
## Null: all pop var are equal; Alt: at least two are different
DeadBdConvar = read.csv(file.choose())
DeadbdExpvar = read.csv(file.choose())
DeadBdsidesvar = read.csv(file.choose())
library(car)
Controlvar = leveneTest(Weight~Group, DeadBdConvar)
Controlvar
## equal variance
Experimentvar = leveneTest(Weight~Group, DeadbdExpvar)
Experimentvar
## equal variance
##Normal distribution and equal variance, can proceed with normal ANOVA
Sidesvar = leveneTest(Weight~Group, DeadBdsidesvar)
Sidesvar
## equal variance

## need to keep nonnormality above in mind for further tests

## Data Analysis Live Naive Bd Volatiles
LiveBdCon= read.csv(file.choose())
LiveBdExp= read.csv(file.choose())
LiveBdside = read.csv(file.choose())

##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

## Control
ggdensity(LiveBdCon$Average.A, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(LiveBdCon$Average.A)
ggdensity(LiveBdCon$Average.C, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(LiveBdCon$Average.C)
ggdensity(LiveBdCon$Average.Neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(LiveBdCon$Average.Neutral)
ggdensity(LiveBdCon$Total, main = "Density Plot of Total", xlab = "Average Total")
ggqqplot(LiveBdCon$Total)

## Experiment
ggdensity(LiveBdExp$Average.Control, main = "Density Plot of Average Control", xlab = "Average Control")
ggqqplot(LiveBdExp$Average.Control)
ggdensity(LiveBdExp$Average.Experiment, main = "Density Plot of Average Experiment", xlab = "Average Experiment")
ggqqplot(LiveBdExp$Average.Experiment)
ggdensity(LiveBdExp$Average.Neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(LiveBdExp$Average.Neutral)
ggdensity(LiveBdExp$Total, main = "Density Plot of Average Total", xlab = "Average Total")
ggqqplot(LiveBdExp$Total)

## Sides
ggdensity(LiveBdside$Average.A, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(LiveBdside$Average.A)
ggdensity(LiveBdside$Average.C, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(LiveBdside$Average.C)
ggdensity(LiveBdside$Average.Neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(LiveBdside$Average.Neutral)
ggdensity(LiveBdside$Total, main = "Density Plot of Average Total", xlab = "Average Total")
ggqqplot(LiveBdside$Total)

## Shapiro-Wilks test
## Null: normal: Alt: not normal
shapiro.test(LiveBdCon$Average.A)
## normal
shapiro.test(LiveBdCon$Average.C)
## normal 
shapiro.test(LiveBdCon$Average.Neutral)
## normal
shapiro.test(LiveBdCon$Total)
## normal
shapiro.test(LiveBdExp$Average.Control)
## normal
shapiro.test(LiveBdExp$Average.Experiment)
## normal
shapiro.test(LiveBdExp$Average.Neutral)
## normal
shapiro.test(LiveBdExp$Total)
## normal
## Can assume normality for both control and experiment pops
shapiro.test(LiveBdside$Average.A)
## normal
shapiro.test(LiveBdside$Average.C)
## normal
shapiro.test(LiveBdside$Average.Neutral)
## normal
shapiro.test(LiveBdside$Total)
## normal

## can assume normality for all of these

## Levene's test for equal variance
## Null: all pop var are equal; Alt: at least two are different
LiveBdConvar = read.csv(file.choose())
LiveBdExpvar = read.csv(file.choose())
LiveBdSidevar = read.csv(file.choose())
library(car)
Controlvar = leveneTest(Weight~Group, LiveBdConvar)
Controlvar
## equal variance
Experimentvar = leveneTest(Weight~Group, LiveBdExpvar)
Experimentvar
## equal variance
##Normal distribution and equal variance, can proceed with normal ANOVA
Sidesvar = leveneTest(Weight~Group, LiveBdSidevar)
Sidesvar

## GLM for all naive frogs
install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)

require(lmerTest)
require(lme4)
AllNaiveside <- read.csv(file.choose())
AllNaiveExp <- read.csv(file.choose())
AllNaiveCon <- read.csv(file.choose())

## Side
ggdensity(AllNaiveside$Average.A, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(AllNaiveside$Average.A)
ggdensity(AllNaiveside$Average.C, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(AllNaiveside$Average.C)
ggdensity(AllNaiveside$Average.Neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(AllNaiveside$Average.Neutral)
ggdensity(AllNaiveside$Average.Total, main = "Density Plot of Average Total", xlab = "Average Total")
ggqqplot(AllNaiveside$Average.Total)

## Control
ggdensity(AllNaiveCon$Average.A, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(AllNaiveCon$Average.A)
ggdensity(AllNaiveCon$Average.C, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(AllNaiveCon$Average.C)
ggdensity(AllNaiveCon$Average.Neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(AllNaiveCon$Average.Neutral)
ggdensity(AllNaiveCon$Average.Total, main = "Density Plot of Average Total", xlab = "Average Total")
ggqqplot(AllNaiveCon$Average.Total)

## Experiment
ggdensity(AllNaiveExp$Control.Average, main = "Density Plot of Control", xlab = "Average Control")
ggqqplot(AllNaiveExp$Control.Average)
ggdensity(AllNaiveExp$Experiment.Average, main = "Density Plot of Experiment", xlab = "Average Experiment")
ggqqplot(AllNaiveExp$Experiment.Average)
ggdensity(AllNaiveExp$Neutral.Average, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(AllNaiveExp$Neutral.Average)
ggdensity(AllNaiveExp$Total.Average, main = "Density Plot of Average Total", xlab = "Average Total")
ggqqplot(AllNaiveExp$Total.Average)

## Shapiro-Wilks test
## Null: normal: Alt: not normal
shapiro.test(AllNaiveCon$Average.A)
## normal
shapiro.test(AllNaiveCon$Average.C)
## normal 
shapiro.test(AllNaiveCon$Average.Neutral)
## normal
shapiro.test(AllNaiveCon$Average.Total)
## normal
shapiro.test(AllNaiveExp$Control.Average)
## normal
shapiro.test(AllNaiveExp$Experiment.Average)
## normal
shapiro.test(AllNaiveExp$Neutral.Average)
## normal
shapiro.test(AllNaiveExp$Total.Average)
## normal
## Can assume normality for both control and experiment pops
shapiro.test(AllNaiveside$Average.A)
## normal
shapiro.test(AllNaiveside$Average.C)
## not normal
shapiro.test(AllNaiveside$Average.Neutral)
## normal
shapiro.test(AllNaiveside$Average.Total)
## normal

## combined data only not normal is side C for total sides data, should keep in mind

## Levene's test for equal variance
## Null: all pop var are equal; Alt: at least two are different
AllBdConvar = read.csv(file.choose())
AllBdExpvar = read.csv(file.choose())
AllBdSidevar = read.csv(file.choose())
library(car)
AllControlvar = leveneTest(Weight~Group, AllBdConvar)
AllControlvar
## equal variance
AllExperimentvar = leveneTest(Weight~Group, AllBdExpvar)
AllExperimentvar
## equal variance
##Normal distribution and equal variance, can proceed with normal ANOVA
AllSidesvar = leveneTest(Weight~Group, AllBdSidevar)
AllSidesvar

## All equal variance


## GLM 

## GLM side with total but not group
AllNaivesidelmertotal <- lmer(Total~Type+Sex+Trial + (1|Frog_Number), data = AllBdSidevar)
summary(AllNaivesidelmertotal)
car::Anova(AllNaivesidelmertotal, type="3")
## No significant difference in total time between males and females, trials or type of frog so perhaps don't need in final model

## GLM control with total but not group
AllNaiveConlmertotal <- lmer(Total~Sex+Trial + (1|Frog_Number), data = AllBdConvar)
summary(AllNaiveConlmertotal)
car::Anova(AllNaiveConlmertotal, type="3")
## No significant difference in total time between males and females, trials or type of frog so perhaps don't need in final model

## GLM experiment with total but not group
AllNaiveExplmertotal <- lmer(Total~Sex+Trial + (1|Frog_Number), data = AllBdExpvar)
summary(AllNaiveExplmertotal)
car::Anova(AllNaiveExplmertotal, type="3")
## Slight significance with sex and total time, consider putting total time into experiment model

# fit model sides
AllNaivesidelmer <- lmer(Weight~Group*Type*Trial*Sex + (1|Frog_Number), data = AllBdSidevar)
summary(AllNaivesidelmer)
car::Anova(AllNaivesidelmer, type="3")
library(emmeans)
emmeans(AllNaivesidelmer, list (pairwise~Group), lmer.df = "satterthwaite")
emmeans(AllNaivesidelmer, list (pairwise~Type), lmer.df = "satterthwaite")
emmeans(AllNaivesidelmer, list (pairwise~Trial), lmer.df = "satterthwaite")
emmeans(AllNaivesidelmer, list (pairwise~Sex), lmer.df = "satterthwaite")

## Significance is really driven by group
## Take away sex interaction
AllNaivesidelmer <- lmer(Weight~Group*Type*Trial+Sex + (1|Frog_Number), data = AllBdSidevar)
summary(AllNaivesidelmer)
car::Anova(AllNaivesidelmer, type="3")
## Doesn't change significance much, and interactions with other factors no longer significant
## Take away trial interaction
AllNaivesidelmer <- lmer(Weight~Group*Type*Sex+Trial + (1|Frog_Number), data = AllBdSidevar)
summary(AllNaivesidelmer)
car::Anova(AllNaivesidelmer, type="3")
## significance close to original model, interaction with sex and group is significant
## Take away type interaction
AllNaivesidelmer <- lmer(Weight~Group*Trial*Sex+Type + (1|Frog_Number), data = AllBdSidevar)
summary(AllNaivesidelmer)
car::Anova(AllNaivesidelmer, type="3")
## significance not really changed from original and sex is not significant now even with group
## Take away group interaction
AllNaivesidelmer <- lmer(Weight~Type*Trial*Sex+Group + (1|Frog_Number), data = AllBdSidevar)
summary(AllNaivesidelmer)
car::Anova(AllNaivesidelmer, type="3")
## no interactions significant now 

## Model with group and sex and group and type as interactions
AllNaivesidelmer <- lmer(Weight~Type+(Group*Sex)+(Group*Type)+Group+Sex+Trial + (1|Frog_Number), data = AllBdSidevar)
summary(AllNaivesidelmer)
car::Anova(AllNaivesidelmer, type="3")
## Type and group interaction not significant, so removing

## Final model will have interaction of group and sex, because in original model with all interactions the ones with sex and group were significant
AllNaivesidelmer <- lmer(Weight~Type+(Group*Sex)+Group+Sex+Trial + (1|Frog_Number), data = AllBdSidevar)
summary(AllNaivesidelmer)
car::Anova(AllNaivesidelmer, type="3")
emmeans(AllNaivesidelmer, list (pairwise~Group*Sex), lmer.df = "satterthwaite")
## no significance between A and C, still significance between A and N and C and N

## fit model control
AllNaiveConlmer <- lmer(Weight~Group*Trial*Sex + (1|Frog_Number), data = AllBdConvar)
summary(AllNaiveConlmer)
car::Anova(AllNaiveConlmer, type="3")
## Interaction between all three predictors is significant

## Take away sex interaction
AllNaiveConlmer <- lmer(Weight~Group*Trial+Sex + (1|Frog_Number), data = AllBdConvar)
summary(AllNaiveConlmer)
car::Anova(AllNaiveConlmer, type="3")
## No interactions are significant

## Take away trial interaction
AllNaiveConlmer <- lmer(Weight~Group*Sex+Trial + (1|Frog_Number), data = AllBdConvar)
summary(AllNaiveConlmer)
car::Anova(AllNaiveConlmer, type="3")
## No interactions are significant

## Take away group interaction
AllNaiveConlmer <- lmer(Weight~Trial*Sex+Group + (1|Frog_Number), data = AllBdConvar)
summary(AllNaiveConlmer)
car::Anova(AllNaiveConlmer, type="3")
## No significant interactions

## Because interaction between all predictors is fairly significant, will keep that model
AllNaiveConlmer <- lmer(Weight~Group*Trial*Sex + (1|Frog_Number), data = AllBdConvar)
summary(AllNaiveConlmer)
Anova(AllNaiveConlmer, type="3")
emmeans(AllNaiveConlmer, list (pairwise~Group*Trial*Sex), lmer.df = "satterthwaite")
## Without considering interactions the say nothing is significant, but the interactions
## say that there is a significant difference between time in side A with dead volatile females and 
## time in 

## fit model experiment
AllNaiveExplmer <- lmer(Weight~Group*Trial*Sex + (1|Frog_Number), data = AllBdExpvar)
summary(AllNaiveExplmer)
car::Anova(AllNaiveConlmer, type="3")
## No interaction effects, only significant is in N group, so using model with no interaction
AllNaiveExplmer <- lmer(Weight~Group+Trial+Sex + (1|Frog_Number), data = AllBdExpvar)
summary(AllNaiveExplmer)
emmeans(AllNaiveExplmer, list (pairwise~Group), lmer.df = "satterthwaite")
## Significant difference between control and and neutral and exp and neutral but not control and exp