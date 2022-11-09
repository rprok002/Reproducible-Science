## Data Analysis Dead Bd Volatiles
DeadBdCon= read.csv(file.choose())
DeadBdExp= read.csv(file.choose())
DeadBdside = read.csv(file.choose())

##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

## Control
ggdensity(DeadBdCon$Proportion.A, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(DeadBdCon$Proportion.A)
ggdensity(DeadBdCon$Proportion.C, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(DeadBdCon$Proportion.C)
ggdensity(DeadBdCon$Proportion.neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(DeadBdCon$Proportion.neutral)

## Experiment
ggdensity(DeadBdExp$Proportion.control, main = "Density Plot of Average Control", xlab = "Average Control")
ggqqplot(DeadBdExp$Proportion.control)
ggdensity(DeadBdExp$Proportion.experiment, main = "Density Plot of Average Experiment", xlab = "Average Experiment")
ggqqplot(DeadBdExp$Proportion.experiment)
ggdensity(DeadBdExp$Proportion.neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(DeadBdExp$Proportion.neutral)

## Sides
ggdensity(DeadBdside$Proportion.A, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(DeadBdside$Proportion.A)
ggdensity(DeadBdside$Proportion.C, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(DeadBdside$Proportion.C)
ggdensity(DeadBdside$Proportion.neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(DeadBdside$Proportion.neutral)

## Shapiro-Wilks test
## Null: normal: Alt: not normal
shapiro.test(DeadBdCon$Proportion.A)
## normal
shapiro.test(DeadBdCon$Proportion.C)
## normal (though p value is only 0.07, so pretty close)
shapiro.test(DeadBdCon$Proportion.neutral)
## normal
shapiro.test(DeadBdExp$Proportion.control)
## normal
shapiro.test(DeadBdExp$Proportion.experiment)
## normal
shapiro.test(DeadBdExp$Proportion.neutral)
## normal
## Can assume normality for both control and experiment pops
shapiro.test(DeadBdside$Proportion.A)
## normal
shapiro.test(DeadBdside$Proportion.C)
## not normal
shapiro.test(DeadBdside$Proportion.neutral)
## normal
## for sides data, side C isn't normal. Keep in mind for future tests

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
## Normal distribution except for Side C, equal variance, going to run a binomial GLM
## Groups are already ordered alphabetically, don't need to redo
## Summary Statistics

library(dplyr)
group_by(DeadBdConvar, Group) %>%
  summarise(
    count = n(),
    mean = mean(Weight, na.rm = TRUE),
    sd = sd(Weight, na.rm = TRUE)
  )
group_by(DeadbdExpvar, Group) %>%
  summarise(
    count = n(),
    mean = mean(Weight, na.rm = TRUE),
    sd = sd(Weight, na.rm = TRUE)
  )
group_by(DeadBdsidesvar, Group) %>%
  summarise(
    count = n(),
    mean = mean(Weight, na.rm = TRUE),
    sd = sd(Weight, na.rm = TRUE)
  )
## Box plots
install.packages("ggpubr")
library(ggpubr)
ggboxplot(DeadBdConvar, x = "Group", y = "Weight", 
          color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Aprop", "Cprop", "Nprop"),
          ylab = "Weight", xlab = "Treatment")
ggboxplot(DeadbdExpvar, x = "Group", y = "Weight", 
          color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Conprop", "Expprop", "Nprop"),
          ylab = "Weight", xlab = "Treatment")
ggboxplot(DeadBdsidesvar, x = "Group", y = "Weight", 
          color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Side A", "Side C", "Neutral"),
          ylab = "Weight", xlab = "Treatment")
## Mean plots
ggline(DeadBdConvar, x = "Group", y = "Weight", 
       add = c("mean_se", "jitter"), 
       order = c("Aprop", "Cprop", "Nprop"),
       ylab = "Weight", xlab = "Treatment")
ggline(DeadbdExpvar, x = "Group", y = "Weight", 
       add = c("mean_se", "jitter"), 
       order = c("Conprop", "Expprop", "Nprop"),
       ylab = "Weight", xlab = "Treatment")
ggline(DeadBdsidesvar, x = "Group", y = "Weight", 
       add = c("mean_se", "jitter"), 
       order = c("Side A", "Side C", "Neutral"),
       ylab = "Weight", xlab = "Treatment")
## ANOVAS
# Compute the analysis of variance
resCon.aov <- aov(Weight ~ Group, data = DeadBdConvar)
resExp.aov <- aov(Weight ~ Group, data = DeadbdExpvar)
# Summary of the analysis
summary(resCon.aov)
summary(resExp.aov)
## Significance in both control and experiment groups

## Multiple pairwise comparisons
TukeyHSD(resCon.aov)
TukeyHSD(resExp.aov)

## Both control and exp have significant more time in neutral,
## but not sig diff between the other two areas: don't care which
## of the treatment areas they are in

## GLM for sides
GLM <- glm(Weight~Group*Type, family = gaussian, data = DeadBdsidesvar)
GLM
summary(GLM)
## Neutral compared to both sides is significant, Experiment versus Control
## isn't different. Don't favor one treatment side over another