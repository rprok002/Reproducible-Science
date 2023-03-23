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