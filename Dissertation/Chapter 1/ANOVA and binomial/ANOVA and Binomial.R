## Data Analysis Dead Bd Volatiles
DeadBdCon= read.csv(file.choose())
DeadBdExp= read.csv(file.choose())

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
