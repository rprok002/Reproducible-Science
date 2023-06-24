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
