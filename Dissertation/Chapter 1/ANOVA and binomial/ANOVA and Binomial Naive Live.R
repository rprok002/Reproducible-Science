## Data Analysis Live Naive Bd Volatiles
LiveBdCon= read.csv(file.choose())
LiveBdExp= read.csv(file.choose())
LiveBdside = read.csv(file.choose())

##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

## Control
ggdensity(LiveBdCon$Proportion.A, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(LiveBdCon$Proportion.A)
ggdensity(LiveBdCon$Proportion.C, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(LiveBdCon$Proportion.C)
ggdensity(LiveBdCon$Proportion.neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(LiveBdCon$Proportion.neutral)

## Experiment
ggdensity(LiveBdExp$Proportion.Control, main = "Density Plot of Average Control", xlab = "Average Control")
ggqqplot(LiveBdExp$Proportion.Control)
ggdensity(LiveBdExp$Proportion.Experiment, main = "Density Plot of Average Experiment", xlab = "Average Experiment")
ggqqplot(LiveBdExp$Proportion.Experiment)
ggdensity(LiveBdExp$Proportion.neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(LiveBdExp$Proportion.neutral)

## Sides
ggdensity(LiveBdside$Proportion.A, main = "Density Plot of Average Side A", xlab = "Average Side A")
ggqqplot(LiveBdside$Proportion.A)
ggdensity(LiveBdside$Proportion.C, main = "Density Plot of Average Side C", xlab = "Average Side C")
ggqqplot(LiveBdside$Proportion.C)
ggdensity(LiveBdside$Proportion.neutral, main = "Density Plot of Average Neutral", xlab = "Average Neutral")
ggqqplot(LiveBdside$Proportion.neutral)

## Shapiro-Wilks test
## Null: normal: Alt: not normal
shapiro.test(LiveBdCon$Proportion.A)
## normal
shapiro.test(LiveBdCon$Proportion.C)
## normal 
shapiro.test(LiveBdCon$Proportion.neutral)
## normal
shapiro.test(LiveBdExp$Proportion.Control)
## normal
shapiro.test(LiveBdExp$Proportion.Experiment)
## normal
shapiro.test(LiveBdExp$Proportion.neutral)
## normal
## Can assume normality for both control and experiment pops
shapiro.test(LiveBdside$Proportion.A)
## normal
shapiro.test(LiveBdside$Proportion.C)
## normal
shapiro.test(LiveBdside$Proportion.neutral)
## normal


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
## nonequal variance

## Normal distribution for all, equal variance except sides 
## Control and Exp can use ANOVA, Sides needs Welch's ANOVA because unequal variance
## Summary Statistics

library(dplyr)
group_by(LiveBdConvar, Group) %>%
  summarise(
    count = n(),
    mean = mean(Weight, na.rm = TRUE),
    sd = sd(Weight, na.rm = TRUE)
  )
group_by(LiveBdExpvar, Group) %>%
  summarise(
    count = n(),
    mean = mean(Weight, na.rm = TRUE),
    sd = sd(Weight, na.rm = TRUE)
  )
group_by(LiveBdSidevar, Group) %>%
  summarise(
    count = n(),
    mean = mean(Weight, na.rm = TRUE),
    sd = sd(Weight, na.rm = TRUE)
  )

## GLM for sides
GLM <- glm(Weight~Group*Type, family = gaussian, data = LiveBdSidevar)
GLM
summary(GLM)

## Neutral compared to both sides is significant, Experiment versus Control
## isn't different. Don't favor one treatment side over another. So, we can
## count all the frogs together and not as separate types of frogs

## ANOVAS
# Compute the analysis of variance for Control and Experiment
resCon.aov <- aov(Weight ~ Group, data = LiveBdConvar)
resExp.aov <- aov(Weight ~ Group, data = LiveBdExpvar)
# Compute the analysis of variance for Sides
resside.aov <- oneway.test(Weight ~ Group, data = LiveBdSidevar, var.equal = FALSE)
# Summary of the analysis
summary(resCon.aov)
summary(resExp.aov)
resside.aov
## Significance in both control and experiment groups, and from sides
## frogs in treatment areas when comparing sides (A as left and C as right)

## Multiple pairwise comparisons
TukeyHSD(resCon.aov)
TukeyHSD(resExp.aov)
library(rstatix)
gameshowell <- games_howell_test(LiveBdSidevar, Weight~Group)

## In all tests, don't care between areas A and C (or chytrid and control)
## but spend significantly more time in neutral no matter what

## Box plots
install.packages("ggpubr")
install.packages("multcompView")
library(ggpubr)
library(multcompView)
## Control boxplot
## compact letter display
control <- multcompLetters4(resCon.aov, TukeyHSD(resCon.aov))
control
## table with factors and 3rd quantile
controltable <- group_by(LiveBdConvar, Group) %>%
  summarise(mean=mean(Weight), quant = quantile(Weight, probs = 0.75)) %>%
  arrange(desc(mean))
# extracting the compact letter display and adding to the control table
control <- as.data.frame.list(control$Group)
controltable$control <- control$Letters
print(controltable)
## Boxplot
ggboxplot(LiveBdConvar, x = "Group", y = "Weight", 
          color = "Group", palette = c("turquoise1", "slateblue4", "purple1"),
          order = c("Nprop", "Aprop", "Cprop"),
          ylab = "Proportion of Time", xlab = "Location")+
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("Aprop","Cprop","Nprop"),
                   labels=c("Side A", "Side C", "Neutral"))+
  ggtitle("Control Frogs: Proportion of Time vs. Location Live Bd Volatiles")+
  theme(plot.title = element_text(hjust = 0.9))+
  geom_text(data = controltable, aes(x = Group, y = quant, label = control), 
            size = 4, vjust = -1, hjust =-1)+
  geom_boxplot(aes(fill = Group))+
  annotate("text", x=2.5, y=0.7, label = "F(2,6) = [7.275], p=0.025")
## Experiment boxplot
experiment <- multcompLetters4(resExp.aov, TukeyHSD(resExp.aov))
experiment
## table with factors and 3rd quantile
exptable <- group_by(LiveBdExpvar, Group) %>%
  summarise(mean=mean(Weight), quant = quantile(Weight, probs = 0.75)) %>%
  arrange(desc(mean))
# extracting the compact letter display and adding to the exp table
experiment <- as.data.frame.list(experiment$Group)
exptable$experiment <- experiment$Letters
print(exptable)
## Boxplot
ggboxplot(LiveBdExpvar, x = "Group", y = "Weight", 
          color = "Group", palette = c("turquoise1", "palegreen", "seagreen"),
          order = c("Nprop", "Cprop", "Eprop"),
          ylab = "Proportion of Time", xlab = "Location")+
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("Cprop","Eprop","Nprop"),
                   labels=c("Control", "Experiment", "Neutral"))+
  ggtitle("Experiment Frogs: Proportion of Time vs. Location Live Bd Volatiles")+
  theme(plot.title = element_text(hjust = 1))+
  geom_text(data = exptable, aes(x = Group, y = quant, label = experiment), 
            size = 4, vjust = -1, hjust =-1)+
  geom_boxplot(aes(fill = Group))+
  annotate("text", x=2.5, y=0.7, label = "F(2,24) = [35.27], p<0.001")+
  font("title", size = 13)

## Side boxplot
library(data.table)
Group <- c("Nprop", "Cprop", "Aprop")
Letters <- c("a", "b", "b")
side <- data.table(Group, Letters)
side
## table with factors and 3rd quantile
sidetable <- group_by(LiveBdSidevar, Group) %>%
  summarise(mean=mean(Weight), quant = quantile(Weight, probs = 0.75)) %>%
  arrange(desc(mean))
# extracting the compact letter display and adding to the control table
sidetable$Letters <- side$Letters
print(sidetable)
## Boxplot
ggboxplot(LiveBdSidevar, x = "Group", y = "Weight", 
          color = "Group", palette = c("turquoise1", "slateblue4", "purple1"),
          order = c("Nprop", "Aprop", "Cprop"),
          ylab = "Proportion of Time", xlab = "Location") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("Neutral","Side A","Side C"),
                   labels=c("Neutral", "Side A", "Side C"))+
  ggtitle("All Frogs: Proportion of Time vs. Location Live Bd Volatiles")+
  theme(plot.title = element_text(hjust = 0.9))+
  geom_text(data = sidetable, aes(x = Group, y = quant, label = Letters), 
            size = 4, vjust = -1, hjust =-1)+
  geom_boxplot(aes(fill = Group))+
  annotate("text", x=2.5, y=0.7, label = "F(2,19.758) = [38.407], p<0.001")
## Mean plots
ggline(LiveBdConvar, x = "Group", y = "Weight", 
       add = c("mean_se", "jitter"), 
       order = c("Aprop", "Cprop", "Nprop"),
       ylab = "Weight", xlab = "Treatment")
ggline(LiveBdExpvar, x = "Group", y = "Weight", 
       add = c("mean_se", "jitter"), 
       order = c("Cprop", "Eprop", "Nprop"),
       ylab = "Weight", xlab = "Treatment")
ggline(LiveBdSidevar, x = "Group", y = "Weight", 
       add = c("mean_se", "jitter"), 
       order = c("Aprop", "Cprop", "Nprop"),
       ylab = "Weight", xlab = "Treatment")


## Stacked bar graphs
## Control frogs
install.packages("ggplot2")
library(ggplot2)
ggplot(DeadBdConvar, aes(fill = Group, y = Weight, x = Frog_Number))+
  geom_bar(position = "stack", stat = "identity", color = "black")+
  facet_grid(.~Frog_Number, scales = "free_x", switch = "x")+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white"),
        plot.title = element_text(hjust = 0.7))+
  xlab("Frog Number") + ylab("Proportion of Time") + 
  ggtitle("Control Frogs: Proportion of Time per Location")+
  labs(fill = "Location")+
  scale_fill_manual(values = c("slateblue4", "purple1", "turquoise1"), 
                    labels = c("Side A", "Side C", "Neutral"))
## Experiment plot
ggplot(DeadbdExpvar, aes(fill = Group, y = Weight, x = Frog_Number))+
  geom_bar(position = "stack", stat = "identity", color = "black")+
  facet_grid(.~Frog_Number, scales = "free_x", switch = "x")+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white"),
        plot.title = element_text(hjust = 0))+
  xlab("Frog Number") + ylab("Proportion of Time") + 
  ggtitle("Experiment Frogs: Proportion of Time per Location")+
  labs(fill = "Location")+
  scale_fill_manual(values = c("palegreen", "seagreen", "turquoise1"), 
                    labels = c("Control", "Experiment", "Neutral"))

## Sides plot
DeadBdsidesvar$Group <- factor(DeadBdsidesvar$Group, levels = c("Side A", "Side C", "Neutral"))
ggplot(DeadBdsidesvar, aes(fill = Group, y = Weight, x = Frog_Number))+
  geom_bar(position = "stack", stat = "identity", color = "black")+
  facet_grid(.~Frog_Number, scales = "free_x", switch = "x")+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white"),
        plot.title = element_text(hjust = 0.5))+
  xlab("Frog Number") + ylab("Proportion of Time") + 
  ggtitle("All Frogs: Proportion of Time per Location")+
  labs(fill = "Location")+
  scale_fill_manual(values = c("slateblue4", "purple1", "turquoise1"), 
                    labels = c("Side A", "Side C", "Neutral"))