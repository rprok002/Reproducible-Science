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

## ANOVAS
# Compute the analysis of variance
resCon.aov <- aov(Weight ~ Group, data = DeadBdConvar)
resExp.aov <- aov(Weight ~ Group, data = DeadbdExpvar)
resside.aov <- aov(Weight ~ Group, data = DeadBdsidesvar)
# Summary of the analysis
summary(resCon.aov)
summary(resExp.aov)
summary(resside.aov)
## Significance in both control and experiment groups, and from sides
## Can run anova because GLM shows that no difference in control and experiment
## frogs in treatment areas when comparing sides (A as left and C as right)

## Multiple pairwise comparisons
TukeyHSD(resCon.aov)
TukeyHSD(resExp.aov)
TukeyHSD(resside.aov)

## Both control and exp have significant more time in neutral,
## but not sig diff between the other two areas: don't care which
## of the treatment areas they are in
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
controltable <- group_by(DeadBdConvar, Group) %>%
  summarise(mean=mean(Weight), quant = quantile(Weight, probs = 0.75)) %>%
  arrange(desc(mean))
# extracting the compact letter display and adding to the control table
control <- as.data.frame.list(control$Group)
controltable$control <- control$Letters
print(controltable)
## Boxplot
ggboxplot(DeadBdConvar, x = "Group", y = "Weight", 
          color = "Group", palette = c("turquoise1", "slateblue4", "purple1"),
          order = c("Nprop", "Aprop", "Cprop"),
          ylab = "Proportion of Time", xlab = "Location")+
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("Aprop","Cprop","Nprop"),
                   labels=c("Side A", "Side C", "Neutral"))+
  ggtitle("Control Frogs: Proportion of Time vs. Location")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data = controltable, aes(x = Group, y = quant, label = control), 
            size = 4, vjust = -1, hjust =-1)+
  geom_boxplot(aes(fill = Group))+
  annotate("text", x=2.5, y=0.7, label = "F(2,6) = [16.61], p=0.003")
## Experiment boxplot
experiment <- multcompLetters4(resExp.aov, TukeyHSD(resExp.aov))
experiment
## table with factors and 3rd quantile
exptable <- group_by(DeadbdExpvar, Group) %>%
  summarise(mean=mean(Weight), quant = quantile(Weight, probs = 0.75)) %>%
  arrange(desc(mean))
# extracting the compact letter display and adding to the exp table
experiment <- as.data.frame.list(experiment$Group)
exptable$experiment <- experiment$Letters
print(exptable)
## Boxplot
ggboxplot(DeadbdExpvar, x = "Group", y = "Weight", 
          color = "Group", palette = c("turquoise1", "palegreen", "seagreen"),
          order = c("Nprop", "Conprop", "Expprop"),
          ylab = "Proportion of Time", xlab = "Location")+
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("Conprop","Expprop","Nprop"),
                   labels=c("Control", "Experiment", "Neutral"))+
  ggtitle("Experiment Frogs: Proportion of Time vs. Location")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data = exptable, aes(x = Group, y = quant, label = experiment), 
            size = 4, vjust = -1, hjust =-1)+
  geom_boxplot(aes(fill = Group))+
  annotate("text", x=2.5, y=0.7, label = "F(2,27) = [163.7], p<0.001")

## Side boxplot
side <- multcompLetters4(resside.aov, TukeyHSD(resside.aov))
side
## table with factors and 3rd quantile
sidetable <- group_by(DeadBdsidesvar, Group) %>%
  summarise(mean=mean(Weight), quant = quantile(Weight, probs = 0.75)) %>%
  arrange(desc(mean))
# extracting the compact letter display and adding to the control table
side <- as.data.frame.list(side$Group)
sidetable$side <- side$Letters
print(sidetable)
## Boxplot
ggboxplot(DeadBdsidesvar, x = "Group", y = "Weight", 
          color = "Group", palette = c("turquoise1", "slateblue4", "purple1"),
          order = c("Neutral", "Side A", "Side C"),
          ylab = "Proportion of Time", xlab = "Location") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("Neutral","Side A","Side C"),
                   labels=c("Neutral", "Side A", "Side C"))+
  ggtitle("All Frogs: Proportion of Time vs. Location")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data = sidetable, aes(x = Group, y = quant, label = side), 
            size = 4, vjust = -1, hjust =-1)+
  geom_boxplot(aes(fill = Group))+
  annotate("text", x=2.5, y=0.7, label = "F(2,36) = [154], p<0.001")
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

## GLM for sides
GLM <- glm(Weight~Group*Type, family = gaussian, data = DeadBdsidesvar)
GLM
summary(GLM)
## Neutral compared to both sides is significant, Experiment versus Control
## isn't different. Don't favor one treatment side over another. So, we can
## count all the frogs together and not as separate types of frogs

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