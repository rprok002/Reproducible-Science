## Volatiles 1 Trial
## Check for normality
volatilesfull <- read.csv(file.choose())
volatiles1 <- read.csv(file.choose())
volatilescontrolfull <- read.csv(file.choose())
volatilescontrol <- read.csv(file.choose())
volatilesexperimentfull <- read.csv(file.choose())
volatilesexperiment <- read.csv(file.choose())
## Boxplot for normality for all data
boxplot(volatilesfull$Control.Proportion, volatilesfull$Neutral.Proportion, volatilesfull$Experiment.Proportion, 
        main = "Volatiles 1 Locations", xlab = "Location", ylab = "Proportion of Time",
        names = c("Control", "Neutral", "Chytrid"), border = c("darkolivegreen", "cyan3", "lightgreen"), col = c("white", "white", "white"))
text(x=1, y=0.8, labels="n=10")
## Boxplot for normality for control
boxplot(volatilescontrolfull$Control.Proportion, volatilescontrolfull$Neutral.Proportion, volatilescontrolfull$Experiment.Proportion, 
        main = "Volatiles 1 Control Locations", xlab = "Location", ylab = "Proportion of Time",
        names = c("Control 1", "Neutral", "Control 2"), border = c("darkolivegreen", "cyan3", "darkolivegreen"), col = c("white", "white", "white"))
text(x=1, y=0.6, labels="n=2")
## Boxplot for normality for experiment
boxplot(volatilesexperimentfull$Control.Proportion, volatilesexperimentfull$Neutral.Proportion, volatilesexperimentfull$Experiment.Proportion, 
        main = "Volatiles 1 Experiment Locations", xlab = "Location", ylab = "Proportion of Time",
        names = c("Control", "Neutral", "Chytrid"), border = c("darkolivegreen", "cyan3", "lightgreen"), col = c("white", "white", "white"))
text(x=1, y=0.6, labels="n=8")
## Shapiro-Wilk test for normality for all data
shapiro.test(volatilesfull$Control.Proportion)
shapiro.test(volatilesfull$Neutral.Proportion)
shapiro.test((volatilesfull$Experiment.Proportion))
##All three areas are normally distributed
## Can't do shapiro-wilk for control because only n=2
## Shapiro-Wilk test for normality for experiment data
shapiro.test(volatilesexperimentfull$Control.Proportion)
shapiro.test(volatilesexperimentfull$Neutral.Proportion)
shapiro.test((volatilesexperimentfull$Experiment.Proportion))
## test of equal variance for all data
library(car)
bartlett.test(Weight ~ Location, data = volatiles1)
## pvalue is greater than 0.05, so have homogeneity of variances
## test of equal variance for experiment data
library(car)
bartlett.test(Weight ~ Location, data = volatilesexperiment)
## pvalue is greater than 0.05, so have homogeneity of variances
## ANOVAs
volatiles1controlgroupaov <- aov(Weight ~ Location, data = volatilescontrol)
summary(volatiles1controlgroupaov)
volatiles1experimentgroupaov <- aov(Weight ~ Location, data = volatilesexperiment)
summary(volatiles1experimentgroupaov)
## Tukey tests
tukeyvolatiles1control <- tukey_hsd(volatiles1controlgroupaov)
tukeyvolatiles1control
tukeyvolatiles1experiment <- tukey_hsd(volatiles1experimentgroupaov)
tukeyvolatiles1experiment
## Graphs
library(ggpubr)
ggboxplot(volatilescontrol, x = "Location", y = "Weight", col = c("darkolivegreen", "cyan3", "darkolivegreen"), main = "Volatiles 1 Control ANOVA", xlab = "Location", ylab = "Proportion of Time")+
  stat_pvalue_manual(tukeyvolatiles1control, label = "p.adj", y.position = c(0.8,0.9,1.0))+
  stat_compare_means(method = "anova", label.y = 1.1)+
  geom_text(x=3, y=1.1, label= "n=2")
ggboxplot(volatilesexperiment, x = "Location", y = "Weight", col = c("darkolivegreen", "cyan3", "lightgreen"), main = "Volatiles 1 Experiment ANOVA", xlab = "Location", ylab = "Proportion of Time")+
  stat_pvalue_manual(tukeyvolatiles1experiment, label = "p.adj", y.position = c(1.0,1.15,1.25))+
  stat_compare_means(method = "anova", label.y = 1.4)+
  geom_text(x=3, y=1.4, label= "n=8")
## GLM
volatiles1GLMgaussian <- glm(Weight~Location*Group, family = gaussian, data = volatiles1)
volatiles1GLMgaussian
summary(volatiles1GLMgaussian)
## summary of the gaussian states that where the significance lies in the model is comparing
## chytrid to neutral area (p for t values is 0.0557). Neutral is more positive than chytrid, so
## indicates spending more time in neutral than in chytrid and there is a significant difference there
anova(volatiles1GLMgaussian)
anova1 = aov(Weight~Location*Group, data = volatiles1)
anova(anova1)
summary(anova1)

lm1 = lm(Weight~Location*Group, data = volatiles1)
anova(lm1)
## Shows that group doesn't matter and the interaction between group and location (ie neutral in Experiment
## group vs neutral in Control group) doesn't matter either. Location matters
summary(lm1)
summ(lm1)
library(jtools)
effect_plot(lm1, pred = Location, interval = TRUE, plot.points = TRUE)
library(ggstance)
plot_summs(lm1, scale = TRUE, plot.distributions = TRUE, inner_ci_level = 0.9, colors = c("lightgreen"),
           coefs = c("Chytrid on Neutral/GroupE Interaction" = "LocationNeutral:GroupE", 
                     "Chytrid on Control/GroupE Interaction" = "LocationControl:GroupE",
                     "Chytrid on GroupE" = "GroupE", "Chytrid on Neutral Location" = "LocationNeutral",
                     "Chytrid on Control Location" = "LocationControl")) + ggtitle("Volatiles Linear Model Coefficients") +
  annotate(geom="text", x = 0.3441, y = 4.7, label = "p=0.0557", color = "black")+
  annotate(geom="text", x = -0.1856, y = 5.6, label = "p=0.2887", color = "black")+
  annotate(geom="text", x = -0.1369, y = 3.8, label = "p=0.3218", color = "black")+
  annotate(geom="text", x = 0.2787, y = 2.6, label = "p=0.1581", color = "black")+
  annotate(geom="text", x = 0.1319, y = 1.6, label = "p=0.4973", color = "black")
library(huxtable)
export_summs(lm1, scale = TRUE)


