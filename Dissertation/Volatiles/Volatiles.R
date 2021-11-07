# Proposal Preliminary Figure

volatiles1A <- read.csv(file.choose())
sp <- c(0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0)
par(mar = c(6, 6.5, 2, 0.5), mgp = c(4.5, 1, 0))
barplot(height = volatiles1A$Weight, names.arg = volatiles1A$Frog_Number, space = sp, main = "Dead Bd Chemosensory", ylab = "Proportion of Time", xlab = "Frog Number", col = volatiles1A$Color_Name, ylim = c(0,1.25), las=1, cex.lab = 1.0, cex.axis = 1.5, cex.names = 1.2, font.lab = 2)
legend(4 ,1.25 , legend = c( "Control","Bd"), col = c("chartreuse4","darkviolet"), pch = 15, cex = 0.75, text.font = 0.75)

volatiles1Aall <- read.csv(file.choose())
sp <- c(0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0, 0.6, 0)
par(mar = c(6, 6.5, 2, 0.5), mgp = c(4.5, 1, 0))
barplot(height = volatiles1Aall$Weight, names.arg = volatiles1Aall$Frog_Number, space = sp, main = "Dead Bd Chemosensory", ylab = "Proportion of Time", xlab = "Frog Number", col = volatiles1Aall$Color_Name, ylim = c(0,1.25), las=1, cex.lab = 1.0, cex.axis = 1.5, cex.names = 1.2, font.lab = 2)
legend(4 ,1.25 , legend = c( "Broth","Bd"), col = c("chartreuse4","darkviolet"), pch = 15, cex = 0.75, text.font = 0.75)

library(car)
volatiles1GLMbinomialA <- glm(Weight~Location*Group, family = binomial, data = volatiles1A)
anova(volatiles1GLMbinomialA, type=2)
car::Anova(volatiles1GLMbinomialA, type=2)
summary(volatiles1GLMbinomialA)

volatiles1GLMbinomialAall <- glm(Weight~Location*Group, family = binomial, data = volatiles1Aall)
anova(volatiles1GLMbinomialAall, type=2)
car::Anova(volatiles1GLMbinomialAall, type=2)
summary(volatiles1GLMbinomialAall)

## for first trial of volatiles, away and towards doesn't matter

library(car)
volatiles1GLMbinomialA <- glm(Weight~Location*Group, family = binomial, data = volatiles1A)
anova(volatiles1GLMbinomialA, type=2)
car::Anova(volatiles1GLMbinomialA, type=2)
summary(volatiles1GLMbinomialA)


library(ggpubr)
ggboxplot(volatiles1A, x = "Location", y = "Weight", col = c("chartreuse4", "grey66", "darkviolet"), main = "Dead Bd Chemosensory Trial 1", xlab = "Location", ylab = "Proportion of Time")+
  annotate(geom="text", x = 2.8, y = 1.1, label = "*p=0.0337", color = "black")
 
