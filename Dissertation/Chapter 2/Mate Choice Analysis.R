## Mate Choice Data Analysis
##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)
library(lmtest)
library(car)


## Mate Choice Data Files####
MateChoiceCompiledDataControlNoCall <- read.csv(file.choose())
MateChoiceAnalysisControl <- read.csv(file.choose())
MateChoiceAnalysisInfected <- read.csv(file.choose())
MateChoiceAnalysisInfectedMore <- read.csv(file.choose())
MateChoiceAnalysisInfectedLess <- read.csv(file.choose())

## Original qqplots of mate choice data####
ggdensity(MateChoiceCompiledDataControlNoCall$Total_Trial_Time_Seconds, main = "Density Plot of Total Trial Time Seconds", xlab = " Total Trial Time Seconds")
ggqqplot(MateChoiceCompiledDataControlNoCall$Total_Trial_Time_Seconds)
## def not normal distribution, but independent variable doesn't need to be 

ggdensity(MateChoiceCompiledDataControlNoCall$Female_Interaction_Zone, main = "Density Plot of Female Interaction Zone", xlab = " Female Interaction Zone")
ggqqplot(MateChoiceCompiledDataControlNoCall$Female_Interaction_Zone)
## not normal distribution

ggdensity(MateChoiceCompiledDataControlNoCall$Female_Left, main = "Density Plot of Female Left", xlab = " Female Left")
ggqqplot(MateChoiceCompiledDataControlNoCall$Female_Left)
## not normal distribution

ggdensity(MateChoiceCompiledDataControlNoCall$Female_Right, main = "Density Plot of Female Right", xlab = " Female Right")
ggqqplot(MateChoiceCompiledDataControlNoCall$Female_Right)
## not normal distribution

ggdensity(MateChoiceCompiledDataControlNoCall$SQRT_Total_Trial_Time_Seconds, main = "Density Plot of SQRT Total Trial Time Seconds", xlab = " SQRT Total Trial Time Seconds")
ggqqplot(MateChoiceCompiledDataControlNoCall$SQRT_Total_Trial_Time_Seconds)
## def not normal, but independent variable doesn't need to be

ggdensity(MateChoiceCompiledDataControlNoCall$SQRT_Female_Interaction_Zone, main = "Density Plot of SQRT Female Interaction Zone", xlab = " SQRT Female Interaction Zone")
ggqqplot(MateChoiceCompiledDataControlNoCall$SQRT_Female_Interaction_Zone)
## sqrt actually skews more

ggdensity(MateChoiceCompiledDataControlNoCall$SQRT_Female_Left, main = "Density Plot of SQRT Female Left", xlab = " SQRT Female Left")
ggqqplot(MateChoiceCompiledDataControlNoCall$SQRT_Female_Left)
## sqrt helps to normalize

ggdensity(MateChoiceCompiledDataControlNoCall$SQRT_Female_Right, main = "Density Plot of SQRT Female Right", xlab = " SQRT Female Right")
ggqqplot(MateChoiceCompiledDataControlNoCall$SQRT_Female_Right)
## sqrt helps to normalize

ggdensity(MateChoiceAnalysisControl$Weight_Seconds, main = "Density Plot of Weight Seconds", xlab = " Weight Seconds")
ggqqplot(MateChoiceAnalysisControl$Weight_Seconds)
## not normal

ggdensity(MateChoiceAnalysisControl$SQRT_Weight_Seconds, main = "Density Plot of SQRT Weight Seconds", xlab = " SQRT Weight Seconds")
ggqqplot(MateChoiceAnalysisControl$SQRT_Weight_Seconds)
## helps to normalize better

ggdensity(MateChoiceAnalysisControl$Time_Interaction_Zone, main = "Density Plot of Time Interaction Zone", xlab = " Time Interaction Zone")
ggqqplot(MateChoiceAnalysisControl$Time_Interaction_Zone)
## not normal, but independent doesn't have to be

ggdensity(MateChoiceAnalysisControl$SQRT_Time_Interaction_Zone, main = "Density Plot of SQRT Time Interaction Zone", xlab = " SQRT Time Interaction Zone")
ggqqplot(MateChoiceAnalysisControl$SQRT_Time_Interaction_Zone)
## actually makes worse

## using sqrt for Weight Seconds because normalizes so no family need be added for LMER 

ggdensity(MateChoiceAnalysisInfected$Weight_Seconds, main = "Density Plot of Weight Seconds", xlab = " Weight Seconds")
ggqqplot(MateChoiceAnalysisInfected$Weight_Seconds)

ggdensity(MateChoiceAnalysisInfected$SQRT_Weight_Seconds, main = "Density Plot of SQRT Weight Seconds", xlab = " SQRT Weight Seconds")
ggqqplot(MateChoiceAnalysisInfected$SQRT_Weight_Seconds)
## more normalized

## using sqrt for Weight Seconds because normalizes so no family need be added for LMER 

## Control Models Mate Choice####
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)
library(lmtest)
library(car)

ControlTrialLMER <- lmer(SQRT_Weight_Seconds~Group+Male_Pair_Letter+Time_Interaction_Zone+(1|Frog_Number)+(1|Female_Trial_Order) , data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)
## Female frog number and trial order not effective as random, moving trial time to fixed

ControlTrialLMER <- lmer(SQRT_Weight_Seconds~Group+Male_Pair_Letter+Time_Interaction_Zone+(1|Frog_Number)+Female_Trial_Order , data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)
## doesn't effect anything, removing trial order from model and putting male pair as random

ControlTrialLMER <- lmer(SQRT_Weight_Seconds~Group+(1|Male_Pair_Letter)+Time_Interaction_Zone+(1|Frog_Number) , data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)
## male pair order still doens't affect, so keeping as random. Removing interation zone to see if does anything

ControlTrialLMER <- lmer(SQRT_Weight_Seconds~Group+(1|Male_Pair_Letter)+(1|Frog_Number) , data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)
## makes Group even farther from significance but doesn't change anything else, so keeping in to be conservative

## Final control model
ControlTrialLMER <- lmer(Weight_Seconds~Group+Time_Interaction_Zone+(1|Frog_Number)+(1|Male_Pair_Letter) , data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)
plot(ControlTrialLMER)

WS <- MateChoiceAnalysisControl$Weight_Seconds
hist(WS)

simsControlTrialLMER <- simulateResiduals(ControlTrialLMER)
plot(simsControlTrialLMER, quantreg = FALSE)
which(residuals(simsControlTrialLMER) == 1 | residuals(simsControlTrialLMER) == 0)
which(residuals(simsControlTrialLMER) >0.99 | residuals(simsControlTrialLMER) < 0.01)

## remove F12 Male Pair B, F5 Male Pair E and F12 Male Pair E, F4 Male Pair E, F5 Male Pair J, F19 Male Pair B making new data table 

MateChoiceAnalysisControlDHARMa <- read.csv(file.choose())
ControlTrialLMERDHARMa <- lmer(SQRT_Weight_Seconds~Group+(1|Frog_Number)+(1|Male_Pair_Letter), data = MateChoiceAnalysisControlDHARMa)
anova(ControlTrialLMERDHARMa)
summary(ControlTrialLMERDHARMa)
simsControlTrialLMERDHARMa <- simulateResiduals(ControlTrialLMERDHARMa)
plot(simsControlTrialLMERDHARMa, quantreg = FALSE)
which(residuals(simsControlTrialLMERDHARMa) == 1 | residuals(simsControlTrialLMERDHARMa) == 0)
which(residuals(simsControlTrialLMERDHARMa) >0.99 | residuals(simsControlTrialLMERDHARMa) < 0.01)

## Still just barely nonsignificant, still worth it to look at weighted regressions 
## interaction zone time significant so keeping in model, but group is not significant so don't have to 
## include side of apparatus as an effect in the infection model

## Infected Models Mate Choice ####
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)
library(lmtest)

InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+Male_Pair_Letter+Time_Interaction_Zone+(1|Frog_Number)+(1|Female_Trial_Order) , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)
## Female frog number and trial order not effective as random, moving trial time to fixed

InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+(1|Male_Pair_Letter)+Time_Interaction_Zone+(1|Frog_Number)+ Female_Trial_Order , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)
## doesn't effect anything, removing trial order from model and putting male pair as random
## male pair order still doens't affect, so keeping as random
## Removing interation zone to see if does anything

InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+(1|Male_Pair_Letter)+(1|Frog_Number), data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)
## interaction zone time significant so keeping in model, but group is not significant so don't have to 


## Comparing more and less infected, adding in a variable real quick
InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+More_Less_Infected+(1|Male_Pair_Letter)+Time_Interaction_Zone+(1|Frog_Number) , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)

InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group*More_Less_Infected+(1|Male_Pair_Letter)+Time_Interaction_Zone+(1|Frog_Number) , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)

## doesn't change anything whether interaction or not, so even though color may change the females don't use that to choose mates

## Final Infected Model
InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+(1|Male_Pair_Letter)+(1|Frog_Number) , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)
plot(InfectedTrialLMER)

simsInfectedTrialLMER <- simulateResiduals(InfectedTrialLMER)
plot(simsInfectedTrialLMER, quantreg = FALSE)
which(residuals(simsInfectedTrialLMER) == 1 | residuals(simsInfectedTrialLMER) == 0)
which(residuals(simsInfectedTrialLMER) >0.99 | residuals(simsInfectedTrialLMER) < 0.01)

## Remove F12 Male Pair A and F12 Male Pair H

MateChoiceAnalysisInfectedDHARMa <- read.csv(file.choose())
InfectedTrialLMERDHARMa <- lmer(SQRT_Weight_Seconds~Group+(1|Frog_Number)+(1|Male_Pair_Letter) , data = MateChoiceAnalysisInfectedDHARMa)
anova(InfectedTrialLMERDHARMa)
summary(ControlTrialLMERDHARMa)
simsInfectedTrialLMERDHARMa <- simulateResiduals(InfectedTrialLMERDHARMa)
plot(simsInfectedTrialLMERDHARMa, quantreg = FALSE)


## interaction zone time significant so keeping in model, but group is not significant, so females don't
## stay close to either infected or clean frog significantly less/more

## Infected Models Separated by Less and More Infected

MoreInfectedTrialLMER <- glmer(Weight_Seconds~Group+(1|Frog_Number)+(1|Male_Pair_Letter) , data = MateChoiceAnalysisInfectedMore, family = "poisson")
anova(MoreInfectedTrialLMER)
summary(MoreInfectedTrialLMER)

plot(MoreInfectedTrialLMER)
## still not signifiant even though more infected

LessInfectedTrialLMER <- glmer(Weight_Seconds~Group+(1|Frog_Number)+(1|Male_Pair_Letter) , data = MateChoiceAnalysisInfectedLess, family = "poisson")
anova(LessInfectedTrialLMER)
summary(LessInfectedTrialLMER)
plot(LessInfectedTrialLMER)

## not significant even though less infected

## Weighted Least Squares Regression Models and Male Movement Models ####
ControlTrialLMER <- lmer(Weight_Seconds~Group+Male_Pair_Letter+Time_Interaction_Zone+(1|Frog_Number)+(1|Female_Trial_Order) , data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)
## Removing female trial order because not significant, putting male pair as random factor 

ControlTrialLMER <- glmer(Weight_Seconds~Group+(1|Frog_Number)+(1|Male_Pair_Letter), data = MateChoiceAnalysisControl,  family = "binomial")
anova(ControlTrialLMER)
summary(ControlTrialLMER)

ControlTrialLMER <- lmer(Weight_Seconds~Group+(1|Frog_Number)+(1|Male_Pair_Letter), data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)

ControlTrialLMER <- glmer(Weight_Seconds~Group+(1|Frog_Number)+(1|Male_Pair_Letter), data = MateChoiceAnalysisControl, family = "poisson")
anova(ControlTrialLMER)
summary(ControlTrialLMER)

simsControlTrialLMER <- simulateResiduals(ControlTrialLMER)
plot(simsControlTrialLMER, quantreg = FALSE)

plot(ControlTrialLMER)

## Linearity
ggqqplot(residuals(ControlTrialLMER))
plot(ControlTrialLMER)

ggdensity(MateChoiceAnalysisInfected$Weight_Seconds_Female, main = "Density Plot of SQRT Weight Seconds", xlab = " SQRT Weight Seconds")
ggqqplot(MateChoiceAnalysisInfected$Weight_Seconds_Female)
hist(MateChoiceAnalysisInfected$Weight_Seconds_Female)

InfectedTrialLMER <- glmer(Weight_Seconds_Female~Group+(1|Frog_Number), data = MateChoiceAnalysisInfected, family = "poisson")
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)
plot(InfectedTrialLMER)
ggqqplot(residuals(InfectedTrialLMER))
emmeans(InfectedTrialLMER, list (pairwise~Group), lmer.df = "satterthwaite")

## Male movement data
MateChoiceAnalysisInfectedMaleMove <- read.csv(file.choose())
ggdensity(MateChoiceAnalysisInfected$Male_Wander_Seconds, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Wander_Seconds)
ggdensity(MateChoiceAnalysisInfected$Male_Wander_Seconds_Log, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Wander_Seconds_Log)
ggdensity(MateChoiceAnalysisInfected$Male_Wander_Seconds_SQRT, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Wander_Seconds_SQRT)
hist(MateChoiceAnalysisInfected$Male_Wander_Seconds)
hist(MateChoiceAnalysisInfected$Male_Wander_Seconds_Log)
hist(MateChoiceAnalysisInfected$Male_Wander_Seconds_SQRT)

InfectedTrialLMERMaleWander <- glmer(Male_Wander_Seconds~Group+(1|Male_Pair_Letter), data = MateChoiceAnalysisInfected, family = poisson(link="log"), weights = wt)
anova(InfectedTrialLMERMaleWander)
summary(InfectedTrialLMERMaleWander)
plot(InfectedTrialLMERMaleWander)
ggqqplot(residuals(InfectedTrialLMERMaleWander))
emmeans(InfectedTrialLMERMaleWander, list (pairwise~Group), lmer.df = "satterthwaite")

simsInfectedTrialLMERMaleWander <- simulateResiduals(InfectedTrialLMERMaleWander)
plot(simsInfectedTrialLMERMaleWander, quantreg = FALSE)

## Front
ggdensity(MateChoiceAnalysisInfected$Male_Front_Seconds, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Front_Seconds)

InfectedTrialLMERMaleFront <- glmer(Male_Front_Seconds~Group+(1|Male_Pair_Letter), data = MateChoiceAnalysisInfected, family = poisson(link="log"), weights = wt)
anova(InfectedTrialLMERMaleFront)
summary(InfectedTrialLMERMaleFront)
plot(InfectedTrialLMERMaleFront)
ggqqplot(residuals(InfectedTrialLMERMaleFront))
emmeans(InfectedTrialLMERMaleFront, list (pairwise~Group), lmer.df = "satterthwaite")


## Boxplots Mate Choice ####

MateChoiceAnalysisControl$Group <- factor(MateChoiceAnalysisControl$Group, levels = c("LeftM", "RightM"))
ggboxplot(MateChoiceAnalysisControl, x = "Group", y = "Weight_Seconds",  ylab = " Time (seconds)", xlab = "Male",
           ylim = c(0, 1200), fill = "lightgrey") + 
  scale_x_discrete(labels=c("Left", "Right"))+
  scale_y_continuous(breaks=seq(0,900,by=100))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.title=element_blank())+
  annotate("text", x=2, y=1100, label= "F: 0.18 ; DF: 1,233 ; p = 0.67", fontface = "bold", size = 5)+
  annotate("text", x=2, y=1200, label= " Side", fontface = "bold", size = 5)+
  annotate("text", x=1, y=296, label="x", size = 7)+
  annotate("text", x=2, y=265, label="x", size = 7)
  

MateChoiceAnalysisInfected$Group <- factor(MateChoiceAnalysisInfected$Group, levels = c("Control", "Infected"))
mc1 <- ggboxplot(MateChoiceAnalysisInfected, x = "Group", y = "Weight_Seconds_Female",  ylab = " Time (seconds)", xlab = "Male",
          ylim = c(0, 1100), fill = "Group", palette = c("snow3", "snow4")) + 
  scale_y_continuous(breaks=seq(0,900,by=100))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.title.x = element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  annotate("text", x=1, y=277, label="x", size = 7)+
  annotate("text", x=2, y=336, label="x", size = 7)+
  geom_signif(y_position = c(1000), xmin = c(1), xmax = c(2),
              annotation = c("***"), tip_length = 0, textsize = 5)
mc1

MateChoiceAnalysisInfected$Group <- factor(MateChoiceAnalysisInfected$Group, levels = c("Control", "Infected"))
mc2 <- ggboxplot(MateChoiceAnalysisInfected, x = "Group", y = "Male_Wander_Seconds",  ylab = " Time (seconds)", xlab = "Male",
          ylim = c(0, 1100), fill = "Group", palette = c("snow3", "snow4")) + 
  scale_y_continuous(breaks=seq(0,900,by=100))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.title=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  annotate("text", x=1, y=89, label="x", size = 7)+
  annotate("text", x=2, y=97, label="x", size = 7)+
  geom_signif(y_position = c(1000), xmin = c(1), xmax = c(2),
              annotation = c("***"), tip_length = 0, textsize = 5)
mc2

MateChoiceAnalysisInfected$Group <- factor(MateChoiceAnalysisInfected$Group, levels = c("Control", "Infected"))
mc3 <- ggboxplot(MateChoiceAnalysisInfected, x = "Group", y = "Male_Front_Seconds",  ylab = " Time (seconds)", xlab = "Male",
          ylim = c(0, 1100), fill = "Group", palette = c("snow3", "snow4")) + 
  scale_y_continuous(breaks=seq(0,900,by=100))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.title=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  annotate("text", x=1, y=615, label="x", size = 7)+
  annotate("text", x=2, y=634, label="x", size = 7)+
  geom_signif(y_position = c(1000), xmin = c(1), xmax = c(2),
              annotation = c("***"), tip_length = 0, textsize = 5)
mc3

## Means and standard deviations for boxplots
mean_Control <- MateChoiceAnalysisControl %>%
    group_by(Group, Type) %>% 
    summarise(mean= mean(Weight_Seconds),
              se = sd(Weight_Seconds))
glimpse(mean_Control)
  
mean_Infected <- MateChoiceAnalysisInfected %>%
    group_by(Group, Type) %>% 
    summarise(mean= mean(Male_Front_Seconds),
              se = sd(Male_Front_Seconds))
glimpse(mean_Infected)


ggarrange(mc1, mc2, mc3, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")

## Different way of trying to combine
MateChoiceGraph <- read.csv(file.choose())
MateChoiceGraph$Factor <- factor(MateChoiceGraph$Factor, levels = c("Female Choice", "Male Wander", "Male Front"))
ggplot(MateChoiceGraph, aes(x=Factor, y=Value, fill = Group))+
  geom_boxplot()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        legend.title= element_blank(),
        legend.position = "bottom")+
  scale_y_continuous(breaks=seq(0,900,by=100))+
  ylab("Time (seconds)")+
  expand_limits(y = 1100)+
  scale_fill_manual(values=c("snow3", "snow4"))+
  geom_signif(y_position = c(1000,1000,1000), xmin = c(0.8,1.8,2.8), xmax = c(1.2,2.2,3.2),
              annotation = c("***"), tip_length = 0, textsize = 5)+
  annotate("text", x=0.8, y=277, label="x", size = 5)+
  annotate("text", x=1.2, y=336, label="x", size = 5)+
  annotate("text", x=1.8, y=89, label="x", size = 5)+
  annotate("text", x=2.2, y=97, label="x", size = 5)+
  annotate("text", x=2.8, y=615, label="x", size = 5)+
  annotate("text", x=3.2, y=634, label="x", size = 5)

## Picture Analysis Data Files####

FrogImageData <- read.csv(file.choose())
FrogImageDataControl <- subset(FrogImageData, Frog_Type == "Control")
FrogImageDataControlDorsal <- subset(FrogImageDataControl,Dorsal_Ventral == "Dorsal")
FrogImageDataControlVentral <- subset(FrogImageDataControl,Dorsal_Ventral == "Ventral")
FrogImageDataInfected <- subset(FrogImageData, Frog_Type == "Infected")
FrogImageDataInfectedDorsal <- subset(FrogImageDataInfected,Dorsal_Ventral == "Dorsal")
FrogImageDataInfectedVentral <- subset(FrogImageDataInfected,Dorsal_Ventral == "Ventral")
FrogImageDataDorsal <- subset(FrogImageData, Dorsal_Ventral == "Dorsal")
FrogImageDataDorsalMale <- subset(FrogImageDataDorsal, Sex == "Male")
FrogImageDataDorsalMaleControl <- subset(FrogImageDataDorsalMale, Frog_Type == "Control")
FrogImageDataVentral <- subset(FrogImageData, Dorsal_Ventral == "Ventral")
FrogImageDataDorsalMaleInfected <- subset(FrogImageDataDorsalMale, Frog_Type == "Infected")
FrogImageDataDorsalMaleDay0 <- subset(FrogImageDataDorsalMale, Day_Bracket == "0")
FrogImageDataDorsalMaleDay12 <- subset(FrogImageDataDorsalMale, Day_Bracket == "1_2")
FrogImageDataDorsalMaleDay45 <- subset(FrogImageDataDorsalMale, Day_Bracket == "4_5")
FrogImageDataDorsalMaleDay78 <- subset(FrogImageDataDorsalMale, Day_Bracket == "7_8")
FrogImageDataDorsalMaleDay1011 <- subset(FrogImageDataDorsalMale, Day_Bracket == "10_11")
FrogImageDataDorsalMaleDay1314 <- subset(FrogImageDataDorsalMale, Day_Bracket == "13_14")
FrogImageDataDorsalMaleDay1618 <- subset(FrogImageDataDorsalMale, Day_Bracket == "16_18")
FrogImageDataDorsalMaleDay2021 <- subset(FrogImageDataDorsalMale, Day_Bracket == "20_21")
FrogImageDataDorsalMaleDay2223 <- subset(FrogImageDataDorsalMale, Day_Bracket == "22_23")
FrogImageDataDorsalMaleDay26 <- subset(FrogImageDataDorsalMale, Day_Bracket == "26")
FrogImageDataDorsalMaleDay2829 <- subset(FrogImageDataDorsalMale, Day_Bracket == "28_29")
FrogImageDataDorsalMaleDay3132 <- subset(FrogImageDataDorsalMale, Day_Bracket == "31_32")
FrogImageDataDorsalMaleDay35 <- subset(FrogImageDataDorsalMale, Day_Bracket == "35")
FrogImageDataDorsalMaleDay38 <- subset(FrogImageDataDorsalMale, Day_Bracket == "38")
FrogImageDataDorsalMaleDay41 <- subset(FrogImageDataDorsalMale, Day_Bracket == "41")
FrogImageDataDorsalMaleDay44 <- subset(FrogImageDataDorsalMale, Day_Bracket == "44")
FrogImageDataDorsalM2 <- subset(FrogImageDataDorsalMale, Frog_Number == "M2")
FrogImageDataDorsalM5 <- subset(FrogImageDataDorsalMale, Frog_Number == "M5")
FrogImageDataDorsalM6 <- subset(FrogImageDataDorsalMale, Frog_Number == "M6")
FrogImageDataDorsalM7 <- subset(FrogImageDataDorsalMale, Frog_Number == "M7")
FrogImageDataDorsalM8 <- subset(FrogImageDataDorsalMale, Frog_Number == "M8")
FrogImageDataDorsalM9 <- subset(FrogImageDataDorsalMale, Frog_Number == "M9")
FrogImageDataDorsalM11 <- subset(FrogImageDataDorsalMale, Frog_Number == "M11")
FrogImageDataDorsalM13 <- subset(FrogImageDataDorsalMale, Frog_Number == "M13")
FrogImageDataDorsalM14 <- subset(FrogImageDataDorsalMale, Frog_Number == "M14")
FrogImageDataDorsalM15 <- subset(FrogImageDataDorsalMale, Frog_Number == "M15")
FrogImageDataDorsalM16 <- subset(FrogImageDataDorsalMale, Frog_Number == "M16")
FrogImageDataDorsalM17 <- subset(FrogImageDataDorsalMale, Frog_Number == "M17")
FrogImageDataDorsalM18 <- subset(FrogImageDataDorsalMale, Frog_Number == "M18")
FrogImageDataDorsalM19 <- subset(FrogImageDataDorsalMale, Frog_Number == "M19")
FrogImageDataDorsalM20 <- subset(FrogImageDataDorsalMale, Frog_Number == "M20")
FrogImageDataDorsalM21 <- subset(FrogImageDataDorsalMale, Frog_Number == "M21")
FrogImageDataDorsalM22 <- subset(FrogImageDataDorsalMale, Frog_Number == "M22")
FrogImageDataDorsalM24 <- subset(FrogImageDataDorsalMale, Frog_Number == "M24")
FrogImageDataDorsalM25 <- subset(FrogImageDataDorsalMale, Frog_Number == "M25")
FrogImageDataDorsalM26 <- subset(FrogImageDataDorsalMale, Frog_Number == "M26")
FrogImageDataDorsalM27 <- subset(FrogImageDataDorsalMale, Frog_Number == "M27")
FrogImageDataDorsalM28 <- subset(FrogImageDataDorsalMale, Frog_Number == "M28")
FrogImageDataDorsalM29 <- subset(FrogImageDataDorsalMale, Frog_Number == "M29")
FrogImageDataDorsalM30 <- subset(FrogImageDataDorsalMale, Frog_Number == "M30")

## Preliminary look plots####
## Brightness
ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Average.Brightness, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataControlVentral, aes(x = Day, y = Average.Brightness, colour = Frog_Number)) +
  geom_line()
## might have a slight trend here

ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Average.Brightness, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Average.Brightness, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Log_Infection, y = Average.Brightness, colour = Frog_Number))+
  geom_line()
## need to add back in a couple of the infection frogs that I took out before
## I log-transformed the data


ggplot(FrogImageDataInfectedVentral, aes(x = Average.Brightness, y = Log_Infection, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.Brightness, colour = Frog_Type)) +
  geom_line()
ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.Brightness, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataVentral, aes(x = Day, y = Average.Brightness, colour = Frog_Type, group = Frog_Number)) +
  geom_line()

## Color Control Over Time
ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Proportion.R, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Proportion.G, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Proportion.B, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Average.R, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Average.G, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Average.B, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Redness.score, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Greeness.score, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataControlDorsal, aes(x = Day, y = Blueness.score, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataControlVentral, aes(x = Day, y = Proportion.R, colour = Frog_Number)) +
  geom_line()
## M22 potential outlier, consider removing for Day 26
ggplot(FrogImageDataControlVentral, aes(x = Day, y = Proportion.G, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlVentral, aes(x = Day, y = Proportion.B, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataControlVentral, aes(x = Day, y = Average.R, colour = Frog_Number)) +
  geom_line()
## M26 potential outlier for Day 23, consider removing
ggplot(FrogImageDataControlVentral, aes(x = Day, y = Average.G, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlVentral, aes(x = Day, y = Average.B, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataControlVentral, aes(x = Day, y = Redness.score, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlVentral, aes(x = Day, y = Greeness.score, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataControlVentral, aes(x = Day, y = Blueness.score, colour = Frog_Number)) +
  geom_line()
## no pattern
## Color Infected Over Time
ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Proportion.R, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Proportion.G, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Proportion.B, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Average.R, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Average.G, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Average.B, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Redness.score, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Greeness.score, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Day, y = Blueness.score, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Proportion.R, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Proportion.G, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Proportion.B, colour = Frog_Number)) +
  geom_line()
## no pattern

ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Average.R, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Average.G, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Average.B, colour = Frog_Number)) +
  geom_line()
## no pattern

ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Redness.score, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Greeness.score, colour = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataInfectedVentral, aes(x = Day, y = Blueness.score, colour = Frog_Number)) +
  geom_line()
## no pattern

## Control vs Infection Over Time Color
ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.R, colour = Frog_Type)) +
  geom_line()
ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.R, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.G, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.B, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.B, colour = Frog_Type))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataDorsal, aes(x = Day, y = Proportion.R, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataDorsal, aes(x = Day, y = Proportion.R, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataDorsal, aes(x = Day, y = Proportion.G, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataDorsal, aes(x = Day, y = Proportion.G, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataDorsal, aes(x = Day, y = Proportion.B, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Redness.score, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Greeness.score, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Blueness.score, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataVentral, aes(x = Day, y = Average.R, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataVentral, aes(x = Day, y = Average.G, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataVentral, aes(x = Day, y = Average.B, colour = Frog_Type, group = Frog_Number)) +
  geom_line()
ggplot(FrogImageDataVentral, aes(x = Day, y = Average.B, colour = Frog_Type, group = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataVentral, aes(x = Day, y = Proportion.G, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataVentral, aes(x = Day, y = Redness.score, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataVentral, aes(x = Day, y = Greeness.score, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

## Infected color vs. infection
ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Proportion.R, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Proportion.G, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Proportion.B, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Average.R, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Average.R, colour = Frog_Number)) +
  geom_line()+
  xlim(0,10000)

ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Average.G, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Average.B, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Redness.score, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Greeness.score, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Blueness.score, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Proportion.R, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Proportion.G, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Proportion.B, colour = Frog_Number)) +
  geom_line()



ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Average.R, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Average.G, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Average.B, colour = Frog_Number)) +
  geom_line()


ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Redness.score, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedVentral, aes(x = Log_Infection, y = Greeness.score)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Blueness.score, colour = Frog_Number)) +
  geom_line()


## Tests of normality Picture Analysis####

## Control
ggdensity(FrogImageDataDorsalMale$Average.Brightness)
ggqqplot(FrogImageDataDorsalMale$Average.Brightness)
## fairly normal
ggdensity(FrogImageDataControlVentral$Average.Brightness)
ggqqplot(FrogImageDataControlVentral$Average.Brightness)
## fairly normal
## bit of a dip in the middle but look fairly normal

ggdensity(FrogImageDataDorsalMale$Average.R)
ggqqplot(FrogImageDataDorsalMale$Average.R)
## fairly normal
ggdensity(FrogImageDataDorsal$Average.R)
ggqqplot(FrogImageDataDorsal$Average.R)

ggdensity(FrogImageDataDorsalMale$Average.G)
ggqqplot(FrogImageDataDorsalMale$Average.G)
## fairly normal
ggdensity(FrogImageDataDorsalMale$Average.B)
ggqqplot(FrogImageDataDorsalMale$Average.B)
## fairly normal

ggdensity(FrogImageDataControlVentral$Average.R)
ggqqplot(FrogImageDataControlVentral$Average.R)
## fairly normal
ggdensity(FrogImageDataControlVentral$Average.G)
ggqqplot(FrogImageDataControlVentral$Average.G)
## fairly normal
ggdensity(FrogImageDataControlVentral$Average.B)
ggqqplot(FrogImageDataControlVentral$Average.B)
## fairly normal

ggdensity(FrogImageDataControlDorsal$Proportion.R)
ggqqplot(FrogImageDataControlDorsal$Proportion.R)
## fairly normal
ggdensity(FrogImageDataControlDorsal$Proportion.G)
ggqqplot(FrogImageDataControlDorsal$Proportion.G)
## fairly normal
ggdensity(FrogImageDataControlDorsal$Proportion.B)
ggqqplot(FrogImageDataControlDorsal$Proportion.B)
## fairly normal

ggdensity(FrogImageDataControlVentral$Proportion.R)
ggqqplot(FrogImageDataControlVentral$Proportion.R)
## slightly skewed right
ggdensity(FrogImageDataControlVentral$Proportion.G)
ggqqplot(FrogImageDataControlVentral$Proportion.G)
## kinda skewed
ggdensity(FrogImageDataVentral$Proportion.G)
ggqqplot(FrogImageDataVentral$Proportion.G)

ggdensity(FrogImageDataControlVentral$Proportion.B)
ggqqplot(FrogImageDataControlVentral$Proportion.B)
## fairly normal

ggdensity(FrogImageDataControlDorsal$Redness.score)
ggqqplot(FrogImageDataControlDorsal$Redness.score)
## fairly normal
ggdensity(FrogImageDataControlDorsal$Greeness.score)
ggqqplot(FrogImageDataControlDorsal$Greeness.score)
## fairly normal
ggdensity(FrogImageDataDorsal$Greeness.score)
ggqqplot(FrogImageDataDorsal$Greeness.score)

ggdensity(FrogImageDataControlDorsal$Blueness.score)
ggqqplot(FrogImageDataControlDorsal$Blueness.score)
## fairly normal

ggdensity(FrogImageDataControlVentral$Redness.score)
ggqqplot(FrogImageDataControlVentral$Redness.score)
## fairly normal
ggdensity(FrogImageDataControlVentral$Greeness.score)
ggqqplot(FrogImageDataControlVentral$Greeness.score)
## fairly normal
ggdensity(FrogImageDataControlVentral$Blueness.score)
ggqqplot(FrogImageDataControlVentral$Blueness.score)
## fairly normal

## Infected
ggdensity(FrogImageDataInfectedDorsal$Average.Brightness)
ggqqplot(FrogImageDataInfectedDorsal$Average.Brightness)
## fairly normal
ggdensity(FrogImageDataInfectedVentral$Average.Brightness)
ggqqplot(FrogImageDataInfectedVentral$Average.Brightness)
## fairly normal

ggdensity(FrogImageDataInfectedDorsal$Average.R)
ggqqplot(FrogImageDataInfectedDorsal$Average.R)
## fairly normal
ggdensity(FrogImageDataInfectedDorsal$Average.G)
ggqqplot(FrogImageDataInfectedDorsal$Average.G)
## fairly normal
ggdensity(FrogImageDataInfectedDorsal$Average.B)
ggqqplot(FrogImageDataInfectedDorsal$Average.B)
## fairly normal

ggdensity(FrogImageDataInfectedVentral$Average.R)
ggqqplot(FrogImageDataInfectedVentral$Average.R)
## fairly normal
ggdensity(FrogImageDataInfectedVentral$Average.G)
ggqqplot(FrogImageDataInfectedVentral$Average.G)
## slight skewed right
ggdensity(FrogImageDataInfectedVentral$Average.B)
ggqqplot(FrogImageDataInfectedVentral$Average.B)
## fairly normal

ggdensity(FrogImageDataInfectedDorsal$Proportion.R)
ggqqplot(FrogImageDataInfectedDorsal$Proportion.R)
## fairly normal
ggdensity(FrogImageDataInfectedDorsal$Proportion.G)
ggqqplot(FrogImageDataInfectedDorsal$Proportion.G)
## fairly normal
ggdensity(FrogImageDataDorsal$Proportion.G)
ggqqplot(FrogImageDataDorsal$Proportion.G)
## DHARMa significant, so need to look at plot and see the issue

ggdensity(FrogImageDataInfectedDorsal$Proportion.B)
ggqqplot(FrogImageDataInfectedDorsal$Proportion.B)
## fairly normal

ggdensity(FrogImageDataInfectedVentral$Proportion.R)
ggqqplot(FrogImageDataInfectedVentral$Proportion.R)
## fairly normal
ggdensity(FrogImageDataInfectedVentral$Proportion.G)
ggqqplot(FrogImageDataInfectedVentral$Proportion.G)
## fairly normal
ggdensity(FrogImageDataInfectedVentral$Proportion.B)
ggqqplot(FrogImageDataInfectedVentral$Proportion.B)
## fairly normal

ggdensity(FrogImageDataInfectedDorsal$Redness.score)
ggqqplot(FrogImageDataInfectedDorsal$Redness.score)

ggdensity(FrogImageDataInfectedDorsal$Greeness.score)
ggqqplot(FrogImageDataInfectedDorsal$Greeness.score)

ggdensity(FrogImageDataInfectedDorsal$Blueness.score)
ggqqplot(FrogImageDataInfectedDorsal$Blueness.score)


ggdensity(FrogImageDataInfectedVentral$Redness.score)
ggqqplot(FFrogImageDataInfectedVentral$Redness.score)
## fairly normal
ggdensity(FrogImageDataVentral$Redness.score)
ggqqplot(FrogImageDataVentral$Redness.score)

ggdensity(FrogImageDataInfectedVentral$Greeness.score)
ggqqplot(FrogImageDataInfectedVentral$Greeness.score)
## fairly normal
ggdensity(FrogImageDataVentral$Greeness.score)
ggqqplot(FrogImageDataVentral$Greeness.score)

ggdensity(FrogImageDataInfectedVentral$Blueness.score)
ggqqplot(FrogImageDataInfectedVentral$Blueness.score)
## fairly normal

## Tests of Normality: combined data for Day
ggdensity(FrogImageDataDorsalMale$Average.Brightness)
ggqqplot(FrogImageDataDorsalMale$Average.Brightness)
## fairly normal, have a bit of a hump

ggdensity(FrogImageDataDorsalMale$Redness.score)
ggqqplot(FrogImageDataDorsalMale$Redness.score)
## normal

ggdensity(FrogImageDataDorsalMale$Greeness.score)
ggqqplot(FrogImageDataDorsalMale$Greeness.score)
## normal

ggdensity(FrogImageDataDorsalMale$Blueness.score)
ggqqplot(FrogImageDataDorsalMale$Blueness.score)
## normal

## Removed M19 D and V color May 31 because outlier 
## Removed M16 D and V Oct 15 because infection is outlier 
## Removed F10 D and V Oct 18 because infection is outlier 
## Removed M26 D and V color Oct 9 because outlier 
## Removed F5 brightness Oct 16 because outlier
## Separate Models by Control and Infected Frogs####
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(lmerTest)
library(emmeans)
library(multcomp)
library(performance)
install.packages("margins")
library(margins)
install.packages("foreign")
library(foreign)

## Control Models

BrightnessControlDorsal <- lmer(Average.Brightness~Day+(1|Frog_Number), data = FrogImageDataControlDorsal)
anova(BrightnessControlDorsal)
summary(BrightnessControlDorsal)
## neg, so over time getting less bright. Going to have Day as a factor in infected plots then

BrightnessInfectionDorsalInfection <- lmer(Average.Brightness~Infection+Day+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(BrightnessInfectionDorsalInfection)
summary(BrightnessInfectionDorsalInfection)
emmeans(BrightnessInfectionDorsalInfection, list (pairwise~Infection), lmer.df = "satterthwaite")

BrightnessInfectionDorsalDay <- lmer(Average.Brightness~Day+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(BrightnessInfectionDorsalDay)
summary(BrightnessInfectionDorsalDay)
## not actually significant for infected frogs, but need because control frogs had slight significance

RednessControlDorsal <- lmer(Redness.score~Day+(1|Frog_Number), data = FrogImageDataControlDorsal)
anova(RednessControlDorsal)
summary(RednessControlDorsal)
emmeans(RednessControlDorsal, list (pairwise~Day), lmer.df = "satterthwaite")
## redness in Control doesn't change

AverageRedControlDorsal <- lmer(Average.R~Day+(1|Frog_Number), data = FrogImageDataControlDorsal)
anova(AverageRedControlDorsal)
summary(AverageRedControlDorsal)
emmeans(AverageRedControlDorsal, list (pairwise~Day), lmer.df = "satterthwaite")
## average red increases over time, need to include Day in infection models

BrightnessInfectionDorsalDay <- lmer(Average.Brightness~Day+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(BrightnessInfectionDorsalDay)
summary(BrightnessInfectionDorsalDay)

BrightnessDorsalDay <- lmer(Average.Brightness~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(BrightnessDorsalDay)
summary(BrightnessDorsalDay)
emmeans(BrightnessDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")

## consider taking out frogs that didn't really get infected in infection trials (M2, M29, etc.)

## Models separated by question####
## Question 1: difference in frog attributes in control vs infected frogs

BrightnessDorsalDay <- lmer(Average.Brightness~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(BrightnessDorsalDay)
summary(BrightnessDorsalDay)
emmeans(BrightnessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## interaction is slightly not significant, so taking out interaction 
BrightnessDorsalDay <- lmer(Average.Brightness~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(BrightnessDorsalDay)
summary(BrightnessDorsalDay)
emmeans(BrightnessDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
r2_nakagawa(BrightnessDorsalDay)
## Conditional R2: 0.456, Marginal R2: 0.115

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Average.Brightness,
  fun = median,
  ylab = "Average Brightness",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))


## Interaction plot doesn't really cross, explains why barely not significant interaction effect 

BrightnessM19 <- lm(Average.Brightness~Day, data = FrogImageDataDorsalM19)
anova(BrightnessM19)
coef(BrightnessM19)
summary(BrightnessVentralDay)

BrightnessVentralDay <- lmer(Average.Brightness~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(BrightnessVentralDay)
summary(BrightnessVentralDay)
emmeans(BrightnessVentralDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## interaction is significant
r2_nakagawa(BrightnessVentralDay)
## Conditional: 0.6, marginal: 0.1
interaction.plot(
  x.factor = FrogImageDataVentral$Day,
  trace.factor = FrogImageDataVentral$Frog_Type,
  response = FrogImageDataVentral$Average.Brightness,
  fun = median,
  ylab = "Average Brightness",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))
## tends to follow same trend until days 13-16 when infected spikes above control 

AverageRDorsalDay <- lmer(Average.R~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(AverageRDorsalDay)
summary(AverageRDorsalDay)
emmeans(BrightnessDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## interaction is slightly not significant, so taking out interaction
AverageRDorsalDay <- lmer(Average.R~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(AverageRDorsalDay)
summary(AverageRDorsalDay)
emmeans(AverageRDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Day slightly not significant, so average R stays the same over time
## Frog type significant, average R higher for control versus infected
r2_nakagawa(AverageRDorsalDay)
## Conditional: 0.487, Marginal: 0.095

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Average.R,
  fun = median,
  ylab = "Average R",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))

AverageGDorsalDay <- lmer(Average.G~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number)-1, data = FrogImageDataDorsal)
anova(AverageGDorsalDay)
summary(AverageGDorsalDay)
emmeans(AverageGDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## interaction is not significant, so taking out interaction
AverageGDorsalDay <- lmer(Average.G~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(AverageGDorsalDay)
summary(AverageGDorsalDay)
emmeans(AverageGDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Day significant, average G decreases over time
## Frog type significant, average G higher for control versus infected
r2_nakagawa(AverageGDorsalDay)
## Conditional: 0.438, Marginal: 0.112

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Average.G,
  fun = median,
  ylab = "Average G",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))

AverageBDorsalDay <- lmer(Average.B~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(AverageBDorsalDay)
summary(AverageBDorsalDay)
emmeans(AverageGDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## interaction significant
## as days go up and control B goes up, so does infected B
## control B higher than infected B
r2_nakagawa(AverageBDorsalDay)
## Unable to calculate

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Average.B,
  fun = median,
  ylab = "Average B",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))

ProportionRDorsalDay <- lmer(Proportion.R~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(ProportionRDorsalDay)
summary(ProportionRDorsalDay)
## interaction not significant, taking interaction out of model
ProportionRDorsalDay <- lmer(Proportion.R~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(ProportionRDorsalDay)
summary(ProportionRDorsalDay)
## Day significant, higher proportion R as days go on
## Frog type not significant, no difference between proportion R in control vs infected
r2_nakagawa(ProportionRDorsalDay)
## Conditional: 0.101, Marginal: 0.099

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Proportion.R,
  fun = median,
  ylab = "Proportion R",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))


ProportionGDorsalDay <- lmer(Proportion.G~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(ProportionGDorsalDay)
summary(ProportionGDorsalDay)
emmeans(ProportionGDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## interaction significant, leaving in
r2_nakagawa(ProportionGDorsalDay)
## Conditional: 0.838, Marginal: 0.014

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Proportion.G,
  fun = median,
  ylab = "Proportion G",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))

ProportionBDorsalDay <- lmer(Proportion.B~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(ProportionBDorsalDay)
summary(ProportionBDorsalDay)
emmeans(ProportionBDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## interaction significant, leaving in
r2_nakagawa(ProportionBDorsalDay)
## Conditional: 0.113, Marginal: 0.061

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Proportion.B,
  fun = median,
  ylab = "Proportion B",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))


RednessDorsalDay <- lmer(Redness.score~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(RednessDorsalDay)
summary(RednessDorsalDay)
emmeans(RednessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## interaction not significant, removing
RednessDorsalDay <- lmer(Redness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(RednessDorsalDay)
summary(RednessDorsalDay)
emmeans(RednessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## not significant, redness doesn't change over time and is not different between frog types
r2_nakagawa(RednessDorsalDay)
## Conditional: 0.164, Marginal: 0.014

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Redness.score,
  fun = median,
  ylab = "Redness Score",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))

GreenessDorsalDay <- lmer(Greeness.score~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(GreenessDorsalDay)
summary(GreenessDorsalDay)
emmeans(GreenessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## interaction significant
## as day goes up and control greeness goes down, so does infection greeness
## control greeness is higher than infected greeness
r2_nakagawa(GreenessDorsalDay)
## Conditional: 0.613, Marginal: 0.092

interaction.plot(
  x.factor = FrogImageDataDorsal$Day,
  trace.factor = FrogImageDataDorsal$Frog_Type,
  response = FrogImageDataDorsal$Greeness.score,
  fun = median,
  ylab = "Greeness Score",
  xlab = "Day",
  trace.label = "Frog Type",
  col = c("#0198f9", "#f95801"))

BluenessDorsalDay <- lmer(Blueness.score~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(BluenessDorsalDay)
summary(BluenessDorsalDay)
emmeans(BluenessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## interaction not significant, taking out
BluenessDorsalDay <- lmer(Blueness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsal)
anova(BluenessDorsalDay)
summary(BluenessDorsalDay)
emmeans(BluenessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## neither day nor frog type is significant

AverageRVentralDay <- lmer(Average.R~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(AverageRVentralDay)
summary(AverageRVentralDay)
emmeans(AverageRVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## interaction not significant, taking out
AverageRVentralDay <- lmer(Average.R~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(AverageRVentralDay)
summary(AverageRVentralDay)
emmeans(AverageRVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Day not significant, frog type significant
## Average R higher in control than infected frogs

AverageGVentralDay <- lmer(Average.G~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(AverageGVentralDay)
summary(AverageGVentralDay)
emmeans(AverageGVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Interaction not significant, taking out
AverageGVentralDay <- lmer(Average.G~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(AverageGVentralDay)
summary(AverageGVentralDay)
emmeans(AverageGVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Both day and frog type significant
## As Day increases, Average G decreases
## Control Average G higher than infected Average G

AverageBVentralDay <- lmer(Average.B~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(AverageBVentralDay)
summary(AverageBVentralDay)
emmeans(AverageBVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Interaction significant, keeping in
## Positive 

ProportionRVentralDay <- lmer(Proportion.R~Day+Frog_Type+Day*Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(ProportionRVentralDay)
summary(ProportionRVentralDay)
emmeans(ProportionRVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Interaction not significant, taking out
ProportionRVentralDay <- lmer(Proportion.R~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(ProportionRVentralDay)
summary(ProportionRVentralDay)
emmeans(ProportionRVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## As Day increases, Proportion R increases
## No difference between control and infected frogs

ProportionGVentralDay <- lmer(Proportion.G~Day+Frog_Type*Day+(1|Frog_Number), data = FrogImageDataVentral)
anova(ProportionGVentralDay)
summary(ProportionGVentralDay)
emmeans(ProportionGVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Interaction significant, keeping in

ProportionBVentralDay <- lmer(Proportion.B~Day+Frog_Type*Day+(1|Frog_Number), data = FrogImageDataVentral)
anova(ProportionBVentralDay)
summary(ProportionBVentralDay)
emmeans(ProportionBVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Interaction significant, keeping in

RednessVentralDay <- lmer(Redness.score~Day+Frog_Type*Day+(1|Frog_Number), data = FrogImageDataVentral)
anova(RednessVentralDay)
summary(RednessVentralDay)
emmeans(RednessVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Interaction not significant, taking out
RednessVentralDay <- lmer(Redness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataVentral)
anova(RednessVentralDay)
summary(RednessVentralDay)
emmeans(RednessVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")

GreenessVentralDay <- lmer(Greeness.score~Day+Frog_Type*Day+(1|Frog_Number), data = FrogImageDataVentral)
anova(GreenessVentralDay)
summary(GreenessVentralDay)
emmeans(GreenessVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Interaction significant, keeping in

BluenessVentralDay <- lmer(Blueness.score~Day+Frog_Type*Day+(1|Frog_Number), data = FrogImageDataVentral)
anova(BluenessVentralDay)
summary(BluenessVentralDay)
emmeans(BluenessVentralDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
## Interaction significant, keeping in

BrightnessInfectionDorsal <- lmer(Average.Brightness~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(BrightnessInfectionDorsal)
summary(BrightnessInfectionDorsal)
emmeans(BrightnessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

BrightnessInfectionVentral <- lmer(Average.Brightness~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(BrightnessInfectionVentral)
summary(BrightnessInfectionVentral)
emmeans(BrightnessInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

AverageRInfectionDorsal <- lmer(Average.R~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(AverageRInfectionDorsal)
summary(AverageRInfectionDorsal)
emmeans(AverageRInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

AverageRInfectionVentral <- lmer(Average.R~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(AverageRInfectionVentral)
summary(AverageRInfectionVentral)
emmeans(AverageRInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

AverageGInfectionDorsal <- lmer(Average.G~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(AverageGInfectionDorsal)
summary(AverageGInfectionDorsal)
emmeans(AverageGInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

AverageGInfectionVentral <- lmer(Average.G~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(AverageGInfectionVentral)
summary(AverageGInfectionVentral)
emmeans(AverageGInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

AverageBInfectionDorsal <- lmer(Average.B~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(AverageBInfectionDorsal)
summary(AverageBInfectionDorsal)
emmeans(AverageBInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

AverageBInfectionVentral <- lmer(Average.B~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(AverageBInfectionVentral)
summary(AverageBInfectionVentral)
emmeans(AverageBInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

ProportionRInfectionDorsal <- lmer(Proportion.R~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(ProportionRInfectionDorsal)
summary(ProportionRInfectionDorsal)
emmeans(ProportionRInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

ProportionRInfectionVentral <- lmer(Proportion.R~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(ProportionRInfectionVentral)
summary(ProportionRInfectionVentral)
emmeans(ProportionRInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

ProportionGInfectionDorsal <- lmer(Proportion.G~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(ProportionGInfectionDorsal)
summary(ProportionGInfectionDorsal)
emmeans(ProportionGInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

ProportionGInfectionVentral <- lmer(Proportion.G~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(ProportionGInfectionVentral)
summary(ProportionGInfectionVentral)
emmeans(ProportionGInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

ProportionBInfectionDorsal <- lmer(Proportion.B~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(ProportionBInfectionDorsal)
summary(ProportionBInfectionDorsal)
emmeans(ProportionBInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

ProportionBInfectionVentral <- lmer(Proportion.B~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(ProportionBInfectionVentral)
summary(ProportionBInfectionVentral)
emmeans(ProportionBInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

RednessInfectionDorsal <- lmer(Redness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(RednessInfectionDorsal)
summary(RednessInfectionDorsal)
emmeans(RednessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

RednessInfectionVentral <- lmer(Redness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(RednessInfectionVentral)
summary(RednessInfectionVentral)
emmeans(RednessInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

GreenessInfectionDorsal <- lmer(Greeness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(GreenessInfectionDorsal)
summary(GreenessInfectionDorsal)
emmeans(GreenessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

GreenessInfectionVentral <- lmer(Greeness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(GreenessInfectionVentral)
summary(GreenessInfectionVentral)
emmeans(GreenessInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

BluenessInfectionDorsal <- lmer(Blueness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(BluenessInfectionDorsal)
summary(BluenessInfectionDorsal)
emmeans(BluenessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

BluenessInfectionVentral <- lmer(Blueness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedVentral)
anova(BluenessInfectionVentral)
summary(BluenessInfectionVentral)
emmeans(BluenessInfectionVentral, list (pairwise~Log_Infection), lmer.df = "satterthwaite")

## DHARMA####
install.packages("DHARMa")
library(DHARMa)

simsBrightnessDorsalDay <- simulateResiduals(BrightnessDorsalDay)
plot(simsBrightnessDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about 

simsBrightnessVentralDay <- simulateResiduals(BrightnessVentralDay)
plot(simsBrightnessVentralDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about 

simsAverageRDorsalDay <- simulateResiduals(AverageRDorsalDay)
plot(simsAverageRDorsalDay, quantreg = FALSE)
## Deviation is significant, so need to ID outliers
which(residuals(simsAverageRDorsalDay) == 1 | residuals(simsAverageRDorsalDay) == 0)
which(residuals(simsAverageRDorsalDay) >0.99 | residuals(simsAverageRDorsalDay) < 0.01)

simsAverageGDorsalDay <- simulateResiduals(AverageGDorsalDay)
plot(simsAverageGDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about 

simsAverageBDorsalDay <- simulateResiduals(AverageBDorsalDay)
plot(simsAverageBDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about 

simsProportionRDorsalDay <- simulateResiduals(ProportionRDorsalDay)
plot(simsProportionRDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsProportionGDorsalDay <- simulateResiduals(ProportionGDorsalDay)
plot(simsProportionGDorsalDay, quantreg = FALSE)
## Deviation is significant for KS test

simsProportionBDorsalDay <- simulateResiduals(ProportionBDorsalDay)
plot(simsProportionBDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsRednessDorsalDay <- simulateResiduals(RednessDorsalDay)
plot(simsRednessDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsGreenessDorsalDay <- simulateResiduals(GreenessDorsalDay)
plot(simsGreenessDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsBluenessDorsalDay <- simulateResiduals(BluenessDorsalDay)
plot(simsBluenessDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageRVentralDay <- simulateResiduals(AverageRVentralDay)
plot(simsAverageRVentralDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageGVentralDay <- simulateResiduals(AverageGVentralDay)
plot(simsAverageGVentralDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageBVentralDay <- simulateResiduals(AverageBVentralDay)
plot(simsAverageBVentralDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsProportionRVentralDay <- simulateResiduals(ProportionRVentralDay)
plot(simsProportionRVentralDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsProportionGVentralDay <- simulateResiduals(ProportionGVentralDay)
plot(simsProportionGVentralDay, quantreg = FALSE)
## Deviation is significant for KS test

simsProportionBVentralDay <- simulateResiduals(ProportionBVentralDay)
plot(simsProportionBVentralDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsRednessVentralDay <- simulateResiduals(RednessVentralDay)
plot(simsRednessVentralDay, quantreg = FALSE)
## just slightly significant, need to look for outliers 
simsRednessVentralDay

simsGreenessVentralDay <- simulateResiduals(GreenessVentralDay)
plot(simsGreenessVentralDay, quantreg = FALSE)
## Definitely significant

simsBluenessVentralDay <- simulateResiduals(BluenessVentralDay)
plot(simsBluenessVentralDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsBrightnessInfectionDorsal <- simulateResiduals(BrightnessInfectionDorsal)
plot(simsBrightnessInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsBrightnessInfectionVentral <- simulateResiduals(BrightnessInfectionVentral)
plot(simsBrightnessInfectionVentral, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageRInfectionDorsal <- simulateResiduals(AverageRInfectionDorsal)
plot(simsAverageRInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageRInfectionVentral <- simulateResiduals(AverageRInfectionVentral)
plot(simsAverageRInfectionVentral, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageGInfectionDorsal <- simulateResiduals(AverageGInfectionDorsal)
plot(simsAverageGInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageGInfectionVentral <- simulateResiduals(AverageGInfectionVentral)
plot(simsAverageGInfectionVentral, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageBInfectionDorsal <- simulateResiduals(AverageBInfectionDorsal)
plot(simsAverageBInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsAverageBInfectionVentral <- simulateResiduals(AverageBInfectionVentral)
plot(simsAverageBInfectionVentral, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsProportionRInfectionDorsal <- simulateResiduals(ProportionRInfectionDorsal)
plot(simsProportionRInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsProportionRInfectionVentral <- simulateResiduals(ProportionRInfectionVentral)
plot(simsProportionRInfectionVentral, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsProportionGInfectionDorsal <- simulateResiduals(ProportionGInfectionDorsal)
plot(simsProportionGInfectionDorsal, quantreg = FALSE)
## Definitely significant

simsProportionGInfectionVentral <- simulateResiduals(ProportionGInfectionVentral)
plot(simsProportionGInfectionVentral, quantreg = FALSE)
## Definitely significant

simsProportionBInfectionDorsal <- simulateResiduals(ProportionBInfectionDorsal)
plot(simsProportionBInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsProportionBInfectionVentral <- simulateResiduals(ProportionBInfectionVentral)
plot(simsProportionBInfectionVentral, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsRednessInfectionDorsal <- simulateResiduals(RednessInfectionDorsal)
plot(simsRednessInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsRednessInfectionVentral <- simulateResiduals(RednessInfectionVentral)
plot(simsRednessInfectionVentral, quantreg = FALSE)
## Definitely significant

simsGreenessInfectionDorsal <- simulateResiduals(GreenessInfectionDorsal)
plot(simsGreenessInfectionDorsal, quantreg = FALSE)
## Deviation is significant, need to figure that out
which(residuals(simsGreenessInfectionDorsal) == 1 | residuals(simsGreenessInfectionDorsal) == 0)
which(residuals(simsGreenessInfectionDorsal) >0.99 | residuals(simsGreenessInfectionDorsal) < 0.01)

simsGreenessInfectionVentral <- simulateResiduals(GreenessInfectionVentral)
plot(simsGreenessInfectionVentral, quantreg = FALSE)
## Definitely significant
which(residuals(simsGreenessInfectionVentral) == 1 | residuals(simsGreenessInfectionVentral) == 0)
which(residuals(simsGreenessInfectionVentral) >0.99 | residuals(simsGreenessInfectionVentral) < 0.01)
## Yusan Yang, ask about natural infection load

simsBluenessInfectionDorsal <- simulateResiduals(BluenessInfectionDorsal)
plot(simsBluenessInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

simsBluenessInfectionVentral <- simulateResiduals(BluenessInfectionVentral)
plot(simsBluenessInfectionVentral, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

## Consolidated models, plots and DARMAs for Question 1 and 2####
## Models using for Question 1: difference in frog attributes in control vs infected frogs
BrightnessDorsalDay <- lmer(Average.Brightness~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BrightnessDorsalDay)
anova(BrightnessDorsalDay) %>% as.data.frame() %>% write.csv(file = "BrightnessDorsalDay_anova_table.csv")
summary(BrightnessDorsalDay)
plot(BrightnessDorsalDay)
leveneTest(residuals(BrightnessDorsalDay) ~ FrogImageDataDorsalMale$Frog_Type)
emmeans(BrightnessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
## Interaction not significant
## Brightness decreases over days for both control and infected frogs and not different between frogs

AverageRDorsalDay <- lmer(Average.R~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(AverageRDorsalDay)
summary(AverageRDorsalDay)
plot(AverageRDorsalDay)
## Interaction not significant
## Average R for both slightly decrease over time

AverageGDorsalDay <- lmer(Average.G~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(AverageGDorsalDay)
summary(AverageGDorsalDay)
emmeans(AverageGDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
plot(AverageGDorsalDay)
## Interaction not significant
## Both decreasing over time, difference between control and infected frogs not significant

AverageBDorsalDay <- lmer(Average.B~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(AverageBDorsalDay)
summary(AverageBDorsalDay)
emmeans(AverageGDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
plot(AverageBDorsalDay)
## Interaction not significant
## Both decreaseing over time, difference between control and infected frogs not singificant

RednessDorsalDay <- lmer(Redness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(RednessDorsalDay)
summary(RednessDorsalDay)
emmeans(RednessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
plot(RednessDorsalDay)
## neither Day nor frog type is significant, overall redness score next to the
## standard red card in photos doesn't change over time and is not different
## between control and infected frogs

GreenessDorsalDay <- lmer(Greeness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(GreenessDorsalDay)
summary(GreenessDorsalDay)
emmeans(GreenessDorsalDay, list (pairwise~Frog_Type), lmer.df = "satterthwaite")
emmeans(GreenessDorsalDay, list (pairwise~Day), lmer.df = "satterthwaite")
plot(GreenessDorsalDay)
## Interaction not significant
## Greeness increases over time


BluenessDorsalDay <- lmer(Blueness.score~Day+Frog_Type+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BluenessDorsalDay)
summary(BluenessDorsalDay)
emmeans(BluenessDorsalDay, list (pairwise~Day*Frog_Type), lmer.df = "satterthwaite")
plot(BluenessDorsalDay)
## Interaction not significant
## Decrease over day

## Models using for Question 2: difference in frog attributes compared to log infection
BrightnessInfectionDorsal <- lmer(Average.Brightness~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BrightnessInfectionDorsal)
summary(BrightnessInfectionDorsal)
emmeans(BrightnessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(BrightnessInfectionDorsal)
## Brightness decreases as infection load increases

AverageRInfectionDorsal <- lmer(Average.R~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(AverageRInfectionDorsal)
summary(AverageRInfectionDorsal)
emmeans(AverageRInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(AverageRInfectionDorsal)
## Average R decreases as infection load increases

AverageGInfectionDorsal <- lmer(Average.G~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(AverageGInfectionDorsal)
summary(AverageGInfectionDorsal)
emmeans(AverageGInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(AverageGInfectionDorsal)
## Average G decreases as infection load increases

AverageBInfectionDorsal <- lmer(Average.B~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(AverageBInfectionDorsal)
summary(AverageBInfectionDorsal)
emmeans(AverageBInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(AverageBInfectionDorsal)
## no significance in Average B

RednessInfectionDorsal <- lmer(Redness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(RednessInfectionDorsal)
summary(RednessInfectionDorsal)
emmeans(RednessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(RednessInfectionDorsal)
## Redness score not significant

GreenessInfectionDorsal <- lmer(Greeness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(GreenessInfectionDorsal)
summary(GreenessInfectionDorsal)
emmeans(GreenessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(GreenessInfectionDorsal)
## not significant

BluenessInfectionDorsal <- lmer(Blueness.score~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMale)
anova(BluenessInfectionDorsal)
summary(BluenessInfectionDorsal)
emmeans(BluenessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(BluenessInfectionDorsal)
## not significant

## Plots using for Question 1: difference in frog attributes in control vs infected frogs
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.Brightness, colour = Frog_Type, line = Frog_Number))+
  geom_line()+
  geom_point()
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.Brightness, colour = Frog_Type))+
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.R, colour = Frog_Type, line = Frog_Number)) +
  geom_line()+
  geom_point()
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.R, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.G, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.G, colour = Frog_Type, line = Frog_Number)) +
  geom_point()+
  geom_line()
ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.B, colour = Frog_Type))+
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.B, colour = Frog_Type, line = Frog_Number))+
  geom_point()+
  geom_line()
ggplot(FrogImageDataDorsal, aes(x = Day, y = Redness.score, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Greeness.score, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataDorsal, aes(x = Day, y = Blueness.score, colour = Frog_Type)) +
  geom_point()+
  geom_smooth(method = "lm")

## Plots using for Question 2: difference in frog attributes compared to log infection
ggplot(FrogImageDataInfectedDorsal, aes(x = Log_Infection, y = Average.Brightness, colour = Frog_Number))+
  geom_line()
ggboxplot(FrogImageDataDorsalMale, x = "Day", y = "Average.R", color = "Frog_Type")
ggplot(FrogImageDataInfectedDorsal, aes(x = Log_Infection, y = Average.R)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataInfectedDorsal, aes(x = Log_Infection, y = Average.G)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataInfectedDorsal, aes(x = Log_Infection, y = Average.B)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataInfectedDorsal, aes(x = Log_Infection, y = Redness.score)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataInfectedDorsal, aes(x = Log_Infection, y = Greeness.score)) +
  geom_point()+
  geom_smooth(method = "lm")
ggplot(FrogImageDataInfectedDorsal, aes(x = Log_Infection, y = Blueness.score)) +
  geom_point()+
  geom_smooth(method = "lm")

## Average R Dorsal over day is significant in DARMa, so removed M19 color information completely 
## Greeness over infection is significant in DARMa, so removed 5 entry's Greeness scores, notes on excel file

## Question 1 DHARMas for all needed models after outliers removed (REMEMBER TO RERUN MODELS!)
simsBrightnessDorsalDay <- simulateResiduals(BrightnessDorsalDay)
plot(simsBrightnessDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about 
simsAverageRDorsalDay <- simulateResiduals(AverageRDorsalDay)
plot(simsAverageRDorsalDay, quantreg = FALSE)
## No deviation anymore
simsAverageGDorsalDay <- simulateResiduals(AverageGDorsalDay)
plot(simsAverageGDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about 
simsAverageBDorsalDay <- simulateResiduals(AverageBDorsalDay)
plot(simsAverageBDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about 
simsRednessDorsalDay <- simulateResiduals(RednessDorsalDay)
plot(simsRednessDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsGreenessDorsalDay <- simulateResiduals(GreenessDorsalDay)
plot(simsGreenessDorsalDay, quantreg = FALSE)
## Deviation just barely significant but not worried
simsBluenessDorsalDay <- simulateResiduals(BluenessDorsalDay)
plot(simsBluenessDorsalDay, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

## Question 2 DHARMas for all needed models after outliers removed (REMEMBER TO RERUN MODELS!)
simsBrightnessInfectionDorsal <- simulateResiduals(BrightnessInfectionDorsal)
plot(simsBrightnessInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsAverageRInfectionDorsal <- simulateResiduals(AverageRInfectionDorsal)
plot(simsAverageRInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsAverageGInfectionDorsal <- simulateResiduals(AverageGInfectionDorsal)
plot(simsAverageGInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsAverageBInfectionDorsal <- simulateResiduals(AverageBInfectionDorsal)
plot(simsAverageBInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsRednessInfectionDorsal <- simulateResiduals(RednessInfectionDorsal)
plot(simsRednessInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsGreenessInfectionDorsal <- simulateResiduals(GreenessInfectionDorsal)
plot(simsGreenessInfectionDorsal, quantreg = FALSE)
## Deviation but not bad so keeping it
simsBluenessInfectionDorsal <- simulateResiduals(BluenessInfectionDorsal)
plot(simsBluenessInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about


## Separate models for each day or nest frog number 
## Ian, ask him about models 
## separate infection models by day as well
## box and whisker plots 

##Day 7-10 does treatment impact dorsal coloration, can also bracket it 
## correlation at same point in time in dorsal coloration
## From Days 0-10, are coloration values differing between control and infected frogs?
## Test movement of control vs infected frog, maybe why she prefers?

## Coloration Boxplots and Line Graphs for Day####
ggboxplot(FrogImageDataDorsalMale, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type", palette = c("black","darkgrey"),
          select = c("0", "7_8", "10_11", "13_14","20_21", "22_23", "28_29",
                     "31_32", "35"),
          order = c("0", "7_8", "10_11", "13_14","20_21", "22_23", "28_29",
                    "31_32", "35"),
          ylab = " Average Brightness (%)", xlab = "Day Bracket", ylim = c(24, 48)) +
scale_y_continuous(breaks=seq(25,45,by=5))+
scale_x_discrete(labels=c("0", "7/8", "10/11", "13/14", "20/21", "22/23", "28/29", "31/32", "35"))+
theme(legend.title=element_blank())+
theme(legend.position = c(0.8, 0.9), legend.direction = "horizontal")

ggboxplot(FrogImageDataDorsalMale, x = "Day_Bracket", y = "Average.R", color = "Frog_Type", palette = c("darkred","red"),
          select = c("0", "7_8", "10_11", "13_14","20_21", "22_23", "28_29",
                     "31_32"),
          order = c("0", "7_8", "10_11", "13_14","20_21", "22_23", "28_29",
                    "31_32"),
          ylab = " Average Red Pixels", xlab = "Day Bracket", ylim = c(95, 170)) +
  scale_y_continuous(breaks=seq(95,165,by=10))+
  scale_x_discrete(labels=c("0", "7/8", "10/11", "13/14", "20/21", "22/23", "28/29", "31/32"))+
  theme(legend.title=element_blank())+
  theme(legend.position = c(0.8, 0.9), legend.direction = "horizontal")

ggboxplot(FrogImageDataDorsalMale, x = "Day_Bracket", y = "Average.G", color = "Frog_Type", palette = c("darkgreen","green"),
          select = c("0", "7_8", "10_11", "13_14","20_21", "22_23", "28_29",
                     "31_32"),
          order = c("0", "7_8", "10_11", "13_14","20_21", "22_23", "28_29",
                    "31_32"),
          ylab = " Average Green Pixels", xlab = "Day Bracket", ylim = c(30, 110)) +
  scale_y_continuous(breaks=seq(30,100,by=10))+
  scale_x_discrete(labels=c("0", "7/8", "10/11", "13/14", "20/21", "22/23", "28/29", "31/32"))+
  theme(legend.title=element_blank())+
  theme(legend.position = c(0.8, 0.9), legend.direction = "horizontal")

ggboxplot(FrogImageDataDorsalMale, x = "Day_Bracket", y = "Average.B", color = "Frog_Type", palette = c("blue","turquoise"),
          select = c("0", "7_8", "10_11", "13_14","20_21", "22_23", "28_29",
                     "31_32"),
          order = c("0", "7_8", "10_11", "13_14","20_21", "22_23", "28_29",
                    "31_32"),
          ylab = " Average Blue Pixels", xlab = "Day Bracket", ylim = c(0, 60)) +
  scale_y_continuous(breaks=seq(0,50,by=10))+
  scale_x_discrete(labels=c("0", "7/8", "10/11", "13/14", "20/21", "22/23", "28/29", "31/32"))+
  theme(legend.title=element_blank())+
  theme(legend.position = c(0.8, 0.9), legend.direction = "horizontal")

ggboxplot(FrogImageDataDorsalMale, x = "Day_Bracket", y = "Log_Infection",
          order = c("0", "1_2", "4_5", "7_8", "10_11", "13_14", "20_21", "22_23","28_29",
                    "31_32"))
## Going to test Day 0, Day 1_2, Day 4_5, Day 7_8, Day 10_11, Day 13_14, Day 20_21, Day 22_23, Day 31_32

ggboxplot(FrogImageDataDorsalMale, x = "Day_Bracket", y = "Log_Infection", facet.by = "Number.of.Doses",
          order = c("0", "1_2", "4_5", "7_8", "10_11", "13_14", "16_18", "20_21", "22_23", "26", "28_29",
                    "31_32", "35", "38", "41", "44"))

## Line Graphs for color change over time


BrightnessLineDay <- ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.Brightness, colour = Frog_Type, line = Frog_Number)) +
  geom_line()+
  geom_point(size=0.75)+
  labs(y = "Average Brigtness (%)", x = "Day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.position="top")+
  xlim(0,50)+
  ylim(20,45)+
  scale_color_manual(values = c("black", "darkgrey"))+
  theme(legend.title=element_blank())+
  annotate("text", x=35, y=43, label= "F: 21.31 ; DF: 1,182.30 ; p <0.001", fontface = "bold", size = 2)+
  annotate("text", x=35, y=45, label= " Day", fontface = "bold", size = 2)
BrightnessLineDay

RedLineDay <- ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.R, colour = Frog_Type, line = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Average Red Pixels", x = "Day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0, 45, by = 5))+
  scale_y_continuous(breaks = seq(95, 165, by = 10))+
  expand_limits(y = 165)+
  expand_limits(x = 45)+
  scale_color_manual(values = c("darkred", "red"))+
  theme(legend.title=element_blank())+
  annotate("text", x=35, y=165, label= "F: 13.56 ; DF: 1,181.33 ; p <0.001", fontface = "bold", size = 2)+
  annotate("text", x=35, y=170, label= " Day", fontface = "bold", size = 2)
RedLineDay

GreenLineDay <- ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.G, colour = Frog_Type, line = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Average Green Pixels", x = "Day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0, 45, by = 5))+
  scale_y_continuous(breaks = seq(30, 100, by = 10))+
  expand_limits(y = 100)+
  expand_limits(x = 45)+
  scale_color_manual(values = c("darkgreen", "green"))+
  theme(legend.title=element_blank())+
  annotate("text", x=35, y=96, label= "F: 32.01 ; DF: 1,181.58 ; p <0.001", fontface = "bold", size = 2)+
  annotate("text", x=35, y=100, label= " Day", fontface = "bold", size = 2)
GreenLineDay

BlueLineDay <-ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Average.B, colour = Frog_Type, line = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Average Blue Pixels", x = "Day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0, 45, by = 5))+
  scale_y_continuous(breaks = seq(0, 50, by = 10))+
  expand_limits(y = 50)+
  expand_limits(x = 45)+
  scale_color_manual(values = c("darkblue", "blue"))+
  theme(legend.title=element_blank())+
  annotate("text", x=35, y=48, label= "F: 16.49 ; DF: 1,191 ; p <0.001", fontface = "bold", size = 2)+
  annotate("text", x=35, y=51, label= " Day", fontface = "bold", size = 2)
BlueLineDay

RednessLineDay <- ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Redness.score, colour = Frog_Type, line = Frog_Number)) +
  geom_line()+
  geom_point(size=0.75)+
  labs(y = "Dorsal Redness (Rdorsum/Rstandard)", x = "Day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.position="top")+
  xlim(0,50)+
  ylim(0.75,1.2)+
  scale_color_manual(values = c("darkred", "red"))+
  theme(legend.title=element_blank())+
  annotate("text", x=35, y=1.15, label= "F < 0.01 ; DF: 1,192.00 ; p = 0.99", fontface = "bold", size = 2)+
  annotate("text", x=35, y=1.2, label= " Day", fontface = "bold", size = 2)
RednessLineDay

GreenessLineDay <- ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Greeness.score, colour = Frog_Type, line = Frog_Number)) +
  geom_line()+
  geom_point(size=0.75)+
  labs(y = "Dorsal Greenness (Gdorsum/Gstandard)", x = "Day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.position="top")+
  ylim(1,2)+
  xlim(0,50)+
  scale_color_manual(values = c("darkgreen", "green"))+
  theme(legend.title=element_blank())+
  annotate("text", x=35, y=1.9, label= "F: 27.31 ; DF: 1,179.76 ; p < 0.001", fontface = "bold", size = 2)+
  annotate("text", x=35, y=2, label= " Day", fontface = "bold", size = 2)
GreenessLineDay

BluenessLineDay <-ggplot(FrogImageDataDorsalMale, aes(x = Day, y = Blueness.score, colour = Frog_Type, line = Frog_Number)) +
  geom_line()+
  geom_point(size=0.75)+
  labs(y = "Dorsal Blueness (Bdorsum/Bstandard)", x = "Day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.position="top")+
  xlim(0,50)+
  ylim(0,1.2)+
  scale_color_manual(values = c("darkblue", "blue"))+
  theme(legend.title=element_blank())+
  annotate("text", x=35, y=1.1, label= "F: 8.41 ; DF: 1,185.17 ; p < 0.01", fontface = "bold", size = 2)+
  annotate("text", x=35, y=1.2, label= " Day", fontface = "bold", size = 2)
BluenessLineDay

library(patchwork)
(BrightnessLineDay | RedLineDay)/
  (GreenLineDay | BlueLineDay)

(BrightnessLineDay | RednessLineDay)/
  (GreenessLineDay | BluenessLineDay)

## Line Graphs for infection coloration####
BrightnessLineInfection <-ggplot(FrogImageDataDorsalMaleInfected, aes(x = Log_Infection, y = Average.Brightness, colour = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Average Brigtness (%)", x = "Bd Infection (log transformed)")+
  labs(color = "Frog ID")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7))+
  ylim(20,45)+
  xlim(0,8)+
  annotate("text", x=6, y=43.5, label= "F: 7.79 ; DF: 1,76.08 ; p < 0.01", fontface = "bold", size = 2)+
  annotate("text", x=6, y=45, label= "Log Infection", fontface = "bold", size = 2)
BrightnessLineInfection

RedLineInfection <-ggplot(FrogImageDataDorsalMaleInfected, aes(x = Log_Infection, y = Average.R, colour = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Average Red Pixels", x = "Bd Infection (log transformed)")+
  labs(color = "Frog ID")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0, 8, by = 1))+
  scale_y_continuous(breaks = seq(80, 170, by = 10))+
  annotate("text", x=5, y=165, label= "F: 9.01 ; DF: 1,83.26 ; p <0.01", fontface = "bold", size = 2)+
  annotate("text", x=5, y=170, label= "Log Infection", fontface = "bold", size = 2)
RedLineInfection

GreenLineInfection <-ggplot(FrogImageDataDorsalMaleInfected, aes(x = Log_Infection, y = Average.G, colour = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Average Green Pixels", x = "Bd Infection (log transformed)")+
  labs(color = "Frog ID")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0, 8, by = 1))+
  scale_y_continuous(breaks = seq(0, 90, by = 10))+
  expand_limits(x = 7)+
  expand_limits(y = c(30,90))+
  annotate("text", x=5, y=90, label= "F: 11.83 ; DF: 1,86.29 ; p <0.001", fontface = "bold", size = 2)+
  annotate("text", x=5, y=95, label= " Log Infection", fontface = "bold", size = 2)
GreenLineInfection

BlueLineInfection <-ggplot(FrogImageDataDorsalMaleInfected, aes(x = Log_Infection, y = Average.B, colour = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Average Blue Pixels", x = "Bd Infection (log transformed)")+
  labs(color = "Frog ID")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(0, 8, by = 1))+
  scale_y_continuous(breaks = seq(0, 50, by = 10))+
  expand_limits(x = 7)+
  expand_limits(y = c(0,50))+
  annotate("text", x=5, y=47, label= "F: 1.57 ; DF: 1,88 ; p = 0.21", fontface = "bold", size = 2)+
  annotate("text", x=5, y=50, label= "Log Infection", fontface = "bold", size = 2)
BlueLineInfection

RednessLineInfection <-ggplot(FrogImageDataDorsalMaleInfected, aes(x = Log_Infection, y = Redness.score, colour = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Dorsal Redness (Rdorsum/Rstandard)", x = "Bd Infection (log transformed)")+
  labs(color = "Frog ID")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7))+
  ylim(0.75,1.2)+
  xlim(0,8)+
  annotate("text", x=6, y=1.17, label= "F: 0.02 ; DF: 1,78.71 ; p = 0.90", fontface = "bold", size = 2)+
  annotate("text", x=6, y=1.2, label= "Log Infection", fontface = "bold", size = 2)
RednessLineInfection

GreennessLineInfection <-ggplot(FrogImageDataDorsalMaleInfected, aes(x = Log_Infection, y = Greeness.score, colour = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Dorsal Greenness (Gdorsum/Gstandard)", x = "Bd Infection (log transformed)")+
  labs(color = "Frog ID")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7))+
  ylim(1,2)+
  xlim(0,8)+
  annotate("text", x=6, y=1.93, label= "F: 1.93 ; DF: 1,86.02 ; p = 0.17", fontface = "bold", size = 2)+
  annotate("text", x=6, y=2, label= "Log Infection", fontface = "bold", size = 2)
GreennessLineInfection

BluenessLineInfection <-ggplot(FrogImageDataDorsalMaleInfected, aes(x = Log_Infection, y = Blueness.score, colour = Frog_Number)) +
  geom_line()+
  geom_point()+
  labs(y = "Dorsal Blueness (Bdorsum/Bstandard)", x = "Bd Infection (log transformed)")+
  labs(color = "Frog ID")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7))+
  ylim(0,1.5)+
  xlim(0,8)+
  annotate("text", x=6, y=1.4, label= "F: 0.01 ; DF: 1,88 ; p = 0.91", fontface = "bold", size = 2)+
  annotate("text", x=6, y=1.5, label= "Log Infection", fontface = "bold", size = 2)
BluenessLineInfection

ggarrange(BrightnessLineInfection, RedLineInfection, GreenLineInfection, BlueLineInfection, ncol=2, nrow=2, common.legend = TRUE, legend="right")
ggarrange(BrightnessLineInfection, RednessLineInfection, GreennessLineInfection, BluenessLineInfection, ncol=2, nrow=2, common.legend = TRUE, legend="right")

## T-tests by day coloration####
## Question 1: difference in frog attributes in control vs infected frogs by day
ggdensity(FrogImageDataDorsalMaleDay0$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay0$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay0)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay0, var.equal = TRUE)
## At day 0, not statistically different

ggdensity(FrogImageDataDorsalMaleDay0$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay0$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay0)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay0, var.equal = FALSE)
## At day 0, not statistically different

ggdensity(FrogImageDataDorsalMaleDay0$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay0$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay0)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay0, var.equal = FALSE)
## At day 0, not statistically different

ggdensity(FrogImageDataDorsalMaleDay0$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay0$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay0)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay0, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay0, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 0, not statistically different

ggdensity(FrogImageDataDorsalMaleDay12$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay12$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay12)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay12, var.equal = TRUE)
## At day 1_2, just barely significant. Infected less bright than control

ggdensity(FrogImageDataDorsalMaleDay12$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay12$Average.R)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay12)
wilcox.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay12, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay12, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At Day 1_2, not statistically different 

ggdensity(FrogImageDataDorsalMaleDay12$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay12$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay12)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay12, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay12, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At Day 1_2, Infected G is lower than control

ggdensity(FrogImageDataDorsalMaleDay12$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay12$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay12)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay12, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay12, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At Day 1_2, Not statistically different

ggdensity(FrogImageDataDorsalMaleDay45$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay45$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay45)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay45, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay45, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 4_5, not significant

ggdensity(FrogImageDataDorsalMaleDay45$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay45$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay45)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay45, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay45, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 4_5, not significant

ggdensity(FrogImageDataDorsalMaleDay45$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay45$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay45)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay45, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay45, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 4_5, not significant

ggdensity(FrogImageDataDorsalMaleDay45$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay45$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay45)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay45, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay45, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 4_5, Average B lower in infected than control

ggdensity(FrogImageDataDorsalMaleDay78$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay78$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay78)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay78, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay78, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 7_8, not significant

ggdensity(FrogImageDataDorsalMaleDay78$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay78$Average.R)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay78)
wilcox.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay78, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay78, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 7_8, not significant

ggdensity(FrogImageDataDorsalMaleDay78$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay78$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay78)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay78, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay78, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 7_8, Infected are less green than control

ggdensity(FrogImageDataDorsalMaleDay78$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay78$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay78)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay78, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay78, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 7_8, not significant

ggdensity(FrogImageDataDorsalMaleDay1011$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay1011$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1011, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 10_11, not significant

ggdensity(FrogImageDataDorsalMaleDay1011$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay1011$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1011, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 10_11, not significant

ggdensity(FrogImageDataDorsalMaleDay1011$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay1011$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1011, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 10_11, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, var.equal = FALSE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, var.equal = FALSE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, var.equal = FALSE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.B)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
wilcox.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.B)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
wilcox.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay2021$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay2021$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2021, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 20_21 brightness for infected frogs is higher

ggdensity(FrogImageDataDorsalMaleDay2021$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay2021$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2021, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 20_21 Average R for infected frogs is higher

ggdensity(FrogImageDataDorsalMaleDay2021$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay2021$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2021, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 20_21 Average G is not different

ggdensity(FrogImageDataDorsalMaleDay2021$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay2021$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2021, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 20_21 Average B is not different

ggdensity(FrogImageDataDorsalMaleDay2223$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay2223$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2223, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 22_23 Average Brightness for infected is lower

ggdensity(FrogImageDataDorsalMaleDay2223$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay2223$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2223, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 22_23 Average R for infected is lower

ggdensity(FrogImageDataDorsalMaleDay2223$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay2223$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2223, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 22_23 Average G for infected is lower

ggdensity(FrogImageDataDorsalMaleDay2223$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay2223$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2223, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 22_23 Average B is not different

ggdensity(FrogImageDataDorsalMaleDay3132$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay3132$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay3132, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 31_32 Average Brightness is lower for infected

ggdensity(FrogImageDataDorsalMaleDay3132$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay3132$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay3132, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 31_32 Average R is lower for infected

ggdensity(FrogImageDataDorsalMaleDay3132$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay3132$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay3132, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 31_32 Average G is lower for infected

ggdensity(FrogImageDataDorsalMaleDay3132$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay3132$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132, var.equal = FALSE)
ggboxplot(FrogImageDataDorsalMaleDay3132, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 31_32 Average B is higher for infected

ggdensity(FrogImageDataDorsalMaleDay2829$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay2829$Average.Brightness)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829)
wilcox.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2829, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 28_29 not different

ggdensity(FrogImageDataDorsalMaleDay2829$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay2829$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2829, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 28_29 not different

ggdensity(FrogImageDataDorsalMaleDay2829$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay2829$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2829, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 28_29 Average G lower in infected

ggdensity(FrogImageDataDorsalMaleDay2829$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay2829$Average.B)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829)
wilcox.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2829, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 28_29 Average B lower in infected

## Important days for mate choice are 20_21, 22_23, 28_29, 31_32
## Overall during this time if there is a difference it is mostly that infected is lower than 
## control, however females spent more time near infected frogs in mate choice trials

## Not going to do across days because don't have enough data similar from same days to make meaningful tests. Will
## only comment that on Day 0 measurements aren't different between groups but on other days they are

## Coloration vs log infection models again####
## Models using for Question 2: difference in frog attributes compared to log infection
BrightnessInfectionDorsal <- lmer(Average.Brightness~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(BrightnessInfectionDorsal)
summary(BrightnessInfectionDorsal)
emmeans(BrightnessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(BrightnessInfectionDorsal)
## Brightness decreases as infection load increases

AverageRInfectionDorsal <- lmer(Average.R~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(AverageRInfectionDorsal)
summary(AverageRInfectionDorsal)
emmeans(AverageRInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
## Average R decreases as infection load increases

AverageGInfectionDorsal <- lmer(Average.G~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(AverageGInfectionDorsal)
summary(AverageGInfectionDorsal)
emmeans(AverageGInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
## Average G decreases as infection load increases

AverageBInfectionDorsal <- lmer(Average.B~Log_Infection+(1|Frog_Number), data = FrogImageDataInfectedDorsal)
anova(AverageBInfectionDorsal)
summary(AverageBInfectionDorsal)
emmeans(AverageBInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
## no significance in Average B

## For Question 2, it doesn't matter what day they were seen with this infection load, it just matters that this was
## their load at that point in time and it corresponded to that color or brightness, and we see a trend for all of these 
## variables that as infection increases coloration decreases and so does dorsal brightness, except for Average B

## Cori Comments from talking about carotenoids####
##Carotenoids helps with both color and immune so might expect fewer carotenoids in color if trying to use for immune or might not have enough to do both 

##Mention in carotenoid paper that frogs euthanized was happening around day 20-30 when I was seeing some color change, go back and actually document. Also frogs that died early were experiencing color change,
## or hitting day 13_14. Might take out frogs that died super early and compare results for just them if they seem strange.
## Also have M24, M17 and M28 in data where they died months before others, same with WF2 died on month 9 thought could argue that had been on rich diet
## for so long it would have been even keel already and euthanized past day 20_21
##Ordination style multivariate to see if composition of all between groups is different 
##-Multivariate analysis of variance (MANOVA)
##-Vegan package meant for this type of ordination analyses

##-Look at wild frogs versus supplemented frogs output (Leila Freeborn dissertation, and Justin one), they should have done MANOVAs 

##Reason supplement is to try and keep coloration similar to what is in the wild but the gut-loaded didn’t change color at all but seems to be healthier frogs with carotenoid supplementation 
##-look up cori supplementation paper, don’t know where exactly these are coming so can’t say exactly what is it becoming
##-can say carotenoids are not limiting resource, same playing field



## Models for Ian to look at####

## Packages
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)
library(lmtest)
library(car)
library(DHARMa)

## First model set: Testing difference in time female spent closer to Bd-clean male versus Bd-infected male
## Weight_Seconds: time in seconds female spent near respective male
## Group: Male frog type (Bd-clean and Bd-infected)
## Females went through trials with each of 9 male frog pairs, so repeated measures both for female (Frog_Number) and male pair (Male_Pair_Letter)
## For glm, weighted by time female spent near both males versus the rest of the behavior chamber (ex. if female spent all 15 minutes near males, weighted high. If female spent only a little time near males, weighted lower)
MateChoiceAnalysisInfected <- read.csv(file.choose())
ggdensity(MateChoiceAnalysisInfected$Weight_Seconds_Female, main = "Density Plot of Weight Seconds", xlab = " Weight Seconds")
ggqqplot(MateChoiceAnalysisInfected$Weight_Seconds_Female)
## not normal

## Try with SQRT seconds
ggdensity(MateChoiceAnalysisInfected$SQRT_Weight_Seconds, main = "Density Plot of SQRT Weight Seconds", xlab = " SQRT Weight Seconds")
ggqqplot(MateChoiceAnalysisInfected$SQRT_Weight_Seconds)
## more normalized

## Tried LMER
InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+(1|Male_Pair_Letter)+(1|Frog_Number) , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)
plot(InfectedTrialLMER)
## Heteroscedasticity check for model
simsInfectedTrialLMER <- simulateResiduals(InfectedTrialLMER)
plot(simsInfectedTrialLMER, quantreg = FALSE)

## Check linearity of data
ggqqplot(residuals(InfectedTrialLMER))
plot(InfectedTrialLMER)

## Not linear, so then tried weighted glmer
InfectedTrialLMER <- glmer(Weight_Seconds_Female~Group+(1|Male_Pair_Letter)+(1|Frog_Number), data = MateChoiceAnalysisInfected, family = "poisson", weights = wt)
anova(InfectedTrialLMER)
car::Anova(InfectedTrialLMER)
summary(InfectedTrialLMER)
plot(InfectedTrialLMER)
ggqqplot(residuals(InfectedTrialLMER))
emmeans(InfectedTrialLMER, list (pairwise~Group), lmer.df = "satterthwaite")

## Boxplot for these data
MateChoiceAnalysisInfected$Group <- factor(MateChoiceAnalysisInfected$Group, levels = c("Clean", "Infected"))
ggboxplot(MateChoiceAnalysisInfected, x = "Group", y = "Weight_Seconds_Female",  ylab = " Time (seconds)", xlab = "Male",
          ylim = c(0, 1200), fill = "darkgrey") + 
  scale_x_discrete(labels=c("Clean", "Infected"))+
  scale_y_continuous(breaks=seq(0,900,by=100))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.title=element_blank())

hist(log(MateChoiceAnalysisInfected$Weight_Seconds_Female))
## Questions for Ian about first model set:
## 1. Am I setting up repeated measures correctly? I've read about nesting random effects and I tried it but the models failed
## 2. Even after converting to glm, still heteroscedasticity and can't do DARMA with weighted data
## 3. If I don't weight the glm, the DHARMa shows incredible heteroscedasticity, so not sure how best to do this
## 4. Boxplot doesn't look significant like the glm says it is

## Second set of models: testing difference in time Bd-clean versus Bd-infected males spent wandering and at the front of the behavior apparatus 

ggdensity(MateChoiceAnalysisInfected$Male_Wander_Seconds, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Wander_Seconds)
hist(MateChoiceAnalysisInfected$Male_Wander_Seconds)

## first lmer for male wander
InfectedTrialLMERMaleWander <- lmer(Male_Wander_Seconds~Group+(1|Male_Pair_Letter)+(1|Frog_Number), data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMERMaleWander)
summary(InfectedTrialLMERMaleWander)
plot(InfectedTrialLMERMaleWander)
ggqqplot(residuals(InfectedTrialLMERMaleWander))

simsInfectedTrialLMERMaleWander <- simulateResiduals(InfectedTrialLMERMaleWander)
plot(simsInfectedTrialLMERMaleWander, quantreg = FALSE)

## then glm

InfectedTrialLMERMaleWander <- glmer(Male_Wander_Seconds~Group+(1|Male_Pair_Letter)+(1|Frog_Number), data = MateChoiceAnalysisInfected, family = poisson(link="log"))
anova(InfectedTrialLMERMaleWander)
summary(InfectedTrialLMERMaleWander)
plot(InfectedTrialLMERMaleWander)
ggqqplot(residuals(InfectedTrialLMERMaleWander))

simsInfectedTrialLMERMaleWander <- simulateResiduals(InfectedTrialLMERMaleWander)
plot(simsInfectedTrialLMERMaleWander, quantreg = FALSE)

## first lmer for male front

ggdensity(MateChoiceAnalysisInfected$Male_Front_Seconds, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Front_Seconds)

InfectedTrialLMERMaleFront <- lmer(Male_Front_Seconds~Group+(1|Male_Pair_Letter)+(1|Frog_Number), data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMERMaleFront)
summary(InfectedTrialLMERMaleFront)
plot(InfectedTrialLMERMaleFront)
ggqqplot(residuals(InfectedTrialLMERMaleFront))

hist(MateChoiceAnalysisInfected$Male_Front_Seconds)
## now glm

InfectedTrialLMERMaleFront <- glmer(Male_Front_Seconds~Group+(1|Male_Pair_Letter)+(1|Frog_Number), data = MateChoiceAnalysisInfectedMaleMove, family = poisson(link="log"))
anova(InfectedTrialLMERMaleFront)
summary(InfectedTrialLMERMaleFront)
plot(InfectedTrialLMERMaleFront)
ggqqplot(residuals(InfectedTrialLMERMaleFront))

## Questions for Ian about second model set: same as first model set

## Third model set: Color differences between Bd-clean and Bd-infected frogs depending on day

## Data subsetting for different days and model needs
FrogImageData <- read.csv(file.choose())
FrogImageDataControl <- subset(FrogImageData, Frog_Type == "Control")
FrogImageDataControlDorsal <- subset(FrogImageDataControl,Dorsal_Ventral == "Dorsal")
FrogImageDataInfected <- subset(FrogImageData, Frog_Type == "Infected")
FrogImageDataInfectedDorsal <- subset(FrogImageDataInfected,Dorsal_Ventral == "Dorsal")
FrogImageDataDorsal <- subset(FrogImageData, Dorsal_Ventral == "Dorsal")
FrogImageDataDorsalMale <- subset(FrogImageDataDorsal, Sex == "Male")
FrogImageDataDorsalMaleControl <- subset(FrogImageDataDorsalMale, Frog_Type == "Control")
FrogImageDataDorsalMaleInfected <- subset(FrogImageDataDorsalMale, Frog_Type == "Infected")
FrogImageDataDorsalMaleDay0 <- subset(FrogImageDataDorsalMale, Day_Bracket == "0")
FrogImageDataDorsalMaleDay12 <- subset(FrogImageDataDorsalMale, Day_Bracket == "1_2")
FrogImageDataDorsalMaleDay45 <- subset(FrogImageDataDorsalMale, Day_Bracket == "4_5")
FrogImageDataDorsalMaleDay78 <- subset(FrogImageDataDorsalMale, Day_Bracket == "7_8")
FrogImageDataDorsalMaleDay1011 <- subset(FrogImageDataDorsalMale, Day_Bracket == "10_11")
FrogImageDataDorsalMaleDay1314 <- subset(FrogImageDataDorsalMale, Day_Bracket == "13_14")
FrogImageDataDorsalMaleDay1618 <- subset(FrogImageDataDorsalMale, Day_Bracket == "16_18")
FrogImageDataDorsalMaleDay2021 <- subset(FrogImageDataDorsalMale, Day_Bracket == "20_21")
FrogImageDataDorsalMaleDay2223 <- subset(FrogImageDataDorsalMale, Day_Bracket == "22_23")
FrogImageDataDorsalMaleDay26 <- subset(FrogImageDataDorsalMale, Day_Bracket == "26")
FrogImageDataDorsalMaleDay2829 <- subset(FrogImageDataDorsalMale, Day_Bracket == "28_29")
FrogImageDataDorsalMaleDay3132 <- subset(FrogImageDataDorsalMale, Day_Bracket == "31_32")
FrogImageDataDorsalMaleDay35 <- subset(FrogImageDataDorsalMale, Day_Bracket == "35")
FrogImageDataDorsalMaleDay38 <- subset(FrogImageDataDorsalMale, Day_Bracket == "38")
FrogImageDataDorsalMaleDay41 <- subset(FrogImageDataDorsalMale, Day_Bracket == "41")
FrogImageDataDorsalMaleDay44 <- subset(FrogImageDataDorsalMale, Day_Bracket == "44")

## T-tests by day
## Separate for average dorsal brightness, average red pixels, average green pixels and average blue pixels
ggdensity(FrogImageDataDorsalMaleDay0$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay0$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay0)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay0, var.equal = TRUE)
## At day 0, not statistically different

ggdensity(FrogImageDataDorsalMaleDay0$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay0$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay0)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay0, var.equal = FALSE)
## At day 0, not statistically different

ggdensity(FrogImageDataDorsalMaleDay0$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay0$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay0)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay0, var.equal = FALSE)
## At day 0, not statistically different

ggdensity(FrogImageDataDorsalMaleDay0$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay0$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay0)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay0, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay0, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 0, not statistically different

ggdensity(FrogImageDataDorsalMaleDay12$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay12$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay12)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay12, var.equal = TRUE)
## At day 1_2, just barely significant. Infected less bright than control

ggdensity(FrogImageDataDorsalMaleDay12$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay12$Average.R)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay12)
wilcox.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay12, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay12, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At Day 1_2, not statistically different 

ggdensity(FrogImageDataDorsalMaleDay12$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay12$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay12)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay12, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay12, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At Day 1_2, Infected G is lower than control

ggdensity(FrogImageDataDorsalMaleDay12$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay12$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay12)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay12, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay12, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At Day 1_2, Not statistically different

ggdensity(FrogImageDataDorsalMaleDay45$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay45$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay45)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay45, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay45, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 4_5, not significant

ggdensity(FrogImageDataDorsalMaleDay45$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay45$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay45)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay45, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay45, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 4_5, not significant

ggdensity(FrogImageDataDorsalMaleDay45$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay45$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay45)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay45, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay45, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 4_5, not significant

ggdensity(FrogImageDataDorsalMaleDay45$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay45$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay45)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay45, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay45, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 4_5, Average B lower in infected than control

ggdensity(FrogImageDataDorsalMaleDay78$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay78$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay78)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay78, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay78, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 7_8, not significant

ggdensity(FrogImageDataDorsalMaleDay78$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay78$Average.R)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay78)
wilcox.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay78, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay78, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 7_8, not significant

ggdensity(FrogImageDataDorsalMaleDay78$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay78$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay78)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay78, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay78, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 7_8, Infected are less green than control

ggdensity(FrogImageDataDorsalMaleDay78$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay78$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay78)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay78, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay78, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 7_8, not significant

ggdensity(FrogImageDataDorsalMaleDay1011$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay1011$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1011, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 10_11, not significant

ggdensity(FrogImageDataDorsalMaleDay1011$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay1011$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1011, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 10_11, not significant

ggdensity(FrogImageDataDorsalMaleDay1011$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay1011$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay1011, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1011, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 10_11, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, var.equal = FALSE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, var.equal = FALSE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, var.equal = FALSE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.B)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
wilcox.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay1314$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay1314$Average.B)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314)
wilcox.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay1314, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay1314, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 13_14, not significant

ggdensity(FrogImageDataDorsalMaleDay2021$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay2021$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2021, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 20_21 brightness for infected frogs is higher

ggdensity(FrogImageDataDorsalMaleDay2021$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay2021$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2021, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 20_21 Average R for infected frogs is higher

ggdensity(FrogImageDataDorsalMaleDay2021$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay2021$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2021, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 20_21 Average G is not different

ggdensity(FrogImageDataDorsalMaleDay2021$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay2021$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2021, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2021, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 20_21 Average B is not different

ggdensity(FrogImageDataDorsalMaleDay2223$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay2223$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2223, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 22_23 Average Brightness for infected is lower

ggdensity(FrogImageDataDorsalMaleDay2223$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay2223$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2223, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 22_23 Average R for infected is lower

ggdensity(FrogImageDataDorsalMaleDay2223$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay2223$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2223, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 22_23 Average G for infected is lower

ggdensity(FrogImageDataDorsalMaleDay2223$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay2223$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2223, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2223, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 22_23 Average B is not different

ggdensity(FrogImageDataDorsalMaleDay3132$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay3132$Average.Brightness)
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132)
t.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay3132, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 31_32 Average Brightness is lower for infected

ggdensity(FrogImageDataDorsalMaleDay3132$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay3132$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay3132, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 31_32 Average R is lower for infected

ggdensity(FrogImageDataDorsalMaleDay3132$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay3132$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay3132, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 31_32 Average G is lower for infected

ggdensity(FrogImageDataDorsalMaleDay3132$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay3132$Average.B)
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132)
t.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay3132, var.equal = FALSE)
ggboxplot(FrogImageDataDorsalMaleDay3132, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 31_32 Average B is higher for infected

ggdensity(FrogImageDataDorsalMaleDay2829$Average.Brightness)
shapiro.test(FrogImageDataDorsalMaleDay2829$Average.Brightness)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829)
wilcox.test(Average.Brightness ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2829, x = "Day_Bracket", y = "Average.Brightness", color = "Frog_Type")
## At day 28_29 not different

ggdensity(FrogImageDataDorsalMaleDay2829$Average.R)
shapiro.test(FrogImageDataDorsalMaleDay2829$Average.R)
leveneTest(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829)
t.test(Average.R ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2829, x = "Day_Bracket", y = "Average.R", color = "Frog_Type")
## At day 28_29 not different

ggdensity(FrogImageDataDorsalMaleDay2829$Average.G)
shapiro.test(FrogImageDataDorsalMaleDay2829$Average.G)
leveneTest(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829)
t.test(Average.G ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829, var.equal = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2829, x = "Day_Bracket", y = "Average.G", color = "Frog_Type")
## At day 28_29 Average G lower in infected

ggdensity(FrogImageDataDorsalMaleDay2829$Average.B)
shapiro.test(FrogImageDataDorsalMaleDay2829$Average.B)
## not normally distributed, so need to do variance test that is less affected by normality and a mann-whitney U test
leveneTest(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829)
wilcox.test(Average.B ~ Frog_Type, data = FrogImageDataDorsalMaleDay2829, alternative = "two.sided", exact = TRUE)
ggboxplot(FrogImageDataDorsalMaleDay2829, x = "Day_Bracket", y = "Average.B", color = "Frog_Type")
## At day 28_29 Average B lower in infected

## Questions for Ian for third model set:
## 1. Just checking that it's ok to switch up which test I am doing based on if data is normal or not (t-test vs. wilcoxin)
## 2. It would be great to do comparisons across days (ex. Day 0 vs. Day7_8) but because of certain pictures not being usable
## I don't have a picture for every frog on every day which can mess with the repeated measures. Will a glm still work?

## Fourth model set: coloration of Bd-infected males versus log-infection
BrightnessInfectionDorsal <- lmer(Average.Brightness~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMaleInfected)
anova(BrightnessInfectionDorsal)
summary(BrightnessInfectionDorsal)
emmeans(BrightnessInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(BrightnessInfectionDorsal)
## Brightness decreases as infection load increases

AverageRInfectionDorsal <- lmer(Average.R~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMaleInfected)
anova(AverageRInfectionDorsal)
summary(AverageRInfectionDorsal)
emmeans(AverageRInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(AverageRInfectionDorsal)
## Average R decreases as infection load increases

AverageGInfectionDorsal <- lmer(Average.G~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMaleInfected)
anova(AverageGInfectionDorsal)
summary(AverageGInfectionDorsal)
emmeans(AverageGInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(AverageGInfectionDorsal)
## Average G decreases as infection load increases

AverageBInfectionDorsal <- lmer(Average.B~Log_Infection+(1|Frog_Number), data = FrogImageDataDorsalMaleInfected)
anova(AverageBInfectionDorsal)
summary(AverageBInfectionDorsal)
emmeans(AverageBInfectionDorsal, list (pairwise~Log_Infection), lmer.df = "satterthwaite")
plot(AverageBInfectionDorsal)
## no significance in Average B

simsBrightnessInfectionDorsal <- simulateResiduals(BrightnessInfectionDorsal)
plot(simsBrightnessInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsAverageRInfectionDorsal <- simulateResiduals(AverageRInfectionDorsal)
plot(simsAverageRInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsAverageGInfectionDorsal <- simulateResiduals(AverageGInfectionDorsal)
plot(simsAverageGInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about
simsAverageBInfectionDorsal <- simulateResiduals(AverageBInfectionDorsal)
plot(simsAverageBInfectionDorsal, quantreg = FALSE)
## Deviation is not significant, so don't need to worry about

## Questions for Ian for fourth model set:
## 1. Should Day be a repeated measures here because measurements were taken on different days? Or is the repeated measures for the frog ID enough?

## Notes From Ian Session ####
##1. For female mate choice, take out repeated measures of male pair, it doesn't make sense to keep in
##2. For male mate choice wander and front, take out female ID as repeated measures because doesn't make sense
##3. For all mate choice data the distributions are strange and poisson may not make most sense, Ian is researching for me to try and help
##4. For color information, makes sense to run lmer with interaction of day and frog type, interaction term is slope. Would expect significant interaction term with control frogs staying same and infected changing
##5. For color information, run each individual and get coef() for slope and intercept. Average slope and intercepts of both control and infected frogs and plot to see what distribution looks like and what lmer should be
##6. For color information compared to log infection, accounted for day by repeated measures of individual frog, so those models are fine
##7. Linear models for mate choice likely don't make sense because of distributions of raw data and residuals
##8. For mate choice wander data seems to be zero inflated, Ian will help me with that as well

## Mate Choice GLMER models poisson distribution ####
## Histograms show general poisson distribution, so staying with it

hist(MateChoiceAnalysisInfected$Weight_Seconds_Female)
InfectedTrialLMER <- glmer(Weight_Seconds_Female~Group+(1|Frog_Number), data = MateChoiceAnalysisInfected, family = "poisson")
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)
plot(InfectedTrialLMER)
ggqqplot(residuals(InfectedTrialLMER))
emmeans(InfectedTrialLMER, list (pairwise~Group), lmer.df = "satterthwaite")

car::Anova(InfectedTrialLMER, type=3)

## Male movement data
MateChoiceAnalysisInfectedMaleMove <- read.csv(file.choose())
ggdensity(MateChoiceAnalysisInfected$Male_Wander_Seconds, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Wander_Seconds)
ggdensity(MateChoiceAnalysisInfected$Male_Wander_Seconds_Log, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Wander_Seconds_Log)
ggdensity(MateChoiceAnalysisInfected$Male_Wander_Seconds_SQRT, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Wander_Seconds_SQRT)
hist(MateChoiceAnalysisInfected$Male_Wander_Seconds)
hist(MateChoiceAnalysisInfected$Male_Wander_Seconds_Log)
hist(MateChoiceAnalysisInfected$Male_Wander_Seconds_SQRT)

InfectedTrialLMERMaleWander <- glmer(Male_Wander_Seconds~Group+(1|Male_Pair_Letter), data = MateChoiceAnalysisInfected, family = poisson(link="log"), weights = wt)
anova(InfectedTrialLMERMaleWander)
summary(InfectedTrialLMERMaleWander)
plot(InfectedTrialLMERMaleWander)
ggqqplot(residuals(InfectedTrialLMERMaleWander))
emmeans(InfectedTrialLMERMaleWander, list (pairwise~Group), lmer.df = "satterthwaite")

car::Anova(InfectedTrialLMERMaleWander, type=3)

simsInfectedTrialLMERMaleWander <- simulateResiduals(InfectedTrialLMERMaleWander)
plot(simsInfectedTrialLMERMaleWander, quantreg = FALSE)

## Front
ggdensity(MateChoiceAnalysisInfected$Male_Front_Seconds, main = "Density Plot of Male Wander Seconds", xlab = " Male Wander Seconds")
ggqqplot(MateChoiceAnalysisInfected$Male_Front_Seconds)

InfectedTrialLMERMaleFront <- glmer(Male_Front_Seconds~Group+(1|Male_Pair_Letter), data = MateChoiceAnalysisInfected, family = poisson(link="log"), weights = wt)
anova(InfectedTrialLMERMaleFront)
summary(InfectedTrialLMERMaleFront)
plot(InfectedTrialLMERMaleFront)
ggqqplot(residuals(InfectedTrialLMERMaleFront))
emmeans(InfectedTrialLMERMaleFront, list (pairwise~Group), lmer.df = "satterthwaite")

car::Anova(InfectedTrialLMERMaleFront, type=3)

## Quick side recheck
ControlTrialLMER <- glmer(Weight_Seconds~Group+(1|Frog_Number), data = MateChoiceAnalysisControl, family = "poisson")
anova(ControlTrialLMER)
summary(ControlTrialLMER)
emmeans(ControlTrialLMER, list (pairwise~Group), lmer.df = "satterthwaite")

InfectedTrialLMERside <- glmer(Weight_Seconds_Female~Side+(1|Frog_Number), data = MateChoiceAnalysisInfected, family = "poisson")
anova(InfectedTrialLMERside)
summary(InfectedTrialLMERside)
emmeans(InfectedTrialLMERside, list (pairwise~Side), lmer.df = "satterthwaite")

## Control trials preferred opposite side to infected, so reiterates that side doesn't matter
## even though I standardized it when I ran the trials in the first place

## Infected more or less
hist(MateChoiceAnalysisInfectedLess$Weight_Seconds)
InfectedLessTrialLMER <- glmer(Weight_Seconds~Group+(1|Frog_Number), data = MateChoiceAnalysisInfectedLess, family = "poisson", weights = wt)
anova(InfectedLessTrialLMER)
summary(InfectedLessTrialLMER)
plot(InfectedLessTrialLMER)
ggqqplot(residuals(InfectedLessTrialLMER))
emmeans(InfectedLessTrialLMER, list (pairwise~Group), lmer.df = "satterthwaite")

hist(MateChoiceAnalysisInfectedMore$Weight_Seconds)
InfectedMoreTrialLMER <- glmer(Weight_Seconds~Group+(1|Frog_Number), data = MateChoiceAnalysisInfectedMore, family = "poisson", weights = wt)
anova(InfectedMoreTrialLMER)
summary(InfectedMoreTrialLMER)
plot(InfectedMoreTrialLMER)
ggqqplot(residuals(InfectedMoreTrialLMER))
emmeans(InfectedMoreTrialLMER, list (pairwise~Group), lmer.df = "satterthwaite")
