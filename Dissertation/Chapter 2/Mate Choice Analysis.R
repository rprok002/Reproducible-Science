## Mate Choice Data Analysis
##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

MateChoiceCompiledDataControlNoCall <- read.csv(file.choose())
MateChoiceAnalysisControl <- read.csv(file.choose())
MateChoiceAnalysisInfected <- read.csv(file.choose())
MateChoiceAnalysisInfectedMore <- read.csv(file.choose())
MateChoiceAnalysisInfectedLess <- read.csv(file.choose())

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

## Control Models 
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)
library(lmtest)

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
ControlTrialLMER <- lmer(SQRT_Weight_Seconds~Group+Time_Interaction_Zone+(1|Frog_Number)+(1|Male_Pair_Letter) , data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)

## interaction zone time significant so keeping in model, but group is not significant so don't have to 
## include side of apparatus as an effect in the infection model

## Infected Models
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
## include side of apparatus as an effect in the infection model

## Comparing more and less infected, adding in a variable real quick
InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+More_Less_Infected+(1|Male_Pair_Letter)+Time_Interaction_Zone+(1|Frog_Number) , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)

InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group*More_Less_Infected+(1|Male_Pair_Letter)+Time_Interaction_Zone+(1|Frog_Number) , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)

## doesn't change anything whether interaction or not, so even though color may change the females don't use that to choose mates

## Final Infected Model
InfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+(1|Male_Pair_Letter)+Time_Interaction_Zone+(1|Frog_Number) , data = MateChoiceAnalysisInfected)
anova(InfectedTrialLMER)
summary(InfectedTrialLMER)

## interaction zone time significant so keeping in model, but group is not significant, so females don't
## stay close to either infected or clean frog significantly less/more

## Infected Models Separated by Less and More Infected

MoreInfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+Male_Pair_Letter+Time_Interaction_Zone+(1|Frog_Number)+(1|Female_Trial_Order) , data = MateChoiceAnalysisInfectedMore)
anova(MoreInfectedTrialLMER)
summary(MoreInfectedTrialLMER)

## still not signifiant even though more infected

LessInfectedTrialLMER <- lmer(SQRT_Weight_Seconds~Group+Male_Pair_Letter+Time_Interaction_Zone+(1|Frog_Number)+(1|Female_Trial_Order) , data = MateChoiceAnalysisInfectedLess)
anova(LessInfectedTrialLMER)
summary(LessInfectedTrialLMER)

## not significant even though less infected

##Boxplots

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
  
MateChoiceAnalysisInfected$Group <- factor(MateChoiceAnalysisInfected$Group, levels = c("Clean", "Infected"))
ggboxplot(MateChoiceAnalysisInfected, x = "Group", y = "Weight_Seconds",  ylab = " Time (seconds)", xlab = "Male",
          ylim = c(0, 1200), fill = "darkgrey") + 
  scale_x_discrete(labels=c("Clean", "Infected"))+
  scale_y_continuous(breaks=seq(0,900,by=100))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.title=element_blank())+
  annotate("text", x=2, y=1100, label= "F: 1.19 ; DF: 1,173 ; p = 0.28", fontface = "bold", size = 5)+
  annotate("text", x=2, y=1200, label= " Side", fontface = "bold", size = 5)+
  annotate("text", x=1, y=277, label="x", size = 7)+
  annotate("text", x=2, y=336, label="x", size = 7)

## Means and standard deviations for boxplots
mean_Control <- MateChoiceAnalysisControl %>%
    group_by(Group, Type) %>% 
    summarise(mean= mean(Weight_Seconds),
              se = sd(Weight_Seconds))
glimpse(mean_Control)
  
mean_Infected <- MateChoiceAnalysisInfected %>%
    group_by(Group, Type) %>% 
    summarise(mean= mean(Weight_Seconds),
              se = sd(Weight_Seconds))
glimpse(mean_Infected)

## Picture Analysis

FrogImageData <- read.csv(file.choose())
FrogImageDataControl <- subset(FrogImageData, Frog_Type == "Control")
FrogImageDataControlDorsal <- subset(FrogImageDataControl,Dorsal_Ventral == "Dorsal")
FrogImageDataControlVentral <- subset(FrogImageDataControl,Dorsal_Ventral == "Ventral")
FrogImageDataInfected <- subset(FrogImageData, Frog_Type == "Infected")
FrogImageDataInfectedDorsal <- subset(FrogImageDataInfected,Dorsal_Ventral == "Dorsal")
FrogImageDataInfectedVentral <- subset(FrogImageDataInfected,Dorsal_Ventral == "Ventral")
FrogImageDataDorsal <- subset(FrogImageData, Dorsal_Ventral == "Dorsal")
FrogImageDataVentral <- subset(FrogImageData, Dorsal_Ventral == "Ventral")

## Preliminary look
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

ggplot(FrogImageDataInfectedDorsal, aes(x = Infection, y = Average.Brightness, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Average.Brightness, colour = Frog_Number)) +
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

ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Greeness.score, colour = Frog_Number)) +
  geom_line()

ggplot(FrogImageDataInfectedVentral, aes(x = Infection, y = Blueness.score, colour = Frog_Number)) +
  geom_line()

## may be pattern, a couple high values of infection might be confounding

ggplot(FrogImageDataDorsal, aes(x = Day, y = Average.Brightness, colour = Frog_Type)) +
  geom_line()
## this is lumping frogs all together which doesn't make much sense because it's what happens to the frogs over time,
## averaging when there is between frog variation within groups doesn't make a lot of sense

## Tests of normality

## Control
ggdensity(FrogImageDataControlDorsal$Average.Brightness)
ggqqplot(FrogImageDataControlDorsal$Average.Brightness)
## fairly normal
ggdensity(FrogImageDataControlVentral$Average.Brightness)
ggqqplot(FrogImageDataControlVentral$Average.Brightness)
## fairly normal
## bit of a dip in the middle but look fairly normal

ggdensity(FrogImageDataControlDorsal$Average.R)
ggqqplot(FrogImageDataControlDorsal$Average.R)
## fairly normal
ggdensity(FrogImageDataControlDorsal$Average.G)
ggqqplot(FrogImageDataControlDorsal$Average.G)
## fairly normal
ggdensity(FrogImageDataControlDorsal$Average.B)
ggqqplot(FrogImageDataControlDorsal$Average.B)
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
ggdensity(FrogImageDataControlVentral$Proportion.B)
ggqqplot(FrogImageDataControlVentral$Proportion.B)
## fairly normal

ggdensity(FrogImageDataControlDorsal$Redness.score)
ggqqplot(FrogImageDataControlDorsal$Redness.score)
## fairly normal
ggdensity(FrogImageDataControlDorsal$Greeness.score)
ggqqplot(FrogImageDataControlDorsal$Greeness.score)
## fairly normal
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
ggdensity(FrogImageDataInfectedVentral$Greeness.score)
ggqqplot(FrogImageDataInfectedVentral$Greeness.score)
## fairly normal
ggdensity(FrogImageDataInfectedVentral$Blueness.score)
ggqqplot(FrogImageDataInfectedVentral$Blueness.score)
## fairly normal

## a few values slightly skewed right but not enough that anything needs to be done

## Removed M19 D and V color May 31 because outlier 
## Removed M16 D and V Oct 15 because infection is outlier 
## Removed F10 D and V Oct 18 because infection is outlier 
## Removed M26 D and V color Oct 9 because outlier 
## Removed F5 brightness Oct 16 because outlier

## Separate Models
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)
library(lmtest)

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

## Questions for Cori
## Show QQ plot for Dorsal Control, see if the hump in the middle isn't ok
## Show QQ plot and regular plot for Proportion R Ventral Control, skewed right but when do I do something about that when I have a lot of potential 
## variables I am comparing?
## Show Color vs. Infection Dorsal plot, how infected some frogs get is likely skewing, how to set rule for what to keep?
## Show Dorsal Brightness Control is significant over days, means Day needs to be a covariate in infection models to account for that possible cause?

