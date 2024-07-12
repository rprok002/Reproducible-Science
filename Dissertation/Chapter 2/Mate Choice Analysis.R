## Mate Choice Data Analysis
##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

MateChoiceCompiledDataControlNoCall <- read.csv(file.choose())
MateChoiceAnalysisControl <- read.csv(file.choose())

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
## male pair order still doens't affect, so keeping as random

## Final control model
ControlTrialLMER <- lmer(SQRT_Weight_Seconds~Group+Time_Interaction_Zone+(1|Frog_Number)+(1|Male_Pair_Letter) , data = MateChoiceAnalysisControl)
anova(ControlTrialLMER)
summary(ControlTrialLMER)

## interaction zone time significant so keeping in model, but group is not significant so don't have to 
## include side of apparatus as an effect in the infection model
