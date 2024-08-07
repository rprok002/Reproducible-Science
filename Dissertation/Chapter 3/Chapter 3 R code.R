## Chapter 3 R Code
Caged_Side_Data= read.csv(file.choose())
Caged_Side_Data
Pre <- Caged_Side_Data[Caged_Side_Data$Pre_Post == 'Pre',]
Post <- Caged_Side_Data[Caged_Side_Data$Pre_Post == 'Post',]
Morning <- Caged_Side_Data[Caged_Side_Data$Morning.Evening == 'Morning',]

Evening <- Caged_Side_Data[Caged_Side_Data$Morning.Evening == 'Evening',]

##Tests of Normality
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

## Side
ggdensity(Pre$Proportion_Left, main = "Density Plot Pre-Addition Left Side", xlab = "Proportion Left")
ggqqplot(Pre$Proportion_Left)
ggdensity(Pre$Proportion_Right, main = "Density Plot Pre-Addition Right Side", xlab = "Proportion Right")
ggqqplot(Pre$Proportion_Right)
ggdensity(Morning$Proportion_Left, main = "Density Plot Post-Addition Morning Left Side", xlab = "Proportion Left")
ggqqplot(Morning$Proportion_Left)
ggdensity(Morning$Proportion_Right, main = "Density Plot Post-Addition Morning Right Side", xlab = "Proportion Right")
ggqqplot(Morning$Proportion_Right)
ggdensity(Evening$Proportion_Left, main = "Density Plot Post-Addition Evening Left Side", xlab = "Proportion Left")
ggqqplot(Evening$Proportion_Left)
ggdensity(Evening$Proportion_Right, main = "Density Plot Post-Addition Evening Right Side", xlab = "Proportion Right")
ggqqplot(Evening$Proportion_Right)

## Sides data doesn't look normalized from these tests of normality plots

## Chytrid Smell Presence
ggdensity(Morning$Proportion_Chytrid, main = "Density Plot Post-Addition Morning Chytrid Side", xlab = "Proportion Chytrid")
ggqqplot(Morning$Proportion_Chytrid)
ggdensity(Morning$Proportion_Control, main = "Density Plot Post-Addition Morning Control Side", xlab = "Proportion Control")
ggqqplot(Morning$Proportion_Control)
ggdensity(Evening$Proportion_Chytrid, main = "Density Plot Post-Addition Evening Chytrid Side", xlab = "Proportion Chytrid")
ggqqplot(Evening$Proportion_Chytrid)
ggdensity(Evening$Proportion_Control, main = "Density Plot Post-Addition Evening Control Side", xlab = "Proportion Control")
ggqqplot(Evening$Proportion_Control)

## Treatment data doesn't look normalized from these tests of normality plots

## Proportion Days Male and Female Together
ggdensity(Pre$Proportion_Days_Together, main = "Density Plot Pre-Addition Proportion Days Together", xlab = "Proportion Days Together")
ggqqplot(Pre$Proportion_Days_Together)
ggdensity(Post$Proportion_Days_Together, main = "Density Plot Post-Addition Proportion Days Together", xlab = "Proportion Days Together")
ggqqplot(Post$Proportion_Days_Together)

## Only Pre addition proportion days together looks normal in plots 

## Shapiro-Wilks test
## Null: normal: Alt: not normal
shapiro.test(Pre$Proportion_Left)
## not normal, p = 0.011
shapiro.test(Pre$Proportion_Right)
## not normal, p = 0.011
shapiro.test(Morning$Proportion_Left)
## not normal, p = 0.003609
shapiro.test(Morning$Proportion_Right)
## not normal, p = 0.003609
shapiro.test(Evening$Proportion_Left)
## not normal, p = 0.02615
shapiro.test(Evening$Proportion_Right)
## not normal, p = 0.02615
shapiro.test(Morning$Proportion_Chytrid)
## not normal, p = 0.004239
shapiro.test(Morning$Proportion_Control)
## not normal, p = 0.004239
shapiro.test(Evening$Proportion_Chytrid)
## not normal, P = 0.02467
shapiro.test(Evening$Proportion_Control)
## not normal, P = 0.02467
shapiro.test(Pre$Proportion_Days_Together)
## normal
shapiro.test(Post$Proportion_Days_Together)
## normal

## Only normal is proportion days the frogs are together in the same side of the cage


## Levene's test for equal variance
## Null: all pop var are equal; Alt: at least two are different
Pre_Variance <- read.csv(file.choose())
Pre_Days <- read.csv(file.choose())
Morning_Post_Variance <- read.csv(file.choose())
Evening_Post_Variance <- read.csv(file.choose())
Morning_Post_Chytrid <- read.csv(file.choose())
Evening_Post_Chytrid <- read.csv(file.choose())
Pre_Together_Variance <- read.csv(file.choose())
Post_Together_Variance <- read.csv(file.choose())
library(car)
PreVar <- leveneTest(Weight~Group, Pre_Variance)
PreVar
## not equal variance
MorningPostVar <- leveneTest(Weight~Group, Morning_Post_Variance)
MorningPostVar
# not equal variance
EveningPostVar <- leveneTest(Weight~Group, Evening_Post_Variance)
EveningPostVar
# not equal variance
MorningPostChytrid <- leveneTest(Weight~Group, Morning_Post_Chytrid)
MorningPostChytrid
# not equal variance
EveningPostChytrid <- leveneTest(Weight~Group, Evening_Post_Chytrid)
EveningPostChytrid
# not equal variance
PreTogether <- leveneTest(Weight~Group, Pre_Together_Variance)
PreTogether
# not equal variance
PostTogether <- leveneTest(Weight~Group, Post_Together_Variance)
PostTogether
# not equal variance

## Use Wilcoxin T-test in future now because non-normal and nonequal variances

## One-Way Tests to compare 
## Pre Side Data
PreSideTtest <- wilcox.test(Weight~Group, data = Pre_Variance, paired = TRUE)
PreSideTtest

PreDaySideTtest <- wilcox.test(Weight~Group, data = Pre_Days, paired = TRUE)
PreDaySideTtest
# Pre-addition, side doesn't matter
# Also, days and proportions bring out same data, so should be able to use them interchangeably???
PostMornSideTtest <- wilcox.test(Weight~Group, data = Morning_Post_Variance, paired = TRUE)
PostMornSideTtest
# Post-addition, side in morning doesn't matter
PostEveSideTtest <- wilcox.test(Weight~Group, data = Evening_Post_Variance, paired = TRUE)
PostEveSideTtest
# Post-addition, side in evening doesn't matter
PostMornChytridTtest <- wilcox.test(Weight~Group, data = Morning_Post_Chytrid, paired = TRUE)
PostMornChytridTtest
# Post-addition, morning time spent where chytrid is isn't significant
PostEveChytridTtest <- wilcox.test(Weight~Group, data = Evening_Post_Chytrid, paired = TRUE)
PostEveChytridTtest
# Post-addition, evening time spent where chytrid is isn't significant 
PreTogetherTtest <- wilcox.test(Weight~Group, data = Pre_Together_Variance, paired = TRUE)
PreTogetherTtest
# Pre-addition, time spent together versus not isn't significant
PostTogetherTtest <- wilcox.test(Weight~Group, data = Post_Together_Variance, paired = TRUE)
PostTogetherTtest
# Post-addition, time spent together versus not is significant

library("ggpubr")
ggboxplot(Post_Together_Variance, x = "Group", y = "Weight", 
          order = c("Days_Together", "Days_Apart"),
          ylab = "Weight", xlab = "Groups")

#Addition t-test for which way significant
PostTogetherTtest <- wilcox.test(Weight~Group, data = Post_Together_Variance, paired = TRUE, alternative = "less")
PostTogetherTtest
## Significant one-sided T-test
# Days_Apart (aphabetical) are less than Days_Together, over time frogs spended significantly more time togehter
# Looking at data, appears that as females move, males move (ex. female moves from right to left, a day later the male moves to the left as well)
# Things to remember: haven't been able to test if gauze gathers scent properly or if volatiles escape water well, 
# because still haven't properly identified Bd volatile profile so can't properly test
# To my nose, the gauze smelled

## Graphs

## Side
ggboxplot(Pre_Variance, x = "Group", y = "Weight", 
          order = c("Left", "Right"), ylab = "Proportion Days", xlab = "Side", fill = "Group", palette = c("lightgreen", "darkgreen")) + ggtitle("Pre-Addition Side Preference") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  annotate("text", y =1.1, x=1, label="V = 153.5, n = 24, p = 0.3815")
ggboxplot(Morning_Post_Variance, x = "Group", y = "Weight", 
          order = c("Left", "Right"), ylab = "Proportion Days", xlab = "Side", fill = "Group", palette = c("lightgreen", "darkgreen")) + ggtitle("Post-Addition Morning Side Preference") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  annotate("text", y =1.1, x=1, label="V = 161, n = 24, p = 0.7613")
ggboxplot(Evening_Post_Variance, x = "Group", y = "Weight", 
          order = c("Left", "Right"), ylab = "Proportion Days", xlab = "Side", fill = "Group", palette = c("lightgreen", "darkgreen")) + ggtitle("Post-Addition Evening Side Preference") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  annotate("text", y =1.1, x=1, label="V = 125.5, n = 24, p = 0.7129")
ggboxplot(Evening_Post_Variance, x = "Group", y = "Weight", 
          order = c("Left", "Right"), ylab = "Proportion Days", xlab = "Side", fill = "Group", palette = c("lightgreen", "darkgreen")) + ggtitle("Post-Addition Evening Side Preference") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  annotate("text", y =1.1, x=1, label="V = 125.5, n = 24, p = 0.7129")
ggboxplot(Morning_Post_Chytrid, x = "Group", y = "Weight", 
          order = c("Chytrid", "Control"), ylab = "Proportion Days", xlab = "Side", fill = "Group", palette = c("darkred", "lightpink")) + ggtitle("Post-Addition Morning Scent Preference") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  annotate("text", y =1.1, x=1, label="V = 127, n = 24, p = 0.5151")
ggboxplot(Evening_Post_Chytrid, x = "Group", y = "Weight", 
          order = c("Chytrid", "Control"), ylab = "Proportion Days", xlab = "Side", fill = "Group", palette = c("darkred", "lightpink")) + ggtitle("Post-Addition Evening Scent Preference") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  annotate("text", y =1.1, x=1, label="V = 128.5, n = 24, p = 0.7826")


ggboxplot(Pre_Together_Variance, x = "Group", y = "Weight", 
          order = c("Days_Apart", "Days_Together"), ylab = "Proportion Days", xlab = "Side", fill = "Group", palette = c("lightblue", "darkblue")) + ggtitle("Pre-Addition Pair Location") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  annotate("text", y =1.1, x=1, label="V = 35.5, n = 12 p = 0.4404")

ggboxplot(Post_Together_Variance, x = "Group", y = "Weight", 
          order = c("Days_Apart", "Days_Together"), ylab = "Proportion Days", xlab = "Side", fill = "Group", palette = c("lightblue", "darkblue")) + ggtitle("Post-Addition Pair Location") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  annotate("text", y =1.1, x=1, label="V = 12, n = 12 p = 0.03697")
