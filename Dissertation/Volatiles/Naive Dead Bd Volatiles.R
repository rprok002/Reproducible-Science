## Naive Dead Bd Chemosensory Trials 1 through 3 frogs 1-7, 9-14

naivedeadbdvolatiles <- read.csv(file.choose())
library(lmerTest)
library(car)
naivedeadbdvolatilesglm <-glmer(Weight~ Location*Group +(1|Trial_Number) +(1|Frog_Number) , 
                                data = naivedeadbdvolatiles, family = binomial)
anova(naivedeadbdvolatilesglm, type=2)
car::Anova(naivedeadbdvolatilesglm, type=2)
summary(naivedeadbdvolatilesglm)

