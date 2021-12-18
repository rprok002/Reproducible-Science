## Naive Dead Bd Chemosensory Trials 1 through 3 frogs 1-7, 9-14


package:
  library(lmerTest)

Example model:
  d <-glmer(Diversity~ Adjusted.day*Sex+ Body.Mass+(1|id) +(1|Enclosure) , data = div, family=binomial)
summary(d)