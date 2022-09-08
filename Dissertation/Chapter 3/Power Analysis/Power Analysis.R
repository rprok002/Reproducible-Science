##Power Analysis for Chapter 2 of Dissertation
## A 1-sample test for proportions assuming we will only be looking at proportion of tadpoles 
## deposited in each treatment is the only independent variable consideration
## Assume 80% in no Bd, 20% in Bd
pwr.p.test(h = ES.h(p1 = 0.8, p2 = 0.5), sig.level = 0.05, n =16)

##Dep variable = proportion of tadpoles in each treatment
##Ind variables = Treatment (experiment or control)
## 16 mesocosms , so n=16
## sig is 0.05
## p1 = 0.8
## p2 = 0.5


## power = 0.73

##Dep variable = proportion of time spent near frogs
##Ind variable: healthy or unhealthy frog
## 25 females, so n=25
## p1 = 0.8
## p2 = 0.5
library(pwr)
pwr.p.test(h = ES.h(p1 = 0.8, p2 = 0.5), sig.level = 0.05, n =25)
## power = 0.90

## 2-sided t test with 35 infected, 45 not infected, medium effect size
## and 0.05 significance
pwr.2p2n.test(h = 0.6, n1 = 35, n2 = 45, sig.level = 0.05)
## power is 0.76

pwr.p.test(h = ES.h(p1 = 0.8, p2 = 0.5), sig.level = 0.05, n =20)
## power is 0.82