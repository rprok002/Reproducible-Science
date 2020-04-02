Tri=read.csv(file.choose())
newTri <- na.omit(Tri)
length=newTri[,7]
condition=newTri[,3]
condition
Tri.ice <- subset(newTri, condition == 1)
Tri.alcohol <- subset(newTri, condition == 2 & length < 100)
Tri.fresh <- subset(newTri, condition == 3)
#2 as "Hydropsychidae", 3 as "Phryganeidae", 4 as "Polycentropodidae, 5 as "Rhyacophilidae"
TrifreshSA=Tri.fresh[,5]
Trifreshlength=Tri.fresh[,7]
Trifreshmass=Tri.fresh[,8]
Trifreshfamily=Tri.fresh[,11]

TrialcoholSA=Tri.alcohol[,5]
Trialcohollength=Tri.alcohol[,7]
Trialcoholmass=Tri.alcohol[,8]
TriiceSA=Tri.ice[,5]
Triicelength=Tri.ice[,7]
Triicemass=Tri.ice[,8]
#Linear model fresh Trichoptera
lm.Trifresh=lm(Trifreshmass~TrifreshSA)
lm.Trifresh
anova(lm.Trifresh)
summary(lm.Trifresh)
plot(TrifreshSA, Trifreshmass)
plot(lm.Trifresh)
lm.Trifreshlength=lm(Trifreshmass~Trifreshlength)
lm.Trifreshlength
anova(lm.Trifreshlength)
summary(lm.Trifreshlength)
plot(lm.Trifreshlength)
plot(Trifreshlength, Trifreshmass)
#Linear model alcohol Trichoptera
lm.Trialcohol=lm(Trialcoholmass~TrialcoholSA)
lm.Trialcohol
anova(lm.Trialcohol)
summary(lm.Trialcohol)
plot(TrialcoholSA, Trialcoholmass)
plot(lm.Trialcohol)
lm.Trialcohollength=lm(Trialcoholmass~Trialcohollength)
lm.Trialcohollength
anova(lm.Trialcohollength)
summary(lm.Trialcohollength)
plot(lm.Trialcohollength)
plot(Trialcohollength, Trialcoholmass)
#Linear model ice Trichoptera
lm.Triice=lm(Triicemass~TriiceSA)
lm.Triice
anova(lm.Triice)
summary(lm.Triice)
plot(TriiceSA, Triicemass)
plot(lm.Trialcohol)
lm.Triicelength=lm(Triicemass~Triicelength)
lm.Triicelength
anova(lm.Triicelength)
summary(lm.Triicelength)
plot(lm.Triicelength)
plot(Triicelength, Triicemass)



library(nlstools)
TriModel <- function(TrifreshSA, a, b) {
  Trifreshmass=a * exp(b*TrifreshSA)
  return(Trifreshmass)
}

Tri.int <- function (mCall, LHS, data){
  TrifreshSA <- Tri.fresh$SA.after..mm2.
  Trifreshmass <- Tri.fresh$Mass.mg.
  a <- 0.0003853  
  b <- -1.4164144  
 
  
  value = list(a,b) 
  names(value) <- mCall[c("a", "b")] 
  return(value)
}


SS.Tri <- selfStart(model=TriModel,initial= Tri.int)

iv.Tri <- getInitial(Trifreshmass ~ SS.Tri("TrifreshSA", "a", "b"),
                 data = Tri.fresh)
iv.Tri
### Create a dataframe to store month parameter values a1, ax and r (parms.Month):

Tri.Family <- data.frame(
  Family=factor(),
  a=numeric(),
  b=numeric(),
  a.pvalue=numeric(),
  b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
Tri.Family [1:4,1]<- seq(1,4,1)

### Write a function to fit the model 

Trifreshfamily <- function(dataframe){ y = nls( Trifreshmass ~ a * exp(b*TrifreshSA), dataframe,
                                         start=list(a= iv$a , b= iv$b),
                                         na.action=na.exclude, trace=F,
                                         control=nls.control(warnOnly=T))

y.df <- as.data.frame(cbind(t(coef(summary(y)) [1:2, 1]), t(coef(summary(y)) [1:3, 4])))
names(y.df) <-c("a","b", "a.pvalue", "b.pvalue")
return (y.df )}

### Write a loop to fit curves and add paramters to a dataframe:

try(for(j in unique(Tri.fresh$Family)){
  
  
  iv <- getInitial(Trifreshmass ~ SS.lrc("TrifreshSA", "a", "b"), data = Tri.fresh[which(Tri.fresh$Family == j),])
  
  y3 <- try(Trifreshfamily(Tri.fresh[which(Tri.fresh$Family == j),]), silent=T)
  
  try(Tri.Family[c(Tri.Family$Family == j ), 2:7 ] <- cbind(y3), silent=T)
  rm(y3)
}, silent=T)
Tri.Family
Warnings()
### Bootstrapping

boot.NEE <- data.frame(parms.Month[, c("MONTH")]); names (boot.NEE) <- "MONTH"
boot.NEE$a1.est <- 0
boot.NEE$ax.est<- 0
boot.NEE$r.est<- 0
boot.NEE$a1.se<- 0
boot.NEE$ax.se<- 0
boot.NEE$r.se<- 0

for( j in unique(boot.NEE$Month)){
  
  y1 <-day[which(day$MONTH == j),] 
  
  iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = y1)
  
  day.fit <- nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1,
                  start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
                  na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
  
  try(results <- nlsBoot(day.fit, niter=100 ), silent=T)
  try(a <- t(results$estiboot)[1, 1:3], silent=T)
  try(names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
  try( b <- t(results$estiboot)[2, 1:3], silent=T)
  try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
  try(c <- t(data.frame(c(a,b))), silent=T)
  
  try(boot.NEE[c(boot.NEE$MONTH == j), 2:7] <- c[1, 1:6], silent=T)
  try(rm(day.fit, a, b, c, results, y1), silent=T)
}

lrc <- merge( parms.Month, boot.NEE, by.x="MONTH", by.y="MONTH") 
lrc

iv.Tri <- getInitial(Trifreshmass ~ SS.Tri("TrifreshSA", "a", "b"),
                     data = Tri.fresh[which(Tri.fresh$Family == 2),])