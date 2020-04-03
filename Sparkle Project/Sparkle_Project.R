Tri=read.csv(file.choose())
#Subsetting
newTri <- na.omit(Tri)
length=newTri[,7]
condition=newTri[,3]
condition

Tri.fresh <- subset(newTri, condition == 3)
TrifreshSA=Tri.fresh[,5]
Trifreshlength=Tri.fresh[,7]
Trifreshmass=Tri.fresh[,8]
Trifreshfamily=Tri.fresh[,11]

Tri.fresh.Hydro <- subset(newTri, condition ==3 & Family == "Hydropsychidae")
Tri.fresh.Phry <- subset(newTri, condition ==3 & Family == "Phryganeidae")
Tri.fresh.Poly <- subset(newTri, condition ==3 & Family == "Polycentropodidae")
Tri.fresh.Rhya <- subset(newTri, condition ==3 & Family == "Rhyacophilidae")
HydrofreshSA=Tri.fresh.Hydro[,5]
Hydrofreshlength=Tri.fresh.Hydro[,7]
Hydrofreshmass=Tri.fresh.Hydro[,8]
Hydrofreshfamily=Tri.fresh.Hydro[,11]
PhryfreshSA=Tri.fresh.Phry[,5]
Phryfreshlength=Tri.fresh.Phry[,7]
Phryfreshmass=Tri.fresh.Phry[,8]
Phryfreshfamily=Tri.fresh.Phry[,11]
PolyfreshSA=Tri.fresh.Poly[,5]
Polyfreshlength=Tri.fresh.Poly[,7]
Polyfreshmass=Tri.fresh.Poly[,8]
Polyfreshfamily=Tri.fresh.Poly[,11]
RhyafreshSA=Tri.fresh.Rhya[,5]
Rhyafreshlength=Tri.fresh.Rhya[,7]
Rhyafreshmass=Tri.fresh.Rhya[,8]
Rhyafreshfamily=Tri.fresh.Rhya[,11]

Tri.alcohol <- subset(newTri, condition == 2 & length < 100)
TrialcoholSA=Tri.alcohol[,5]
Trialcohollength=Tri.alcohol[,7]
Trialcoholmass=Tri.alcohol[,8]

Tri.alcohol.Hydro <- subset(newTri, condition ==2 & Family == "Hydropsychidae")
Tri.alcohol.Phry <- subset(newTri, condition ==2 & Family == "Phryganeidae")
Tri.alcohol.Poly <- subset(newTri, condition ==2 & Family == "Polycentropodidae")
Tri.alcohol.Rhya <- subset(newTri, condition ==2 & Family == "Rhyacophilidae")
Tri.alcohol.Bra <- subset(newTri, condition ==2 & Family == "Bracgycentridae" & length < 100)
HydroalcoholSA=Tri.alcohol.Hydro[,5]
Hydroalcohollength=Tri.alcohol.Hydro[,7]
Hydroalcoholmass=Tri.alcohol.Hydro[,8]
Hydroalcoholfamily=Tri.alcohol.Hydro[,11]
PhryalcoholSA=Tri.alcohol.Phry[,5]
Phryalcohollength=Tri.alcohol.Phry[,7]
Phryalcoholmass=Tri.alcohol.Phry[,8]
Phryalcoholfamily=Tri.alcohol.Phry[,11]
PolyalcoholSA=Tri.alcohol.Poly[,5]
Polyalcohollength=Tri.alcohol.Poly[,7]
Polyalcoholmass=Tri.alcohol.Poly[,8]
Polyalcoholfamily=Tri.alcohol.Poly[,11]
RhyaalcoholSA=Tri.alcohol.Rhya[,5]
Rhyaalcohollength=Tri.alcohol.Rhya[,7]
Rhyaalcoholmass=Tri.alcohol.Rhya[,8]
Rhyaalcoholfamily=Tri.alcohol.Rhya[,11]
BraalcoholSA=Tri.alcohol.Bra[,5]
Braalcohollength=Tri.alcohol.Bra[,7]
Braalcoholmass=Tri.alcohol.Bra[,8]
Braalcoholfamily=Tri.alcohol.Bra[,11]

Tri.ice <- subset(newTri, condition == 1)
TriiceSA=Tri.ice[,5]
Triicelength=Tri.ice[,7]
Triicemass=Tri.ice[,8]

Tri.ice.Hydro <- subset(newTri, condition ==1 & Family == "Hydropsychidae")
Tri.ice.Poly <- subset(newTri, condition ==1 & Family == "Polycentropodidae")
Tri.ice.Rhya <- subset(newTri, condition ==1 & Family == "Rhyacophilidae")
HydroiceSA=Tri.ice.Hydro[,5]
Hydroicelength=Tri.ice.Hydro[,7]
Hydroicemass=Tri.ice.Hydro[,8]
Hydroicefamily=Tri.ice.Hydro[,11]
PolyiceSA=Tri.ice.Poly[,5]
Polyicelength=Tri.ice.Poly[,7]
Polyicemass=Tri.ice.Poly[,8]
Polyicefamily=Tri.ice.Poly[,11]
RhyaiceSA=Tri.ice.Rhya[,5]
Rhyaicelength=Tri.ice.Rhya[,7]
Rhyaicemass=Tri.ice.Rhya[,8]
Rhyaicefamily=Tri.ice.Rhya[,11]

#Linear model fresh Trichoptera
lm.Trifresh=lm(Trifreshmass~TrifreshSA)
lm.Trifresh
anova(lm.Trifresh)
summary(lm.Trifresh)
plot(TrifreshSA, Trifreshmass, main= "Trichoptera Fresh Mass vs. Surface Area", xlab="Trichoptera Fresh Surface Area (sq mm)", ylab="Trichoptera Fresh Mass (mg)") 
lines(Tri.fresh$SA.after..mm2., fitted(lm.Trifresh), col="blue")
plot(lm.Trifresh)

lm.Trifreshlength=lm(Trifreshmass~Trifreshlength)
lm.Trifreshlength
anova(lm.Trifreshlength)
summary(lm.Trifreshlength)
plot(lm.Trifreshlength)
plot(Trifreshlength, Trifreshmass, main= "Trichoptera Fresh Mass vs. Length", xlab="Trichoptera Fresh Length (mm)", ylab="Trichoptera Fresh Mass (mg)")
lines(Tri.fresh$length.after..mm., fitted(lm.Trifreshlength), col="blue")

#Linear model Hydro
lm.Hydrofresh=lm(Hydrofreshmass~HydrofreshSA)
lm.Hydrofresh
anova(lm.Hydrofresh)
summary(lm.Hydrofresh)
plot(HydrofreshSA, Hydrofreshmass, main= "Hydropsychidae Fresh Mass vs. Surface Area", xlab="Hydropsychidae Fresh Surface Area (sq mm)", ylab="Hydropsychidae Fresh Mass (mg)") 
lines(Tri.fresh.Hydro$SA.after..mm2., fitted(lm.Hydrofresh), col="blue")
plot(lm.Hydrofresh)

lm.Hydrofreshlength=lm(Hydrofreshmass~Hydrofreshlength)
lm.Hydrofreshlength
anova(lm.Hydrofreshlength)
summary(lm.Hydrofreshlength)
plot(Hydrofreshlength, Hydrofreshmass, main= "Hydropsychidae Fresh Mass vs. Length", xlab="Hydropsychidae Fresh Length (mm)", ylab="Hydropsychidae Fresh Mass (mg)") 
lines(Tri.fresh.Hydro$length.after..mm., fitted(lm.Hydrofreshlength), col="blue")
plot(lm.Hydrofreshlength)

#Linear model Phry
lm.Phryfresh=lm(Phryfreshmass~PhryfreshSA)
lm.Phryfresh
anova(lm.Phryfresh)
summary(lm.Phryfresh)
plot(PhryfreshSA, Phryfreshmass, main= "Phryganeidae Fresh Mass vs. Surface Area", xlab="Phryganeidae Fresh Surface Area (sq mm)", ylab="Phryganeidae Fresh Mass (mg)") 
lines(Tri.fresh.Phry$SA.after..mm2., fitted(lm.Phryfresh), col="blue")
plot(lm.Phryfresh)

lm.Phryfreshlength=lm(Phryfreshmass~Phryfreshlength)
lm.Phryfreshlength
anova(lm.Phryfreshlength)
summary(lm.Phryfreshlength)
plot(Phryfreshlength, Phryfreshmass, main= "Phryganeidae Fresh Mass vs. Length", xlab="Phryganeidae Fresh Length (mm)", ylab="Phryganeidae Fresh Mass (mg)") 
lines(Tri.fresh.Phry$length.after..mm., fitted(lm.Phryfreshlength), col="blue")
plot(lm.Phryfreshlength)

#Linear model Poly
lm.Polyfresh=lm(Polyfreshmass~PolyfreshSA)
lm.Polyfresh
anova(lm.Polyfresh)
summary(lm.Polyfresh)
plot(PolyfreshSA, Polyfreshmass, main= "Polycentropodidae Fresh Mass vs. Surface Area", xlab="Polycentropodidae Fresh Surface Area (sq mm)", ylab="Polycentropodidae Fresh Mass (mg)") 
lines(Tri.fresh.Poly$SA.after..mm2., fitted(lm.Polyfresh), col="blue")
plot(lm.Polyfresh)

lm.Polyfreshlength=lm(Polyfreshmass~Polyfreshlength)
lm.Polyfreshlength
anova(lm.Polyfreshlength)
summary(lm.Polyfreshlength)
plot(Polyfreshlength, Polyfreshmass, main= "Polycentropodidae Fresh Mass vs. Length", xlab="Polycentropodidae Fresh Length (mm)", ylab="Polycentropodidae Fresh Mass (mg)") 
lines(Tri.fresh.Poly$length.after..mm., fitted(lm.Polyfreshlength), col="blue")
plot(lm.Polyfreshlength)

#Linear model Rhya
lm.Rhyafresh=lm(Rhyafreshmass~RhyafreshSA)
lm.Rhyafresh
anova(lm.Rhyafresh)
summary(lm.Rhyafresh)
plot(RhyafreshSA, Rhyafreshmass, main= "Rhyacophilidae Fresh Mass vs. Surface Area", xlab="Rhyacophilidae Fresh Surface Area (sq mm)", ylab="Rhyacophilidae Fresh Mass (mg)") 
lines(Tri.fresh.Rhya$SA.after..mm2., fitted(lm.Rhyafresh), col="blue")
plot(lm.Rhyafresh)

lm.Rhyafreshlength=lm(Rhyafreshmass~Rhyafreshlength)
lm.Rhyafreshlength
anova(lm.Rhyafreshlength)
summary(lm.Rhyafreshlength)
plot(Rhyafreshlength, Rhyafreshmass, main= "Rhyacophilidae Fresh Mass vs. Length", xlab="Rhyacophilidae Fresh Length (mm)", ylab="Rhyacophilidae Fresh Mass (mg)") 
lines(Tri.fresh.Rhya$length.after..mm., fitted(lm.Rhyafreshlength), col="blue")
plot(lm.Rhyafreshlength)

#Linear model alcohol Trichoptera
lm.Trialcohol=lm(Trialcoholmass~TrialcoholSA)
lm.Trialcohol
anova(lm.Trialcohol)
summary(lm.Trialcohol)
plot(TrialcoholSA, Trialcoholmass, main= "Trichoptera Alcohol Mass vs. Surface Area", xlab="Trichoptera Alcohol Surface Area (sq mm)", ylab="Trichoptera Alcohol Mass (mg)")
lines(Tri.alcohol$SA.after..mm2., fitted(lm.Trialcohol), col="red")
plot(lm.Trialcohol)

lm.Trialcohollength=lm(Trialcoholmass~Trialcohollength)
lm.Trialcohollength
anova(lm.Trialcohollength)
summary(lm.Trialcohollength)
plot(Trialcohollength, Trialcoholmass,main= "Trichoptera Alcohol Mass vs. Length", xlab="Trichoptera Alcohol Length (mm)", ylab="Trichoptera Alcohol Mass (mg)")
lines(Tri.alcohol$length.after..mm., fitted(lm.Trialcohollength), col="red")
plot(lm.Trialcohollength)

#Linear model Hydro
lm.Hydroalcohol=lm(Hydroalcoholmass~HydroalcoholSA)
lm.Hydroalcohol
anova(lm.Hydroalcohol)
summary(lm.Hydroalcohol)
plot(HydroalcoholSA, Hydroalcoholmass, main= "Hydropsychidae Alcohol Mass vs. Surface Area", xlab="Hydropsychidae Alcohol Surface Area (sq mm)", ylab="Hydropsychidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Hydro$SA.after..mm2., fitted(lm.Hydroalcohol), col="red")
plot(lm.Hydroalcohol)

lm.Hydroalcohollength=lm(Hydroalcoholmass~Hydroalcohollength)
lm.Hydroalcohollength
anova(lm.Hydroalcohollength)
summary(lm.Hydroalcohollength)
plot(Hydroalcohollength, Hydroalcoholmass, main= "Hydropsychidae Alcohol Mass vs. Length", xlab="Hydropsychidae Alcohol Length (mm)", ylab="Hydropsychidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Hydro$length.after..mm., fitted(lm.Hydroalcohollength), col="red")
plot(lm.Hydroalcohollength)

#Linear model Phry
lm.Phryalcohol=lm(Phryalcoholmass~PhryalcoholSA)
lm.Phryalcohol
anova(lm.Phryalcohol)
summary(lm.Phryalcohol)
plot(PhryalcoholSA, Phryalcoholmass, main= "Phryganeidae Alcohol Mass vs. Surface Area", xlab="Phryganeidae Alcohol Surface Area (sq mm)", ylab="Phryganeidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Phry$SA.after..mm2., fitted(lm.Phryalcohol), col="red")
plot(lm.Phryalcohol)

lm.Phryalcohollength=lm(Phryalcoholmass~Phryalcohollength)
lm.Phryalcohollength
anova(lm.Phryalcohollength)
summary(lm.Phryalcohollength)
plot(Phryalcohollength, Phryalcoholmass, main= "Phryganeidae Alcohol Mass vs. Length", xlab="Phryganeidae Alcohol Length (mm)", ylab="Phryganeidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Phry$length.after..mm., fitted(lm.Phryalcohollength), col="red")
plot(lm.Phryalcohollength)

#Linear model Poly
lm.Polyalcohol=lm(Polyalcoholmass~PolyalcoholSA)
lm.Polyalcohol
anova(lm.Polyalcohol)
summary(lm.Polyalcohol)
plot(PolyalcoholSA, Polyalcoholmass, main= "Polycentropodidae Alcohol Mass vs. Surface Area", xlab="Polycentropodidae Alcohol Surface Area (sq mm)", ylab="Polycentropodidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Poly$SA.after..mm2., fitted(lm.Polyalcohol), col="red")
plot(lm.Polyalcohol)

lm.Polyalcohollength=lm(Polyalcoholmass~Polyalcohollength)
lm.Polyalcohollength
anova(lm.Polyalcohollength)
summary(lm.Polyalcohollength)
plot(Polyalcohollength, Polyalcoholmass, main= "Polycentropodidae Alcohol Mass vs. Length", xlab="Polycentropodidae Alcohol Length (mm)", ylab="Polycentropodidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Poly$length.after..mm., fitted(lm.Polyalcohollength), col="red")
plot(lm.Polyalcohollength)

#Linear model Rhya
lm.Rhyaalcohol=lm(Rhyaalcoholmass~RhyaalcoholSA)
lm.Rhyaalcohol
anova(lm.Rhyaalcohol)
summary(lm.Rhyaalcohol)
plot(RhyaalcoholSA, Rhyaalcoholmass, main= "Rhyacophilidae Alcohol Mass vs. Surface Area", xlab="Rhyacophilidae Alcohol Surface Area (sq mm)", ylab="Rhyacophilidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Rhya$SA.after..mm2., fitted(lm.Rhyaalcohol), col="red")
plot(lm.Rhyaalcohol)

lm.Rhyaalcohollength=lm(Rhyaalcoholmass~Rhyaalcohollength)
lm.Rhyaalcohollength
anova(lm.Rhyaalcohollength)
summary(lm.Rhyaalcohollength)
plot(Rhyaalcohollength, Rhyaalcoholmass, main= "Rhyacophilidae Alcohol Mass vs. Length", xlab="Rhyacophilidae Alcohol Length (mm)", ylab="Rhyacophilidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Rhya$length.after..mm., fitted(lm.Rhyaalcohollength), col="red")
plot(lm.Rhyaalcohollength)

#Linear model Bra
lm.Braalcohol=lm(Braalcoholmass~BraalcoholSA)
lm.Braalcohol
anova(lm.Braalcohol)
summary(lm.Braalcohol)
plot(BraalcoholSA, Braalcoholmass, main= "Bracgycentridae Alcohol Mass vs. Surface Area", xlab="Bracgycentridae Alcohol Surface Area (sq mm)", ylab="Bracgycentridae Alcohol Mass (mg)") 
lines(Tri.alcohol.Bra$SA.after..mm2., fitted(lm.Braalcohol), col="red")
plot(lm.Braalcohol)

lm.Braalcohollength=lm(Braalcoholmass~Braalcohollength)
lm.Braalcohollength
anova(lm.Braalcohollength)
summary(lm.Braalcohollength)
plot(Braalcohollength, Braalcoholmass, main= "Bracgycentridae Alcohol Mass vs. Length", xlab="Bracgycentridae Alcohol Length (mm)", ylab="Bracgycentridae Alcohol Mass (mg)") 
lines(Tri.alcohol.Bra$length.after..mm., fitted(lm.Braalcohollength), col="red")
plot(lm.Braalcohollength)

#Linear model ice Trichoptera
lm.Triice=lm(Triicemass~TriiceSA)
lm.Triice
anova(lm.Triice)
summary(lm.Triice)
plot(TriiceSA, Triicemass, main= "Trichoptera Ice Mass vs. Surface Area", xlab="Trichoptera Ice Surface Area (sq mm)", ylab="Trichoptera Ice Mass (mg)")
lines(Tri.ice$SA.after..mm2., fitted(lm.Triice), col="green")
plot(lm.Triice)

lm.Triicelength=lm(Triicemass~Triicelength)
lm.Triicelength
anova(lm.Triicelength)
summary(lm.Triicelength)
plot(lm.Triicelength)
plot(Triicelength, Triicemass, main= "Trichoptera Ice Mass vs. Length", xlab="Trichoptera Ice Length (mm)", ylab="Trichoptera Ice Mass (mg)")
lines(Tri.ice$length.after..mm., fitted(lm.Triicelength), col="green")

#Linear model ice Hydro
lm.Hydroice=lm(Hydroicemass~HydroiceSA)
lm.Hydroice
anova(lm.Hydroice)
summary(lm.Hydroice)
plot(HydroiceSA, Hydroicemass, main= "Hydropsychidae Ice Mass vs. Surface Area", xlab="Hydropsychidae Ice Surface Area (sq mm)", ylab="Hydropsychidae Ice Mass (mg)")
lines(Tri.ice.Hydro$SA.after..mm2., fitted(lm.Hydroice), col="green")
plot(lm.Hydroice)

lm.Hydroicelength=lm(Hydroicemass~Hydroicelength)
lm.Hydroicelength
anova(lm.Hydroicelength)
summary(lm.Hydroicelength)
plot(lm.Hydroicelength)
plot(Hydroicelength, Hydroicemass, main= "Hydropsychidae Ice Mass vs. Length", xlab="Hydropsychidae Ice Length (mm)", ylab="Hydropsychidae Ice Mass (mg)")
lines(Tri.ice.Hydro$length.after..mm., fitted(lm.Hydroicelength), col="green")

#Linear model ice Poly
lm.Polyice=lm(Polyicemass~PolyiceSA)
lm.Polyice
anova(lm.Polyice)
summary(lm.Polyice)
plot(PolyiceSA, Polyicemass, main= "Polycentropodidae Ice Mass vs. Surface Area", xlab="Polycentropodidae Ice Surface Area (sq mm)", ylab="Polycentropodidae Ice Mass (mg)")
lines(Tri.ice.Poly$SA.after..mm2., fitted(lm.Polyice), col="green")
plot(lm.Polyice)

lm.Polyicelength=lm(Polyicemass~Polyicelength)
lm.Polyicelength
anova(lm.Polyicelength)
summary(lm.Polyicelength)
plot(lm.Polyicelength)
plot(Polyicelength, Polyicemass, main= "Polycentropodidae Ice Mass vs. Length", xlab="Polycentropodidae Ice Length (mm)", ylab="Polycentropodidae Ice Mass (mg)")
lines(Tri.ice.Poly$length.after..mm., fitted(lm.Polyicelength), col="green")

#Linear model ice Rhya
lm.Rhyaice=lm(Rhyaicemass~RhyaiceSA)
lm.Rhyaice
anova(lm.Rhyaice)
summary(lm.Rhyaice)
plot(RhyaiceSA, Rhyaicemass, main= "Rhyacophilidae Ice Mass vs. Surface Area", xlab="Rhyacophilidae Ice Surface Area (sq mm)", ylab="Rhyacophilidae Ice Mass (mg)")
lines(Tri.ice.Rhya$SA.after..mm2., fitted(lm.Rhyaice), col="green")
plot(lm.Rhyaice)

lm.Rhyaicelength=lm(Rhyaicemass~Rhyaicelength)
lm.Rhyaicelength
anova(lm.Rhyaicelength)
summary(lm.Rhyaicelength)
plot(lm.Rhyaicelength)
plot(Rhyaicelength, Rhyaicemass, main= "Rhyacophilidae Ice Mass vs. Length", xlab="Rhyacophilidae Ice Length (mm)", ylab="Rhyacophilidae Ice Mass (mg)")
lines(Tri.ice.Rhya$length.after..mm., fitted(lm.Rhyaicelength), col="green")

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