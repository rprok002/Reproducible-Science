Tri=read.csv(file.choose())
newTri <- na.omit(Tri)
length=newTri[,7]
condition=newTri[,3]
condition
Tri.ice <- subset(newTri, condition == 1)
Tri.alcohol <- subset(newTri, condition == 2 & length < 100)
Tri.fresh <- subset(newTri, condition == 3)
TrifreshSA=Tri.fresh[,5]
Trifreshlength=Tri.fresh[,7]
Trifreshmass=Tri.fresh[,8]
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
# Selfstart for the trc:
TrifreshSAModel <- function(TrifreshSA, a, b) {
  y=a * exp(b*TA)
  return(y)
}



# Create a function to find initial values for the selfstart function:
trc.int <- function (mCall, LHS, data){
  x <- data$TA
  y <- data$NEE
  
  a <-1.00703982 + -0.08089044* (min(na.omit(y)))
  b <- 0.051654 + 0.001400 * (min(na.omit(y)))
  
  value = list(a, b)
  names(value) <- mCall[c("a", "b")]
  return(value)
}

# Selfstart Function
SS.trc <- selfStart(model=trcModel,initial= trc.int)

try(for(j in unique(day$MONTH)){

# Determines starting values:
iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = night[which(night$MONTH == j),])
# Fits night response curve:
y4 <- try(nee.night(night[which(night$MONTH == j),]), silent=T)
# Extracts data and saves it in the dataframe
try(parms.Month.night[c(parms.Month.night$MONTH == j ), 2:7 ] <- cbind(y4), silent=T)
rm(y4) 
}, silent=T)
parms.Month.night

parms.Month.night <- data.frame(
  
  MONTH=numeric(),
  
  a=numeric(),
  
  b=numeric(),
  
  a.pvalue=numeric(),
  
  b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)

parms.Month.night[1:12, 1] <- seq(1,12,1) # Creates time file to merge with parm file

nee.night <- function(dataframe){y.df = nls(NEE ~ a * exp(b*TA),
                                            
                                            dataframe, start=list(a= iv$a , b=iv$b ),
                                            
                                            na.action=na.exclude, trace=F,
                                            
                                            control=nls.control(warnOnly=T))

y.df <- as.data.frame(cbind(t(coef(summary(y.df))[1:2, 1]), t(coef(summary(y.df)) [1:2, 4])))

names(y.df) <- c("a", "b", "a.pvalue", "b.pvalue")                     

return(y.df)}

# This loop fits monthly models (1:12):
#Bootstraps to create more data based off of function created by initial values to see if created data still fits the model
try(for(j in unique(night$MONTH)){
  
  # Determines starting values:
  iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = night[which(night$MONTH == j),])
  # Fits night response curve:
  y4 <- try(nee.night(night[which(night$MONTH == j),]), silent=T)
  # Extracts data and saves it in the dataframe
  try(parms.Month.night[c(parms.Month.night$MONTH == j ), 2:5 ] <- cbind(y4), silent=T)
  rm(y4)
}, silent=T)
parms.Month.night




#Bootstrapping
# Create file to store parms and se
boot.NEE.night <- data.frame(parms.Month.night[, c("MONTH")]); names (boot.NEE.night) <- "MONTH"
boot.NEE.night$a.est <- 0
boot.NEE.night$b.est<- 0
boot.NEE.night$a.se<- 0
boot.NEE.night$b.se<- 0


for ( j in unique(boot.NEE.night$Month)){
  
  y5 <-night[which(night$MONTH == j),] # Subsets data
      
# Determines the starting values:
iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = y5)

# Fit curve:
night.fit <- nls( NEE ~ a * exp(b*TA), data=y5,
                  start=list(a= iv$a , b= iv$b),
                  na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))

# Bootstrap and extract values:

  try(results <- nlsBoot(night.fit, niter=100 ), silent=T)
  try(a <- t(results$estiboot)[1, 1:2], silent=T)
  try(names(a) <- c('a.est','b.est'), silent=T)
  try(b <- t(results$estiboot)[2, 1:2], silent=T)
  try(names(b) <- c('a.se','b.se'), silent=T)
  try(c <- t(data.frame(c(a,b))), silent=T)
  # Add bootstrap data to dataframe:
  try(boot.NEE.night[c(boot.NEE.night$MONTH == j), 2:5] <- c[1, 1:4], silent=T)
  try(rm(night.fit, a, b, c, results, y5), silent=T)
}
trc <- merge( parms.Month.night, boot.NEE.night,by.x="MONTH", by.y="MONTH") # Merge dataframes
trc

res.trc <- nlsResiduals(trc)
par(mfrow=c(2,2))
plot(res.trc, which=1)# Residulas vs fitted values (Constant Variance)
plot(res.trc, which=3) # Standardized residuals
plot(res.trc, which=4) # Autocorrelation
plot(res.trc, which=5) # Histogram (Normality)