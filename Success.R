load("~/Desktop/NLM_Workshop.RData")
library(nlstools)
#Create a dataframe to store month parameter values (parms.Month.night).
# Selfstart for the trc:
trcModel <- function(TA, a, b) {
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



parms.Month <- data.frame(
  
  MONTH=numeric(),
  
  a=numeric(),
  
  b=numeric(),
  
  a.pvalue=numeric(),
  
  b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)

parms.Month [1:12, 1] <- seq(1,12,1) # Creates time file to merge with parm file

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
  try(parms.Month[c(parms.Month$MONTH == j ), 2:5 ] <- cbind(y4), silent=T)
  rm(y4)
}, silent=T)

parms.Month



#Bootstrapping
# Create file to store parms and se
boot.NEE <- data.frame(parms.Month[, c("MONTH")]); names (boot.NEE) <- "MONTH"
boot.NEE$a.est <- 0
boot.NEE$b.est <- 0
boot.NEE$a.se <- 0
boot.NEE$b.se <- 0


for ( j in unique(boot.NEE$MONTH)){
  
  y1 <-night[which(night$MONTH == j),]  #Subsets data
  
  # Determines the starting values:
  iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = y1)
  
  # Fit curve:
  night.fit <- nls( NEE ~ (a * exp(b * TA)), 
                    data=y1, start=list(a= iv$a , b= iv$b),
                    na.action=na.exclude, trace=F, 
                    control=nls.control(warnOnly=T))
  
  # Bootstrap and extract values:
  try(results <- nlsBoot(night.fit, niter=100 ), silent=T)
  try(a <- t(results$estiboot)[1, 1:2], silent=T)
  try(names(a) <- c('a.est','b.est'), silent=T)
  try(b <- t(results$estiboot)[2, 1:2], silent=T)
  try(names(b) <- c('a.se','b.se'), silent=T)
  try(c <- t(data.frame(c(a,b))), silent=T)
  # Add bootstrap data to dataframe:
  try(boot.NEE[c(boot.NEE$MONTH == j), 2:5] <- c[1, 1:4], silent=T)
  try(rm(night.fit, a, b, c, results, y1), silent=T)
}
trc <- merge( parms.Month, boot.NEE,by.x="MONTH", by.y="MONTH") # Merge dataframes
trc
library(xtable)
library(knitr)



excelTable(data=trc)

load("~/Desktop/NLM_Workshop.RData")
library(nlstools)
lrcModel <- function(PAR, a1, ax, r) {
  NEE <- (a1 * PAR * ax)/(a1 * PAR + ax) + r
  return(NEE)
}

lrc.int <- function (mCall, LHS, data){
  x <- data$PAR
  y <- data$NEE
  r <- max(na.omit(y), na.rm=T) 
  ax <- min(na.omit(y), na.rm=T) 
  a1 <- (r + ax)/2 
  
  a1[a1 > 0]<- -0.1
  r[r > 50] <- ax*-1
  r[r < 0] <- 1
  value = list(a1, ax, r) 
  names(value) <- mCall[c("a1", "ax", "r")] 
  return(value)
}


SS.lrc <- selfStart(model=lrcModel,initial= lrc.int)

iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"),
                 data = day[which(day$MONTH == 07),])
iv
### Create a dataframe to store month parameter values a1, ax and r (parms.Month):

parms.Month <- data.frame(
  MONTH=numeric(),
  a1=numeric(),
  ax=numeric(),
  r=numeric(),
  a1.pvalue=numeric(),
  ax.pvalue=numeric(),
  r.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
parms.Month[1:12, 1] <- seq(1,12,1) 

### Write a function to fit the model and extract paramter values a1, ax and r (nee.day):

nee.day <- function(dataframe){ y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, dataframe,
                                         start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
                                         na.action=na.exclude, trace=F,
                                         control=nls.control(warnOnly=T))

y.df <- as.data.frame(cbind(t(coef(summary(y)) [1:3, 1]), t(coef(summary(y)) [1:3, 4])))
names(y.df) <-c("a1","ax", "r", "a1.pvalue", "ax.pvalue", "r.pvalue")
return (y.df )}

### Write a loop to fit monthly curves and add paramters to a dataframe (parms.Month):

try(for(j in unique(day$MONTH)){
  
  
  iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = day[which(day$MONTH == j),])
  
  y3 <- try(nee.day(day[which(day$MONTH == j),]), silent=T)
  
  try(parms.Month[c(parms.Month$MONTH == j ), 2:7 ] <- cbind(y3), silent=T)
  rm(y3)
}, silent=T)
parms.Month

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
