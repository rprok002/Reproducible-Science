library(nlstools)

'nlstools' has been loaded.

IMPORTANT NOTICE: Most nonlinear regression models and data set examples
related to predictive microbiolgy have been moved to the package 'nlsMicrobio'

par(mai=c(1,1,0.1,0.1))
plot(harv$TIMESTAMP, harv$NEE,
     ylab=expression(paste("NEE (",mu,"mol m"^{-2} ~ s^{-1} ~ ")" )), xlab="")
plot( NEE ~ PAR, data= day)
y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=day[which(day$MONTH == 07),],
         start=list(a1= -1 , ax= -1, r= 1),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))

summary(y)

# Create a function of the model
lrcModel <- function(PAR, a1, ax, r) {
  NEE <- (a1 * PAR * ax)/(a1 * PAR + ax) + r
  return(NEE)
}
# Initial: create a function that calculates the initial values of the data sets
lrc.int <- function (mCall, LHS, data){
  x <- data$PAR
  y <- data$NEE
  r <- max(na.omit(y), na.rm=T) 
  ax <- min(na.omit(y), na.rm=T) 
  a1 <- (r + ax)/2 
  
  # Create limits for the parameters:
  a1[a1 > 0]<- -0.1
  r[r > 50] <- ax*-1
  r[r < 0] <- 1
  value = list(a1, ax, r) 
  names(value) <- mCall[c("a1", "ax", "r")] 
  return(value)
}
#Selftstart function
SS.lrc <- selfStart(model=lrcModel,initial= lrc.int)

#Find initial values
iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"),
                 data = day[which(day$MONTH == 07),])
iv
#Use initial values
y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, day[which(day$MONTH == 07),],
         start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
summary(y)
#Here, the model converged. Now, let check assumptions
res.lrc <- nlsResiduals(y)
par(mfrow=c(2,2))
plot(res.lrc, which=1)# Residulas vs fitted values (Constant Variance)
plot(res.lrc, which=3) # Standardized residuals
plot(res.lrc, which=4) # Autocorrelation
plot(res.lrc, which=5) # Histogram (Normality)

#Run bootstrap results
results <- nlsBoot(y, niter=100 )
summary(results)
plot(results, type = "boxplot")

# Dataframe to store parms and se
parms.Month <- data.frame(
  MONTH=numeric(),
  a1=numeric(),
  ax=numeric(),
  r=numeric(),
  a1.pvalue=numeric(),
  ax.pvalue=numeric(),
  r.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
parms.Month[1:12, 1] <- seq(1,12,1) # Adds months to the file

nee.day <- function(dataframe){ y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, dataframe,
                                         start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
                                         na.action=na.exclude, trace=F,
                                         control=nls.control(warnOnly=T))
y.df <- as.data.frame(cbind(t(coef(summary(y)) [1:3, 1]), t(coef(summary(y)) [1:3, 4])))
names(y.df) <-c("a1","ax", "r", "a1.pvalue", "ax.pvalue", "r.pvalue")
return (y.df )}

#Write a loop to fit monthly curves and add paramters to a
#dataframe (parms.Month).
try(for(j in unique(day$MONTH)){
  # Determines starting values:
  iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = day[which(day$MONTH == j),])
  # Fits light response curve:
  y3 <- try(nee.day(day[which(day$MONTH == j),]), silent=T)
  # Extracts data and saves it in the dataframe
  try(parms.Month[c(parms.Month$MONTH == j ), 2:7 ] <- cbind(y3), silent=T)
  rm(y3)
}, silent=T)
parms.Month

#Bootstrapping
# Create file to store parms and se
boot.NEE <- data.frame(parms.Month[, c("MONTH")]); names (boot.NEE) <- "MONTH"
boot.NEE$a1.est <- 0
boot.NEE$ax.est<- 0
boot.NEE$r.est<- 0
boot.NEE$a1.se<- 0
boot.NEE$ax.se<- 0
boot.NEE$r.se<- 0
for ( j in unique(boot.NEE$Month)){
  y1 <-day[which(day$MONTH == j),] # Subsets data
  # Determines the starting values:
  iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = y1)
  # Fit curve:
  day.fit <- nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1,
                  start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
                  na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
  # Bootstrap and extract values:
  try(results <- nlsBoot(day.fit, niter=100 ), silent=T)
  try(a <- t(results$estiboot)[1, 1:3], silent=T)
  try(names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
  try( b <- t(results$estiboot)[2, 1:3], silent=T)
  try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
  try(c <- t(data.frame(c(a,b))), silent=T)
  # Add bootstrap data to dataframe:
  try(boot.NEE[c(boot.NEE$MONTH == j), 2:7] <- c[1, 1:6], silent=T)
  try(rm(day.fit, a, b, c, results, y1), silent=T)
}
lrc <- merge( parms.Month, boot.NEE, by.x="MONTH", by.y="MONTH") # Merge dataframes

#Challenge
#Create a dataframe to store month parameter values (parms.Month.night).
# Selfstart for the trc:
trcModel <- function(TA, a, b) {
  y=a * exp(b-TA)
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

iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"),
                 data = night[which(night$MONTH == 07),])
iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"),
                 data = night[which(night$MONTH == c(1,2,3,4,5,6,7,8,9,10,11,12)),])
iv
#nls for new data set
y = nls( NEE ~ a * exp(b-TA), night[which(night$MONTH ==c(11,9)),],
         start=list(a= iv$a , b= iv$b),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
summary(y)
        
# Sparkle questions:
#for the methods, is it the workflow steps and then refer to code in an appendix or something?
#why do we need photosynthetic potential for night data?
#is the first section needed (starting values plugging back into model?) That's not in the workflow but seems necessary
#where are the a and b values that the first workflow step refers to? For example, the parms.Month has the ax and a1 and such that is needed for the first workflow of the exercise, but where are the a and b values supposed to be?
#Warning message:
#In nls(NEE ~ a * exp(b - TA), night[which(night$MONTH == 7), ],  :
        # singular gradient
       #> summary(y)
       #Error in chol2inv(object$m$Rmat()) : 
        # element (1, 1) is zero, so the inverse cannot be computed
#Thoughts?

# Create Dataframe to store the data: Creates a blank data frame for data to be entered into

parms.Month <- data.frame(
  
  MONTH=numeric(),
  
  a=numeric(),
  
  b=numeric(),
  
  a.pvalue=numeric(),
  
  b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)

parms.Month[1:12, 1] <- seq(1,12,1) # Creates time file to merge with parm file

#Write a function to fit the model and extract paramters
#(nee.night).


#Functions: based off of initial values

nee.night <- function(dataframe){y.df = nls(NEE ~ a * exp(b-TA),
                                            
                                            dataframe, start=list(a= iv$a , b=iv$b ),
                                            
                                            na.action=na.exclude, trace=F,
                                            
                                            control=nls.control(warnOnly=T))

y.df <- as.data.frame(cbind(t(coef(summary(y.df))[1:2, 1]), t(coef(summary(y.df)) [1:2, 4])))



names(y.df) <- c("a", "b", "a.pvalue", "b.pvalue")                     

return(y.df)}
# This loop fits monthly models (1:12):
#Bootstraps to create more data based off of function created by initial values to see if created data still fits the model

try(for(j in unique(night$MONTH)){
  
  print(j)
  
  
  
  iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = night[which(night$MONTH == j),])
  
  
  
  y4 <- try(nee.night(night[which(night$MONTH == j),]), silent=T) # Fit night model
  
  
  
  try(parms.Month[c(parms.Month$MONTH == j ), 2:5 ] <- cbind(y4), silent=T)
  
  
  
  rm(y4)
  
}, silent=T)
#bunch of warnings because code broke at a=iv$a, meaning loop based on initial values and bootstrapping didn't work
#created data doesn't fit the model

# Create file to store parms and se: maybe making dataframe to store data created by bootstrapping?
#File name is boo.NEE
#Created a file with the column Month and 4 other columns
boot.NEE <- data.frame(parms.Month[, c("MONTH")]); names (boot.NEE) <- "MONTH"

boot.NEE$a.est<- 0

boot.NEE$b.est<- 0

boot.NEE$a.se<- 0

boot.NEE$b.se<- 0

# Night Model:

for ( j in unique(boot.NEE$MONTH)){
  
  print(j)
  
  y1 <-night[which(night$MONTH == j),]
  
  
  
  iv <- getInitial(NEE ~ SS.trc('TA',"a", "b"), data = y1)
  
  
  
  night.fit <- nls(NEE ~ a * exp(b-TA),
                   
                   data=y1, start=list(a= iv$a , b=iv$b ),
                   
                   na.action=na.exclude, trace=F,
                   
                   control=nls.control(warnOnly=T))
  
  
  
  results <- nlsBoot(night.fit, niter=100 )
  
  a <- t(results$estiboot)[1, 1:2]
  
  names(a) <- c('a.est', 'b.est')
  
  b <- t(results$estiboot)[2, 1:2]
  
  names(b) <- c('a.se', 'b.se')
  
  c <- t(data.frame(c(a,b)))
  
  boot.NEE[c(boot.NEE$MONTH == j), 2:5] <- c[1, 1:4]
  
  rm(day.fit, a, b, c, results, y1)
  
}



trc <- merge( parms.Month, boot.NEE)
#new trc with the zeros and the p values and such 
trc

#subset the data by month
jan <- subset(night, night$MONTH==01)
feb <- subset(night, night$MONTH==02)
mar <- subset(night, night$MONTH==03)
apr <- subset(night, night$MONTH==04)
may <- subset(night, night$MONTH==05)
jun <- subset(night, night$MONTH==06)
jul <- subset(night, night$MONTH==07)
aug <- subset(night, night$MONTH==08)
sep <- subset(night, night$MONTH==09)
oct <- subset(night, night$MONTH==10)
nov <- subset(night, night$MONTH==11)
dec <- subset(night, night$MONTH==12)

#determine what TA values work for model
#know from running model before that apr-oct don't work in the model
#model is based off TA values, so something must be throwing the model off
#Determine min and max of TA for each month

month=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
min=c(min(jan$TA),min(feb$TA),min(mar$TA),min(apr$TA),min(may$TA),min(jun$TA),min(jul$TA),min(aug$TA),min(sep$TA),min(oct$TA),min(nov$TA),min(dec$TA))
max=c(max(jan$TA),max(feb$TA),max(mar$TA),max(apr$TA),max(may$TA),max(jun$TA),max(jul$TA),max(aug$TA),max(sep$TA),max(oct$TA),max(nov$TA),max(dec$TA))

values=data.frame(month,min,max)
values

par(mfrow=c(2,1))
plot(min)
plot(max)


#Values that don't seem to work are 22.2 degrees celcius and higher. 21.6 degrees celcius works, but we don't know for range of 21.6-22.2 degrees celcius, so we are going to be conservative and go with 21.6 degrees celcius
#TA values that work for model
work <- subset(night,night$TA<21.6)
#TA values that don't work for model
dontwork <- subset(night,night$TA>21.6)

#Create a dataframe to store month parameter values (parms.Month.night).
# Selfstart for the trc:
trcworkModel <- function(TA, a, b) {
  y=a * exp(b-TA)
  return(y)
}

# Create a function to find initial values for the selfstart function:
trcwork.int <- function (mCall, LHS, data){
  x <- work$TA
  y <- work$NEE
  
  a <-1.00703982 + -0.08089044* (min(na.omit(y)))
  b <- 0.051654 + 0.001400 * (min(na.omit(y)))
  
  value = list(a, b)
  names(value) <- mCall[c("a", "b")]
  return(value)
}

# Selfstart Function
SS.trcwork <- selfStart(model=trcworkModel,initial= trcwork.int)
#rerun model with values that work
#get initial values for new dataset

iv <- getInitial(NEE ~ SS.trcwork('TA', "a", "b"),
                 data = work)
iv

#nls for new data set
y = nls( NEE ~ a * exp(b-TA), work,
         start=list(a= iv$a , b= iv$b),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
summary(y)

# Create Dataframe to store the data: Creates a blank data frame for data to be entered into

parms.Month.night <- data.frame(
  
  MONTH=numeric(),
  
  a=numeric(),
  
  b=numeric(),
  
  a.pvalue=numeric(),
  
  b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)

parms.Month.night[1:12, 1] <- seq(1,12,1) # Creates time file to merge with parm file

#Write a function to fit the model and extract paramters
#(nee.night.work).


#Functions: based off of initial values

nee.night.work <- function(dataframe){y.df = nls(NEE ~ a * exp(b-TA),
                                            
                                            dataframe, start=list(a= iv$a , b=iv$b ),
                                            
                                            na.action=na.exclude, trace=F,
                                            
                                            control=nls.control(warnOnly=T))

y.df <- as.data.frame(cbind(t(coef(summary(y.df))[1:2, 1]), t(coef(summary(y.df)) [1:2, 4])))



names(y.df) <- c("a", "b", "a.pvalue", "b.pvalue")                     

return(y.df)}

# This loop fits monthly models (1:12):
#Bootstraps to create more data based off of function created by initial values to see if created data still fits the model

try(for(j in unique(work$MONTH)){
  
  print(j)
  
  
  
  iv <- getInitial(NEE ~ SS.trcwork('TA', "a", "b"), data = work[which(work$MONTH == j),])
  
  
  
  y4 <- try(nee.night.work(work[which(work$MONTH == j),]), silent=T) # Fit night model
  
  
  
  try(parms.Month.night[c(parms.Month.night$MONTH == j ), 2:5 ] <- cbind(y4), silent=T)
  
  
  
  rm(y4)
  
}, silent=T)