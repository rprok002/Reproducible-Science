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

#Get initial values
iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"),
                 data = night[which(night$MONTH >0),])
iv

#nls for new data set
y = nls( NEE ~ a * exp(b*TA), night,
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
  try(parms.Month.night[c(parms.Month.night$MONTH == j ), 2:7 ] <- cbind(y4), silent=T)
  rm(y4)
}, silent=T)
parms.Month.night


nee.night(night)
#plot scatter plot and fitted line
plot(night$TA,night$NEE)

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
  try(a <- t(results$estiboot)[1, 1:3], silent=T)
  try(names(a) <- c('a.est','b.est'), silent=T)
  try(b <- t(results$estiboot)[2, 1:3], silent=T)
  try(names(b) <- c('a.se','b.se'), silent=T)
  try(c <- t(data.frame(c(a,b))), silent=T)
  # Add bootstrap data to dataframe:
  try(boot.NEE.night[c(boot.NEE.night$MONTH == j), 2:7] <- c[1, 1:6], silent=T)
  try(rm(night.fit, a, b, c, results, y5), silent=T)
}
trc <- merge( parms.Month.night, boot.NEE.night, by.x="MONTH", by.y="MONTH") # Merge dataframes
