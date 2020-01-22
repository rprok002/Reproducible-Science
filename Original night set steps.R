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
plot(jan$TA,jan$NEE)
