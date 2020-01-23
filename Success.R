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
