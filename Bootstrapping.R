library(nlstools)

'nlstools' has been loaded.

IMPORTANT NOTICE: Most nonlinear regression models and data set examples
related to predictive microbiolgy have been moved to the package 'nlsMicrobio'

par(mai=c(1,1,0.1,0.1))
plot(harv$TIMESTAMP, harv$NEE,
     ylab=expression(paste("NEE (",mu,"mol m"^{-2} ~ s^{-1} ~ ")" )), xlab="")
