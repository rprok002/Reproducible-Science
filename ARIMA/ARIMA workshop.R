load("~/FALCON HD/GitHub//Reproducible-Science/ARIMA/ARIMA_Workshop.RData")
library(zoo)
library(tseries)
library(forecast)
library(xts)

#Create timeseries objects:
#In the frequency parameter in the ts() object, we are specifying periodicity of the data, i.e., number of
#observations per period. Since we are using daily data, we have 30 observations per month.

nee <- ts( mangroves$nee, start= 1, frequency=30)

#Visualize data:
#A good starting point is to plot the timeseries and visually examine it for any outliers, volatility, or
#irregularities.

par(mfrow=c(1,1), mai=c(1.25,0.8,0.8, 0.5))
plot( nee, typ="l", ylab= "NEE", xlab="")


#You want to remove any outliers that could bias the model by skewing statistical summaries. R provides
#a convenient method for removing time series outliers: tsclean() as part of its forecast package. tsclean()
#identifies and replaces outliers using series smoothing and decomposition.

plot(nee)
lines(tsclean(nee), col="red")
nee <- tsclean(nee)

#Time series analysis involves trying to separate the time series into the seasonal, trend and irregular components.
#Deconstructing the series will help you understand its behavior and prepare a foundation for building an
#ARIMA model.
#The Seasonal component refers to fluctuations in the data related to calendar cycles. Usually, seasonality is
#fixed at some number; for instance, quarter or month of the year.
#Trend component is the overall pattern of the series. It consists of decreasing or increasing patterns that are
#not seasonal. This is estimated using moving averages.
#The part of the series that can't be attributed to the seasonal or trend components is referred to as residual
#or error.

#Decompose the timeseries:
nee.d <- decompose(nee, 'multiplicative')
plot(nee.d)

#Test for stationarity
#Fitting an ARIMA model requires the series to be stationary. A series is stationary when its mean, variance,
#and autocovariance are time invariant. This assumption makes intuitive sense: Since ARIMA uses previous
#lags of series to model its behavior, modeling stable series with consistent properties involves less uncertainty.
#The augmented Dickey-Fuller (ADF) test is a formal statistical test for stationarity. The null hypothesis
#assumes that the series is non-stationary. ADF procedure tests whether the change in Y can be explained by
#lagged value and a linear trend. If contribution of the lagged value to the change in Y is non-significant and
#there is a presence of a trend component, the series is non-stationary and null hypothesis will not be rejected.

# p-value < 0.05 indicates the TS is stationary
adf.test(nee)

#Detecting Autocorrelation:
#Autocorrelation plots (also known as ACF or the auto correlation function) are a useful visual tool in
#determining whether a series is stationary. ACF plots display correlation between a series and its lags.
#If the series is correlated with its lags then, generally, there are some trend or seasonal components and
#therefore its statistical properties are not constant over time.
#ACF plots can help in determining the order of the MA (q) model. Partial autocorrelation plots (PACF),
#display correlation between a variable and its lags that is not explained by previous lags. PACF plots are
#useful when determining the order of the AR(p) model.
#R plots 95% significance boundaries as blue dotted lines.

acf(nee, lag.max=45)
pacf(nee, lag.max=45)

#There are significant autocorrelations with many lags in the nee
#series, as shown by the ACF plot above. However, this could be due to carry-over correlation from early lags,
#since the PACF plot only shows a spike at certain lags.
#There are significant auto correlations at lags 1 and 2 and beyond. Partial correlation plots show a significant
#spike at multiple lags. This suggests that we might want to test models with AR or MA components of an
#order associated with the lags.

#Fitting an ARIMA Model
#Now let’s fit a model. The forecast package allows the user to explicitly specify the order of the model using
#the arima() function, or automatically generate a set of optimal (p, d, q) using auto.arima(). This function
#searches through combinations of order parameters and picks the set that optimizes model fit criteria. While
#auto.arima() can be very useful, it is still important to complete the steps above in order to understand the
#series and interpret model results.


#Where do I have spikes??
arima.nee1 <-auto.arima(nee, trace=TRUE)

#So now we have fitted a model, but does it make sense? Can we trust this model? We can start by examining
#ACF and PACF plots for model residuals. If model order parameters and structure are correctly specified, we
#would expect no significant autocorrelations present.
#Ideally, residuals should look like white noise, meaning they are normally distributed. A convenience function
#tsdisplay() can be used to plot these model diagnostics.
#Residuals plots show a smaller error range, more or less centered around 0. We can observe that AIC is
#smaller for the second model structure as well:

tsdisplay(residuals(arima.nee1), lag.max=45)

#There is a clear pattern present
#in ACF/PACF and model residuals plots repeating at lag 10. This suggests that our model may be better off
#with a different specification, such as p = 10 or q = 10.
#We can repeat the fitting process allowing for the MA(10 or 35) component and examine diagnostic plots
#again. This time, there are no significant autocorrelations present. If the model is not correctly specified, that
#will usually be reflected in residuals in the form of trends, skewedness, or any other patterns not captured by
#the model.

arima.nee2 <-arima(nee , order=c(10,1,3), seasonal= list(order=c(2,0,2)))

#To compare models, you use the AIC. You also want to compare observed versus predicted values.
AIC(arima.nee1, arima.nee2)
tsdisplay(residuals(arima.nee2), lag.max= 30)
par(mfrow=c(1,1))
plot(nee , typ="l", main="Original and ARIMA-modeled Data Series for NEE" , xlab="Time" , ylab="NEE (g C m2 day−1)"); lines(fitted(arima.nee2),col="red")

#Next, we just need test for independence. The Ljung-Box is a test of independence at all lags up to the one specified.
#Instead of testing randomness at each distinct lag, it tests the “overall” randomness based on a number of
#lags, and is therefore a portmanteau test. It is applied to the residuals of a fitted ARIMA model, not the
#original series, and in such applications the hypothesis actually being tested is that the residuals from the
#ARIMA model have no autocorrelation.

# Measuring for significant difference from white noise.
# You need a p-value greater than 0.05!

checkresiduals(arima.nee2, lag=36)

par(mfrow=c(1,1))
plot(nee , typ="l"); lines(fitted(arima.nee2),col="red")

#Now that you have captured the temporal dynamics in nee, the next step maybe to forecast NEE if you are
#interested in making projections. You can do this using the *forecast function. For fun, lets forecast nee for
#30 days.

plot(forecast(arima.nee2, h=30), main="Forecasts Regression with ARIMA" , xlab="Time" , ylab="NEE (g C m2 day−1)")
#Challenge)

#Exercise
#Create a timeseries object.

sal <- ts(mangroves$salinity.max, start= 1, frequency=30)

#Visualize data:

par(mfrow=c(1,1), mai=c(1.25,0.8,0.5, 0.5))
plot(sal , typ="l", ylab= "Salinity", xlab="")

#You want to remove any outliers that could bias the model by skewing statistical summaries.
#Visualize whether there are outliers and tsclean is needed
plot(sal , typ="l", ylab= "Salinity", xlab="")
lines(tsclean(sal) , col="red")

#Alter data series to take out calculated outliers
sal <- tsclean(sal)
#Check to make sure outliers were taken out
par(mfrow=c(1,1), mai=c(1.25,0.8,0.5, 0.5))
plot(sal , typ="l", ylab= "Salinity", xlab="")

#Decompose the time series.

sal.d <- decompose(sal, 'multiplicative')
plot(sal.d)

#Test for stationarity
#If your null hypothesis is not rejected, you can try differencing the time series using the function diff()
# p-value < 0.05 indicates the TS is stationary

adf.test(sal)
adf.test(diff(sal))

#Explore correlations
#ccf( diff(sal),nee, na.action = na.pass, lag.max=40, plot=TRUE)
ccf( diff(sal),nee, na.action = na.pass, lag.max=40, plot=TRUE)

#Explore Models of NEE.
arima.nee3 <-auto.arima(nee, xreg=c(diff(sal),0), trace=TRUE)

#Compare to current model
AIC(arima.nee2, arima.nee3 )

#So, adding salinity to nee did not improve the model. Maybe extreme salinity is more important. Lets create
#a salinity index to ID when salinity values are greater than 25n ppt.

sal.i <- sal
sal.i[sal.i < 25 ]<- 0
sal.i[sal.i >= 25 ]<- 1
plot(sal.i)

#Now try adding the extreme salinity indicator into the model to see if this is an improvement:

arima.nee4 <-auto.arima(nee, xreg=sal.i, trace=TRUE)
AIC(arima.nee2,arima.nee4 )

checkresiduals(arima.nee4, lag=36)

par(mfrow=c(1,1), mai=c(1.25,1.8,0.6, 1.5))
plot(nee , typ="l", main="Original and ARIMA-modeled Data Series 
     for NEE regulated by Maximum Salinity" , xlab="Time" , ylab="NEE (g C m2 day−1)"); lines(fitted(arima.nee4),col="blue")

par(mfrow=c(1,1), mai=c(1.25,0.8,0.5, 0.5))
plot(forecast(arima.nee4, h=30,xreg=sal.i), main="Forecasts from Max Salinity Regression with ARIMA" , xlab="Time" , ylab="NEE (g C m2 day−1)")
#Challenge
#Create a timeseries object

watertemp <- ts(mangroves$water.tmax, start= 1, frequency=30)

#Visualize data series

par(mfrow=c(1,1), mai=c(1.25,0.8,0.5, 0.5))
plot(watertemp , typ="l", ylab= "Max Water Temp", xlab="")
plot(watertemp , typ="l", ylab= "Max Water Temp", xlab="")
lines(tsclean(watertemp) , col="green")
sal <- tsclean(watertemp)

plot(watertemp , typ="l", ylab= "Max Water Temp", xlab="")
#data didn't really need cleaning, no true outliers but I ran it anyway just to make sure

#Decompose the time series.
watertemp.d <- decompose(watertemp, 'multiplicative')
plot(watertemp.d)

#Test for stationarity
## p-value < 0.05 indicates the TS is stationary
adf.test(watertemp)

#P value isn't significant, so not currently stationary. Trying diff() function

adf.test(diff(watertemp))
#P value is now significant, so data series is stationary. Moving on

#Detecting correlations:

ccf( diff(watertemp),nee, na.action = na.pass, lag.max=40, plot=TRUE)
arima.nee5 <-auto.arima(nee, xreg=c(diff(watertemp),0), trace=TRUE)
AIC(arima.nee2,arima.nee5 )
par(mfrow=c(1,2))
plot(nee , typ="l"); lines(fitted(arima.nee5),col="green")
plot(nee , typ="l"); lines(fitted(arima.nee2),col="red")
par(mfrow=c(1,1))
plot(nee , typ="l"); lines(fitted(arima.nee2),col="red")

par(mfrow=c(1,2))
plot(nee , typ="l"); lines(fitted(arima.nee4),col="purple")
plot(nee , typ="l"); lines(fitted(arima.nee5),col="green")

# Measuring for significant difference from white noise.
# You need a p-value greater than 0.05!
checkresiduals(arima.nee5, lag=36)

#Residuals have no autocorrelation

#Check to see if any correlations (though not autocorrelated) can still be accounted for to make it better
tsdisplay(residuals(arima.nee1), lag.max=45)

arima.nee6 <-arima(nee , order=c(10,1,1), seasonal= list(order=c(2,0,1)))
tsdisplay(residuals(arima.nee6), lag.max= 30)

#Even though not autocorrelated, this made it better
#Check residuals 

checkresiduals(arima.nee6, lag=36)
#Higher p-value, so no autocorrelation and even less correlated than it may have been before

#Compare models
AIC(arima.nee2,arima.nee6 )
AIC(arima.nee5,arima.nee6)

#arima.nee5 still better than arima.nee6
#Try extreme high temps above 33 degrees c

watertemp.e <- watertemp
watertemp.e[watertemp.e < 33 ]<- 0
watertemp.e[watertemp.e >= 33 ]<- 1
plot(watertemp.e)

arima.nee7 <-auto.arima(nee, xreg=watertemp.e, trace=TRUE)
AIC(arima.nee5,arima.nee7)

#Air temperature

airtemp <- ts(mangroves$tair, start= 1, frequency=30)

par(mfrow=c(1,1), mai=c(1.25,0.8,0.5, 0.5))
plot(airtemp , typ="l", ylab= "Max Water Temp", xlab="")
plot(airtemp , typ="l", ylab= "Max Water Temp", xlab="")
lines(tsclean(airtemp) , col="green")
airtempclean <- tsclean(airtemp)

#Take out the outliers
plot(airtempclean, typ="l", ylab= "Air Temp", xlab="")
airtemp.d <- decompose(airtempclean, 'multiplicative')
plot(airtemp.d)


#Test for stationarity
## p-value < 0.05 indicates the TS is stationary
adf.test(airtempclean)

#Not stationary
#If your null hypothesis is not rejected, you can try differencing the time series using the function diff()
adf.test(diff(airtempclean))

#Detecting correlations
ccf( diff(airtempclean),nee, na.action = na.pass, lag.max=40, plot=TRUE)
arima.nee8 <-auto.arima(nee, xreg=c(diff(airtempclean),0), trace=TRUE)

tsdisplay(residuals(arima.nee8), lag.max=45)
AIC(arima.nee4,arima.nee8 )
checkresiduals(arima.nee8, lag=36)

par(mfrow=c(1,1), mai=c(1.25,1.8,0.6, 1.5))
plot(nee , typ="l", main="Original and ARIMA-modeled Data Series 
     for NEE regulated by Air Temperature (°C)" , xlab="Time" , ylab="NEE (g C m2 day−1)"); lines(fitted(arima.nee8),col="purple")

par(mfrow=c(1,1), mai=c(1.25,0.8,0.5, 0.5))
plot(forecast(arima.nee8, h=30,xreg=airtempclean), lines(fitted(arima.nee8),col="purple"), main="Forecasts from Air Temperature Regression with ARIMA" , xlab="Time" , ylab="NEE (g C m2 day−1)")


df<- ARIMAtable[1:3,1:8]
