---
output:
  word_document: default
  html_document: default
---
Workshop 2\_Nonlinear Models
================
Rachel Prokopius
1/23/2020

# Objectives

The objective of the analysis was to fit light-response curves to data
collected at Harvard Forest in Massachusetts in order to better estimate
net ecosystem exchange (NEE) during periods of photosynthesis (day) and
plant respiration (night). These estimations will inform researchers
about the photosynthetic potential and ecosystem respiration for the
studied area of Harvard Forest.

# Methods

## Site Information (include a map of the harvard forest site)

![site image and
map](C:/Users/Rachel%20Prokopius/Documents/FALCON%20HD/Graduate%20School/First%20Year/Spring%20Semester/Quantitative%20Ecology/Reproducible%20Science/Harvard%20Forest.jpg)

###### Images from Google Maps and <https://harvardforest.fas.harvard.edu/about-us>

Harvard Forest is a deciduous forest that is part of the sciences
program at Harvard College. The specific study site is found at the
following GPS coordinates:

Latitude: 42.53 Longitude: -72.19 Elevation: 330

The study site is a cool and moist temperature forest, with mean annual
temperatures ranging from -7°C to 20°C. Precipitation is fairly
consistent throughout the year.The dominant tree species found in the
area are Red oak (*Quercus rubra*), Red maple (*Acer rubrum*), Black
birch (*Betula lenta*), White pine (*Pinus strobus*)and Eastern hemlock
(*Tsuga canadensis*).

Data was collected from Environmental Measurement Station Eddy Flux
Towers (EMS) in the study area. The data set used in the analysis
includes hourly measurements of NEE in exchange per unit ground area,
air temperature in °C, and photosynthetically active radiation (PAR) in
nanometers. Measurements were attempted every hour by the EMS tower
beginning at 1 am on January 1st, 1991, and concluding at midnight on
January 1st, 2017.

## Photosynthetic Potential

The equation used to estimate photosynthetic potential of the study site
was NEE \~ (a1 \* PAR \* ax)/(a1 \* PAR + ax) + r, where a1, ax and r
are all parameters that need to be estimated for the model.

### Estimate initial values for the photosynthetic potential model using selfStart:

lrcModel \<- function(PAR, a1, ax, r) { NEE \<- (a1 \* PAR \* ax)/(a1 \*
PAR + ax) + r return(NEE) }

lrc.int \<- function (mCall, LHS, data){ x \<- data\(PAR y <- data\)NEE
r \<- max(na.omit(y), na.rm=T) ax \<- min(na.omit(y), na.rm=T) a1 \<- (r
+ ax)/2

a1\[a1 \> 0\]\<- -0.1 r\[r \> 50\] \<- ax\*-1 r\[r \< 0\] \<- 1 value =
list(a1, ax, r) names(value) \<- mCall\[c(“a1”, “ax”, “r”)\]
return(value) }

SS.lrc \<- selfStart(model=lrcModel,initial= lrc.int)

iv \<- getInitial(NEE \~ SS.lrc(‘PAR’, “a1”, “ax”, “r”), data =
day\[which(day$MONTH == 07),\]) iv \#\#\# Create a dataframe to store
month parameter values a1, ax and r (parms.Month):

parms.Month \<- data.frame( MONTH=numeric(), a1=numeric(), ax=numeric(),
r=numeric(), a1.pvalue=numeric(), ax.pvalue=numeric(),
r.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
parms.Month\[1:12, 1\] \<- seq(1,12,1)

### Write a function to fit the model and extract paramter values a1, ax and r (nee.day):

nee.day \<- function(dataframe){ y = nls( NEE \~ (a1 \* PAR \* ax)/(a1
\* PAR + ax) + r, dataframe, start=list(a1= iv\(a1 , ax= iv\)ax, r=
iv$r), na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))

y.df \<- as.data.frame(cbind(t(coef(summary(y)) \[1:3, 1\]),
t(coef(summary(y)) \[1:3, 4\]))) names(y.df) \<-c(“a1”,“ax”, “r”,
“a1.pvalue”, “ax.pvalue”, “r.pvalue”) return (y.df )}

### Write a loop to fit monthly curves and add paramters to a dataframe (parms.Month):

try(for(j in unique(day$MONTH)){

iv \<- getInitial(NEE \~ SS.lrc(‘PAR’, “a1”, “ax”, “r”), data =
day\[which(day$MONTH == j),\])

y3 \<- try(nee.day(day\[which(day$MONTH == j),\]), silent=T)

try(parms.Month\[c(parms.Month$MONTH == j ), 2:7 \] \<- cbind(y3),
silent=T) rm(y3) }, silent=T) parms.Month \#\# Ecosystem Respiration

# Results (at least 1 plot and one table)

library(knitr) kable(head(parms.Month\[1:7,1:12\]),“markdown”,
table.attr = “id="parms.Month\_table"”)

# Discussion (1 paragrapgh)
