dir.create(path = "data")
dir.create(path = "output")
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
SDM=read.csv(file.choose())
summary(SDM)
#To make species distribution modeling more streamlined, 
#it is useful to have an idea of how widely our species is 
#geographically distributed. We are going to find general 
#latitudinal and longitudinal boundaries and store this 
#information for later use:
max.lat <- ceiling(max(SDM$latitude))
min.lat <- floor(min(SDM$latitude))
max.lon <- ceiling(max(SDM$longitude))
min.lon <- floor(min(SDM$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

#Before we do any modeling, it is also a good idea to run a reality check on your occurrence data 
#by plotting the points on a map.