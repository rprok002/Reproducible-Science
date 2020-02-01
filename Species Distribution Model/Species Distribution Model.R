dir.create(path = "data")
dir.create(path = "output")
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
SDM=read.csv(file.choose())
bioclim.data <- getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = "data/")
