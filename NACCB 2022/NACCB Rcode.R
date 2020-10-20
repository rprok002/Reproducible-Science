NACCBlocal = read.csv(file.choose())
city = NACCBlocal[,1]
stateprov = NACCBlocal[,2]
zip = NACCBlocal[,4]
library(devtools)
install_github("hrbrmstr/albersusa")
force = TRUE
library(zipcodeR)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
library(albersusa)
data("zip_code_db")
zip_code_db
NACCB.zip = aggregate(data.frame(count = zip),list(zipcode=NACCBlocal[,4],stateprov=NACCBlocal[,2]),length)
NACCB.stateprov = aggregate(data.frame(count = stateprov),list(zipcode=NACCBlocal[,4],stateprov=NACCBlocal[,2]),length)
NACCB.zip.data = merge(NACCB.zip,zip_code_db)
us = map_data('state')
us
ggplot(NACCB.zip.data,aes(lng,lat)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = count),size=.15,alpha=.25) +
  xlim(-125,-65)+ylim(20,50)
nchar(NACCBlocal$X)
