NACCBlocal = read.csv(file.choose())
city = NACCBlocal[,1]
stateprov = NACCBlocal[,2]
zip = NACCBlocal[,3]
library(devtools)
install_github("hrbrmstr/albersusa")
library(zipcodeR)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
library(albersusa)
data("zip_code_db")
zip_code_db
NACCB.zip = aggregate(data.frame(count = zip),list(zip,stateprov),length)
NACCB.zip.data = merge(NACCB.zip,zip_code_db)
NACCB.zip.data
?ggplot
