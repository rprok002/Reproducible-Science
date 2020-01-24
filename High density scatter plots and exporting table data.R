# High Density Scatterplot with Binning
library(hexbin)
x <- day$TA
y <- day$NEE
bin<-hexbin(x, y, xbins=50)
plot(bin, main = "Net Ecosystem Exchange (NEE) 
     versus Photosynthetically Active Radiation (PAR)", xlab = "PAR (nanometers)", ylab = "NEE (per unit ground area)")

write.table(trc, file = "trc.txt", sep = ",", quote = FALSE, row.names = F)
write.table(lrc, file = "lrc.txt", sep = ",", quote = FALSE, row.names = F)
