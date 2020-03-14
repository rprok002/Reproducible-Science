Data = read.csv(file.choose())
head(Data)
# CUMULATIVE number of species per sample
Data$firstAppearance = !duplicated(Data$Species)  #first appearance = TRUE, subsequent appearances = FALSE; this line identifies whether a species listed in a sample is new or previously recorded

CumSppData = aggregate(firstAppearance ~ Sample, data = Data, FUN = sum)  #creates a new data frame with Sample & how many new unique species were found in each sample

CumSppData$CumSpp = cumsum(CumSppData$firstAppearance)
#calculates the cumulative sum of unique species by Sample

CumSppData$firstAppearance = NULL
#removes raw data on true/false first appearance

head(CumSppData)
#view the first six rows to see if your code worked

#calculate CUMULATIVE area per sample

Area <- 2 * CumSppData$Sample 
#multiply the plot number by 2 because 2 m^2 was the area of each sample
#you can change the 2 to whatever area value that you used; for example, you can set the value to 100 for 100 m^2 (10 m x 10 m) quadrats

head(Area)
#check that your code lines worked

# calculate CUMULATIVE number of individuals per sample

SumInd <- aggregate(Data$Individuals, by = list(Sample = Data$Sample), FUN = sum)
#finds the total number of individuals in each Sample

names(SumInd)[2] <-"SumInd"
#renames the column in our data frame
#make sure the quotations are correct, that can mess you up

CumInd <- cumsum(SumInd$SumInd) 

#finds the cumulative number of individuals for each sample

head(CumInd)

#generate a data frame (table)

TotalData<-cbind(CumSppData,CumInd,Area) #create summary table

head(TotalData) #view summary table

library(ggplot2)

SppAreaCurve=ggplot(TotalData, aes(x=Area, y=CumSpp)) +
  geom_point(shape=16)

print(SppAreaCurve)

SppAreaCurve + theme_classic() + #makes it pretty
  labs(x="Cumulative Area Sampled (m^2)", y = "Cumulative Number of Species") + #adds axes titles
  stat_smooth(method = "gam",formula = y ~ s(x, k = 3), size = 1, se = FALSE, colour = "grey") #adds curve

library(ggplot2)

SampEffCurve=ggplot(TotalData, aes(x=CumInd, y=CumSpp)) +
  geom_point(shape=16)

print(SampEffCurve)

SampEffCurve+theme_classic() + #makes it pretty
  labs(x="Cumulative Number of Individuals", y = "Cumulative Number of Species") + #adds axes titles
  stat_smooth(method = "gam",formula = y ~ s(x, k = 3), size = 1, se = FALSE, colour = "grey") #adds curve

# calculate SPECIES RICHNESS for a community
SppRich <- as.numeric(length(unique(Data$Species))) 
#counts the number of unique species (Species Richness)
print(SppRich)

# calculate number of individuals for each species (ni) 
DiversityIndex <- aggregate(Data$Individuals, by = list(Species = Data$Species), FUN = sum) 

#sum of individuals in each Sample 
names(DiversityIndex)[2] <- "ni" 

# calculations 
Ntotal = as.numeric(sum(DiversityIndex$ni)) #total number of individuals 
pi <- DiversityIndex$ni/Ntotal #proportion of total individuals for each species (pi) 
pi2 <- pi^2 #pi^2 
lnp <- log(pi) #ln(p) 
plnp <- pi * lnp #p*ln(p) 

# compile all data & calculations for summary table 
DiversityIndexTotal <- cbind(DiversityIndex, pi, pi2, lnp, plnp) 
options(digits = 2) 
head(DiversityIndexTotal) #view the first six rows of the summary table 

# Simpson's Index (D) = D = 1 / sumpi2 
D <- 1/sum(pi2) 
print(D) 

# Shannon-Wiener Index (H) = -sum[pi*ln(pi)] 
H <- sum(plnp) * -1 
print(H) 




