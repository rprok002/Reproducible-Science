#3.5
getwd()#gets working directory
setwd("C:/Users/Rachel Prokopius/Documents/FALCON HD/GitHub/Reproducible-Science")#sets working directory
tableB3=read.csv("C:/Users/Rachel Prokopius/Documents/FALCON HD/GitHub/Reproducible-Science/Regression Analysis/Homework 2/Copy of data-table-B3.csv")#reads file from a diferent directory
tableB3#prints the data
dim(tableB3)#gives dimensions
x1=tableB3[,2]
x6=tableB3[,7]
y=tableB3[,1]
png(file = "scatterplot_matrices.png")
pairs(~x1+x6+y,data=tableB3, main = "Scatterplot Matrix")
dev.off()#the above line saves the plot to the working directory
multreg=lm(y~x1+x6)
multreg
summary(multreg)
cor(tableB3)


anova(multreg)
anova(mod0, multreg)

lm.displacement= lm(y~x1)
lm.displacement
summary(lm.displacement)

new.data= data.frame(x1=275, x6=2)
predict(multreg, new.data, interval="confidence")
predict(multreg, new.data, interval="predict")


#3.16
getwd()#gets working directory
setwd("C:/Users/Rachel Prokopius/Documents/FALCON HD/GitHub/Reproducible-Science")#sets working directory
tableB16=read.csv("C:/Users/Rachel Prokopius/Documents/FALCON HD/GitHub/Reproducible-Science/Regression Analysis/Homework 2/Copy of data-table-B-16.csv")#reads file from a diferent directory
tableB16#prints the data
dim(tableB16)#gives dimensions
ylifeexp=tableB16[,2]
ylifeexpmale=tableB16[,5]
ylifeexpfemale=tableB16[,6]
xpepTV=tableB16[,3]
xpepDr=tableB16[,4]
png(file = "scatterplot_matrices.png")
pairs(~ylifeexp+ylifeexpmale+ylifeexpfemale+xpepTV+xpepDr,data=tableB16, main = "Scatterplot Matrix")
dev.off()#the above line saves the plot to the working directory

multreglifeexp=lm(ylifeexp~xpepDr+xpepTV)
multreglifeexp
summary(multreglifeexp)
anova(multreglifeexp)


multreglifeexpmale=lm(ylifeexpmale~xpepDr+xpepTV)
multreglifeexpmale
summary(multreglifeexpmale)
anova(multreglifeexpmale)

multreglifeexpfemale=lm(ylifeexpfemale~xpepDr+xpepTV)
multreglifeexpfemale
summary(multreglifeexpfemale)
anova(multreglifeexpfemale)

new.data.life= data.frame(xpepDr=-0.000447, xpepTV=-0.0226)
predict(multreglifeexp, new.data.life, interval="confidence")

new.data.lifemale= data.frame(xpepDr=-0.000479, xpepTV=-0.0257)
predict(multreglifeexpmale, new.data.lifemale, interval="confidence")

new.data.lifefemale= data.frame(xpepDr=-0.000409, xpepTV=-0.0199)
predict(multreglifeexpfemale, new.data.lifefemale, interval="confidence")

