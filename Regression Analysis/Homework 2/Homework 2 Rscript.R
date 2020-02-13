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
