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
new.data = data.frame(x1,x6, y)
predict(multreg, new.data, interval="confidence")
predict(multreg, new.data, interval="predict")

modx <- lm(y ~ x1 + x6) # Complex model
mod0 <- lm(y ~ 1) # Intercept only model (omitting all three predictors)
anova(mod0, modx) # List the least complex model first

anova(multreg)
anova(mod0, multreg)
