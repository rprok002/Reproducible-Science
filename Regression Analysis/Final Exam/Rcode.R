library(car)
library(qpcR)
#Add Data
Data=read.csv(file.choose())

#Subset
y=Data[,1]
x1=Data[,2]
x2=Data[,3]
x3=Data[,4]
x4=Data[,5]
x5=Data[,6]
x6=Data[,7]
x7=Data[,8]
x8=Data[,9]
x9=Data[,10]

#Question 1: Box Plots
boxplot(y, main = "Pounds of Steam used Monthly", xlab = "y", ylab = "Pounds")
boxplot(x1, main = "Pounds of Real Fatty Acid in Storage per Month", xlab= "x1", ylab = "Pounds per Month")
boxplot(x2, main = "Pounds of Crude Glycerin", xlab = "x2", ylab = "Pounds")
boxplot(x3, main = "Average Wind Velocity", xlab = "x3", ylab = "Avergae Wind Velocity (mph)")
boxplot(x4, main = "Calender Days per Month", xlab = "x4", ylab = "Days per Month")
boxplot(x5, main = "Operating Days per Month", xlab = "x5", ylab= "Days per Month")
boxplot(x6, main = "Days below 32F", xlab = "x6", ylab = "Days")
boxplot(x7, main = "Avg Atmospheric Temperature", xlab = "x7", ylab = "Avg Atmospheric Temp (F)")
boxplot(x8, main = "Avg Wind Velocity squared", xlab = "x8", ylab = "Avg Wind Velocity Squared (mph2)")
boxplot(x9, main = "Number of Starups", xlab = "x9", ylab = "Number")

#Question 2: Scatter Plots
plot(x1, y, main = "Pounds of Steam used Monthly vs Pounds Real Fatty Acid", xlab= "Real Fatty Acid in Storage (lbs/month)", ylab = "Steam (lbs/month)")
plot(x2, y, main = "Pounds of Steam used Monthly vs Crude Glycerin", xlab= "Crude Glycerin (lbs)", ylab = "Steam (lbs/month)")
plot(x3, y, main = "Pounds of Steam used Monthly vs Average Wind Velocity", xlab= "Average Wind Velocity (mph)", ylab = "Steam (lbs/month)")
plot(x4, y, main = "Pounds of Steam used Monthly vs Calender Days", xlab= "Calender (days/month)", ylab = "Steam (lbs/month)")
plot(x5, y, main = "Pounds of Steam used Monthly vs Operating Days per Month", xlab= "Operating Days (days/month)", ylab = "Steam (lbs/month)")
plot(x6, y, main = "Pounds of Steam used Monthly vs Days below 32F", xlab= "Days below 32F (days)", ylab = "Steam (lbs/month)")
plot(x7, y, main = "Pounds of Steam used Monthly vs Average Atmospheric Temp", xlab= "Average Atmospheric Temp (F)", ylab = "Steam (lbs/month)")
plot(x8, y, main = "Pounds of Steam used Monthly vs Average Wind Velocity Squared", xlab= "Average Wind Velocity Squared (mph2)", ylab = "Steam (lbs/month)")
plot(x9, y, main = "Pounds of Steam used Monthly vs Number of Startups", xlab= "Number of Startups (#)", ylab = "Steam (lbs/month)")

#Question 3: Linear Model
lm.full=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9)
lm.full
summary(lm.full)

residfull=resid(lm.full)
residfull

predictlmfull=predict(lm.full)
predictlmfull
plot(predictlmfull,residfull, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residfull)

standfull=rstandard(lm.full)
standfull
studentfull=rstudent(lm.full)
studentfull
vif(lm.full)
PRESS(lm.full)

#Partial x1 ith
lm.x1=lm(y~x2+x3+x4+x5+x6+x7+x8+x9)
lm.x1
lm.x1ith=lm(x1~x2+x3+x4+x5+x6+x7+x8+x9)
lm.x1ith
resid.x1=resid(lm.x1)
resid.x1
resid.x1ith=resid(lm.x1ith)
resid.x1ith
plot(resid.x1,resid.x1ith,main="Partial Regression Plot for x1 as ith", xlab= "Residuals for y=x2+x3+x4+x5+x6+x7+x8+x9", ylab= "Residuals for x1=x2+x3+x4+x5+x6+x7+x8+x9")
qqnorm(resid.x1)

summary(lm.x1)
predictlm.x1=predict(lm.x1)
predictlm.x1
plot(predictlm.x1,resid.x1, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
standx1=rstandard(lm.x1)
standx1
studentx1=rstudent(lm.x1)
studentx1
vif(lm.x1)
PRESS(lm.x1)

#Partial x2 ith
lm.x2=lm(y~x1+x3+x4+x5+x6+x7+x8+x9)
lm.x2
lm.x2ith=lm(x2~x1+x3+x4+x5+x6+x7+x8+x9)
lm.x2ith
resid.x2=resid(lm.x2)
resid.x2
resid.x2ith=resid(lm.x2ith)
resid.x2ith
plot(resid.x2,resid.x2ith,main="Partial Regression Plot for x2 as ith", xlab= "Residuals for y=x1+x3+x4+x5+x6+x7+x8+x9", ylab= "Residuals for x2=x1+x3+x4+x5+x6+x7+x8+x9")
qqnorm(resid.x2)

summary(lm.x2)
predictlm.x2=predict(lm.x2)
predictlm.x2
plot(predictlm.x2,resid.x2, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
standx2=rstandard(lm.x2)
standx2
studentx2=rstudent(lm.x2)
studentx2
vif(lm.x2)
PRESS(lm.x2)

#Partial x3 ith
lm.x3=lm(y~x1+x2+x4+x5+x6+x7+x8+x9)
lm.x3
lm.x3ith=lm(x3~x1+x2+x4+x5+x6+x7+x8+x9)
lm.x3ith
resid.x3=resid(lm.x3)
resid.x3
resid.x3ith=resid(lm.x3ith)
resid.x3
plot(resid.x3,resid.x3,main="Partial Regression Plot for x3 as ith", xlab= "Residuals for y=x1+x2+x4+x5+x6+x7+x8+x9", ylab= "Residuals for x3=x1+x2+x4+x5+x6+x7+x8+x9")
qqnorm(resid.x3)

summary(lm.x3)
predictlm.x3=predict(lm.x3)
predictlm.x3
plot(predictlm.x3,resid.x3, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
standx3=rstandard(lm.x3)
standx3
studentx3=rstudent(lm.x3)
studentx3
vif(lm.x3)
PRESS(lm.x3)

#Partial x4 ith
lm.x4=lm(y~x1+x2+x3+x5+x6+x7+x8+x9)
lm.x4
lm.x4ith=lm(x4~x1+x2+x3+x5+x6+x7+x8+x9)
lm.x4ith
resid.x4=resid(lm.x4)
resid.x4
resid.x4ith=resid(lm.x4ith)
resid.x4ith
plot(resid.x4,resid.x4ith,main="Partial Regression Plot for x4 as ith", xlab= "Residuals for y=x1+x2+x3+x5+x6+x7+x8+x9", ylab= "Residuals for x4=x1+x2+x3+x5+x6+x7+x8+x9")
qqnorm(resid.x4)

summary(lm.x4)
predictlm.x4=predict(lm.x4)
predictlm.x4
plot(predictlm.x4,resid.x4, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
standx4=rstandard(lm.x4)
standx4
studentx4=rstudent(lm.x4)
studentx4
vif(lm.x4)
PRESS(lm.x4)

#Partial x5 ith
lm.x4=lm(y~x1+x2+x3+x5+x6+x7+x8+x9)
lm.x4
lm.x4ith=lm(x4~x1+x2+x3+x5+x6+x7+x8+x9)
lm.x4ith
resid.x4=resid(lm.x4)
resid.x4
resid.x4ith=resid(lm.x4ith)
resid.x4ith
plot(resid.x4,resid.x4ith,main="Partial Regression Plot for x4 as ith", xlab= "Residuals for y=x1+x2+x3+x5+x6+x7+x8+x9", ylab= "Residuals for x4=x1+x2+x3+x5+x6+x7+x8+x9")
qqnorm(resid.x4)

summary(lm.x4)
predictlm.x4=predict(lm.x4)
predictlm.x4
plot(predictlm.x4,resid.x4, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
standx4=rstandard(lm.x4)
standx4
studentx4=rstudent(lm.x4)
studentx4
vif(lm.x4)
PRESS(lm.x4)