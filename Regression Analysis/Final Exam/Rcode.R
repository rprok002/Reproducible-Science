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