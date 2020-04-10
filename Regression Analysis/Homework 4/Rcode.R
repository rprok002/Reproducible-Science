library(car)
library(qpcR)
#Choose the file needed
Hmwk4=read.csv(file.choose())
Creclear=Hmwk4[,4]
Crecon=Hmwk4[,1]
Age=Hmwk4[,2]
Weight=Hmwk4[,3]

#Scatter plots
plot(Crecon,Creclear, main= "Creatinine Clearance vs. Creatinine Concentration", xlab ="Creatinine Concentration", ylab="Creatinine Clearance ($1000's)")
plot(Age,Creclear, main= "Creatinine Clearance vs. Age", xlab ="Age", ylab="Creatinine Clearance ($1000's)")
plot(Weight,Creclear, main= "Creatinine Clearance vs. Weight", xlab ="Weight", ylab="Creatinine Clearance ($1000's)")

#Matrix plot
pairs(~Crecon+Age+Weight,data=Hmwk4, main = "Correlation Matrix")

#Linear regression
lm.cre=lm(Creclear~Crecon+Age+Weight)
lm.cre

#VIF
vif(lm.cre)

#Get the unstandardized residuals of the model
residmodel=resid(lm.cre)
residmodel

#Create a normal probability plot of the unstandardized residuals
qqnorm(residmodel)

#Get predicted values from the multiregression model
predictmultreg=predict(lm.cre)
predictmultreg

#Plot residuals vs the predicted response
plot(predictmultreg,residmodel, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Create partial regression for age and weight
lm.ageweight=lm(Creclear~Age+Weight)
lm.ageweight
#Get residuals for age weight model
resid1=resid(lm.ageweight)
resid1
#Create partial regression for Crecon as ith
lm.Creconasith=lm(Crecon~Age+Weight)
lm.Creconasith
#Get residuals for Crecon as ith
residCreconasith=resid(lm.Creconasith)
residCreconasith
#Plot partial regression models
plot(resid1,residCreconasith,main="Partial Regression Plot for Creatinine Concentration as ith", xlab= "Residuals for Creclear=Age+Weight", ylab= "Residuals for Crecon=Age+Weight")

#Create partial regression for crecon and weight
lm.creconweight=lm(Creclear~Crecon+Weight)
lm.creconweight
#Get residuals for crecon weight model
resid2=resid(lm.creconweight)
resid2
#Create partial regression for age as ith
lm.ageasith=lm(Age~Crecon+Weight)
lm.ageasith
#Get residuals for Crecon as ith
residageasith=resid(lm.ageasith)
residageasith
#Plot partial regression models
plot(resid2,residageasith,main="Partial Regression Plot for Age as ith", xlab= "Residuals for Creclear=Crecon+Weight", ylab= "Residuals for Age=Crecon+Weight")

#Create partial regression for crecon and age
lm.creconage=lm(Creclear~Crecon+Age)
lm.creconage
#Get residuals for crecon age model
resid3=resid(lm.creconage)
resid3
#Create partial regression for weight as ith
lm.weightasith=lm(Weight~Crecon+Age)
lm.weightasith
#Get residuals for Crecon as ith
residweightasith=resid(lm.weightasith)
residweightasith
#Plot partial regression models
plot(resid3,residweightasith,main="Partial Regression Plot for Weight as ith", xlab= "Residuals for Creclear=Crecon+Age", ylab= "Residuals for Weight=Crecon+Age")

#Theoretical Model
lm.theo=lm(log(Creclear)~(log(Crecon)+(log(140-Age))+(log(Weight))))
lm.theo

#Theoretical scatter plots
lnCrecclear=log(Creclear)
lnCrecclear
lnCrecon=log(Crecon)
lnCrecon
Altage=(140-Age)
Altage
lnWeight=log(Weight)
lnWeight

#Scatter plots
plot(lnCrecon,lnCrecclear, main= "Ln Creatinine Clearance vs. Ln Creatinine Concentration", xlab ="Ln Creatinine Concentration", ylab="Ln Creatinine Clearance ($1000's)")
plot(Altage,lnCrecclear, main= "Ln Creatinine Clearance vs. (140-Age)", xlab ="(140-Age)", ylab="Ln Creatinine Clearance ($1000's)")
plot(lnWeight,lnCrecclear, main= "Ln Creatinine Clearance vs. Ln Weight", xlab ="Ln Weight", ylab="Ln Creatinine Clearance ($1000's)")

#Matrix plot
pairs(~lnCrecon+Altage+lnWeight,data=Hmwk4, main = "Correlation Matrix")
