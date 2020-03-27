library(qpcR)
#Question 4.4
#Choose the file needed
B3=read.csv(file.choose())
#Extract values from certain columns in the dataset
B3y=B3[,1]
B3displace=B3[,2]
B3barrels=B3[,7]
#Create the multiregression model
multreg=lm(B3y~B3displace+B3barrels)
multreg
#Get the unstandardized residuals of the model
residmodel=resid(multreg)
residmodel
#Create a normal probability plot of the unstandardized residuals
qqnorm(residmodel)
#Get predicted values from the multiregression model
predictmultreg=predict(multreg)
predictmultreg
#Plot residuals vs the predicted response
plot(predictmultreg,residmodel, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Create partial regression for displacement
lm.displacement=lm(B3y~B3displace)
lm.displacement
#Get residuals for displacement model
resid1=resid(lm.displacement)
#Create partial regression for barrels as ith
lm.barrelsith=lm(B3barrels~B3displace)
lm.barrelsith
#Get residuals for barrels as ith
residbarrelsith=resid(lm.barrelsith)
#Plot partial regression models
plot(resid1,residbarrelsith,main="Partial Regression Plot for Barrels as ith", xlab= "Residuals for y=x1", ylab= "Residuals for x6=x1")
#Create partial regression for barrels
lm.barrels=lm(B3y~B3barrels)
lm.barrels
#Get residuals for displacement as ith
resid2=resid(lm.barrels)
#Create partial regression for displacement as ith
lm.displaceith=lm(B3displace~B3barrels)
lm.displaceith
#Get residuals for displacement as ith
residdisplaceith=resid(lm.displaceith)
#Plot partial regression models
plot(resid2,residdisplaceith,main="Partial Regression Plot for Displacement as ith", xlab= "Residuals for y=x6", ylab= "Residuals for x1=x6")


#Studentized residuals (standardized)
standr=rstandard(multreg)
standr
#Student residuals (studentized)
studentr=rstudent(multreg)
studentr



#Question 4.14
#Choose the file needed
B5=read.csv(file.choose())
#Extract values from certain columns in the dataset
B5y=B5[,1]
B5solvent=B5[,7]
B5hydrogen=B5[,8]
#Create the multiregression model
multreg2=lm(B5y~B5solvent+B5hydrogen)
multreg2
#Get the unstandardized residuals of the model
residmodel2=resid(multreg2)
residmodel2
#Create a normal probability plot of the unstandardized residuals
qqnorm(residmodel2)
#Get predicted values from the multiregression model
predictmultreg2=predict(multreg2)
predictmultreg2
#Plot residuals vs the predicted response
plot(predictmultreg2,residmodel2, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Create partial regression for solvent
lm.solvent=lm(B5y~B5solvent)
lm.solvent
#Get residuals for solvent
residsolv=resid(lm.solvent)
residsolv
#Create partial regression for hydrogen as ith term
lm.hydrogenith=lm(B5hydrogen~B5solvent)
lm.hydrogenith
#Get residuals for hydrogen as ith term
residhydrogenith=resid(lm.hydrogenith)
#Plot partial regression models
plot(residsolv,residhydrogenith,main="Partial Regression Plot for Hydrogen as ith", xlab= "y=x6", ylab= "x7=x6")
#Create partial regression for solvent as ith
lm.hydrogen=lm(B5y~B5hydrogen)
lm.hydrogen
#Get residuals for hydrogen
residhydr=resid(lm.hydrogen)
#Create partial regression for solvent as ith
lm.solventith=lm(B5solvent~B5hydrogen)
lm.solventith
#Get residuals for solvent as ith
residsolventith=resid(lm.solventith)
#Plot partial regression models
plot(residhydr,residsolventith,main="Partial Regression Plot for Solvent as ith", xlab= "y=x7", ylab= "x6=x7")


#Studentized residuals (standardized)
standrmultreg2=rstandard(multreg2)
standrmultreg2
#Student residuals (studentized)
studentrmultreg2=rstudent(multreg2)
studentrmultreg2


#PRESS Statistic for full model
library(qpcR)
#Getting hat values
PRESS(multreg2)

#PRESS statistic for solvent model
#Getting hat values
PRESS(lm.solvent)

#Question 5.1
#Choose the file needed
Data=read.csv(file.choose())
Temp=Data[,1]
Viscosity=Data[,2]
plot(Temp,Viscosity, main= "Scatter Plot of Viscosity vs Temperature", xlab= "Temperature (degrees C)", ylab= "Viscosity (mPa*s)")

#Fit a straight-line model
lm.TV=lm(Viscosity~Temp)
lm.TV
summary(lm.TV)

#Get the unstandardized residuals of the model
residlm.TV=resid(lm.TV)
residlm.TV

#Get predicted values from the model
predictlm.TV=predict(lm.TV)
predictlm.TV
plot(predictlm.TV,residlm.TV, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Create a normal probability plot of the unstandardized residuals
qqnorm(residlm.TV)

#Fit a transformed model
NewV=1/Viscosity
NewT=1/Temp
new.TV=lm(NewV~NewT)
new.TV
summary(new.TV)

#Get the unstandardized residuals of the model
residnew.TV=resid(new.TV)
residnew.TV

#Get predicted values from the model
predictnew.TV=predict(new.TV)
predictnew.TV
plot(predictnew.TV,residnew.TV, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Create a normal probability plot of the unstandardized residuals
qqnorm(residnew.TV)

#Fit a transformed model
NewV=1/Viscosity
NewT=1/Temp
new2.TV=lm(Viscosity~NewT)
new2.TV
summary(new2.TV)

#Get the unstandardized residuals of the model
residnew2.TV=resid(new2.TV)
residnew2.TV

#Get predicted values from the model
predictnew2.TV=predict(new2.TV)
predictnew2.TV
plot(predictnew2.TV,residnew2.TV, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Create a normal probability plot of the unstandardized residuals
qqnorm(residnew2.TV)

#Fit a transformed model
NewV=1/Viscosity
NewT=1/Temp
NegNewV=-(1/Viscosity)
new3.TV=lm(NegNewV~Temp)
new3.TV
summary(new3.TV)

#Get the unstandardized residuals of the model
residnew3.TV=resid(new3.TV)
residnew3.TV

#Get predicted values from the model
predictnew3.TV=predict(new3.TV)
predictnew3.TV
plot(predictnew3.TV,residnew3.TV, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Create a normal probability plot of the unstandardized residuals
qqnorm(residnew3.TV)

plot(Temp,NegNewV, main= "Scatter Plot of Viscosity vs Temperature", xlab= "Temperature (degrees C)", ylab= "1/(Viscosity (mPa*s))")

V2=Viscosity^2
T2=Temp^2
logV=log(Viscosity)
logT=log(Temp)
expT=exp(Temp)
#Fit a transformed model

new4.TV=exp(Viscosity~Temp)
new4.TV
summary(new4.TV)

#Get the unstandardized residuals of the model
residnew4.TV=resid(new4.TV)
residnew4.TV

#Get predicted values from the model
predictnew4.TV=predict(new4.TV)
predictnew4.TV
plot(predictnew4.TV,residnew4.TV, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Create a normal probability plot of the unstandardized residuals
qqnorm(residnew4.TV)
