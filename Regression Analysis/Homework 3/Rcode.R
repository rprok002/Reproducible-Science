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


