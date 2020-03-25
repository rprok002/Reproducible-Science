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
plot(predictmultreg,residmodel)

#Create partial regression for displacement
lm.displacement=lm(B3y~B3displace)
lm.displacement
#Get residuals for displacement model
resid1=resid(lm.displacement)
#Create partial regression for barrels as y
lm.barrelsasy=lm(B3barrels~B3displace)
lm.barrelsasy
#Get residuals for barrels as y
residbarrelsasy=resid(lm.barrelsasy)
#Plot partial regression models
plot(resid1,residbarrelsasy,main="Partial Regression Plot for Barrels Removed")
#Create partial regression for barrels
lm.barrels=lm(B3y~B3barrels)
lm.barrels
#Get residuals for barrels model
resid2=resid(lm.barrels)
#Create partial regression for displacement as y
lm.displaceasy=lm(B3displace~B3barrels)
lm.displaceasy
#Get residuals for displacement as y
residdisplaceasy=resid(lm.displaceasy)
#Plot partial regression models
plot(resid2,residdisplaceasy,main="Partial Regression Plot for Displacement Removed")


#Studentized residuals (standardized)
standr=rstandard(multreg)
standr
qqnorm(standr,main= "Normal QQ-Plot for Standardized Residuals")
#Student residuals (studentized)
studentr=rstudent(multreg)
studentr
qqnorm(studentr,main= "Normal QQ-Plot for Studentized Residuals")



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
plot(predictmultreg2,residmodel2)

#Create partial regression for solvent
lm.solvent=lm(B5y~B5solvent)
lm.solvent
#Get residuals for solvent
residsolv=resid(lm.solvent)
residsolv
#Create partial regression for hydrogen as ith term
lm.hydrogenith=lm(B5y~B5hydrogen)
lm.hydrogenith
#Get residuals for hydrogen as ith term
residhydrogenith=resid(lm.hydrogenith)
#Plot partial regression models
plot(residsolv,residhydrogenith,main="Partial Regression Plot for Hydrogen as ith")
#Create partial regression for solvent as ith
lm.hydrogen=lm(B5y~B5hydrogen)
lm.hydrogen
#Get residuals for hydrogen
residhydr=resid(lm.hydrogen)
#Create partial regression for solvent as ith
lm.solventith=lm(B5y~B5solvent)
lm.solventith
#Get residuals for solvent as ith
residsolventith=resid(lm.solventith)
#Plot partial regression models
plot(residhydr,residsolventith,main="Partial Regression Plot for Solvent as ith")


#Studentized residuals (standardized)
standrmultreg2=rstandard(multreg2)
standrmultreg2
qqnorm(standrmultreg2,main= "Normal QQ-Plot for Standardized Residuals")
#Student residuals (studentized)
studentrmultreg2=rstudent(multreg2)
studentrmultreg2
qqnorm(studentrmultreg2,main= "Normal QQ-Plot for Studentized Residuals")
studentrlm.solvent=rstudent(lm.solvent)
studentrlm.solvent

#PRESS Statistic for full model
#Getting hat values
hatlm.solvent=hatvalues(multreg2)
hatmultreg2
#Get numerator
Press1=(studentrmultreg2^2)
Press1

#calculate all press statistics
Press2=Press1/(1-hatmultreg2)
Press2

#Add all press stats to get PRESS residual
PRESS=sum(Press2)
PRESS

#PRESS statistic for solvent model
#Getting hat values
hatlm.solvent=hatvalues(lm.solvent)
hatlm.solvent
#Get numerator
Press1solvent=(studentrlm.solvent^2)
Press1solvent

#calculate all press statistics
Press2solvent=Press1solvent/(1-hatlm.solvent)
Press2solvent

#Add all press stats to get PRESS residual
PRESSsolvent=sum(Press2solvent)
PRESSsolvent

#Model comparision with R^2
anova(multreg2)
SSTfull=5506.3
anova(lm.solvent)
SSTsolvent=5009

Rsquarefull=1-(PRESS/SSTfull)
Rsquarefull
Rsquarepartial=1-(PRESSsolvent/SSTsolvent)
Rsquarepartial
#Questions for professor:
#1) We are looking for pattern on a line with a slope of 1, right?
#2) What is the 0.0767 multiplied to the student squared?
