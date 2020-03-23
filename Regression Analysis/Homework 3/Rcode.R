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
