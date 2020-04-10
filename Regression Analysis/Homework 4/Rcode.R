library(car)
library(qpcR)
#Choose the file needed
Hmwk4=read.csv(file.choose())
Creclear=Hmwk4[,4]
Crecon=Hmwk4[,1]
Age=Hmwk4[,2]
Weight=Hmwk4[,3]

#Plot residuals vs the predicted response
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
