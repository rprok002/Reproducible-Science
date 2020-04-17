Tri=read.csv(file.choose())
#Subsetting
newTri <- na.omit(Tri)
length=newTri[,7]
condition=newTri[,3]
condition

Tri.fresh <- subset(newTri, condition == 3)
TrifreshSA=Tri.fresh[,5]
Trifreshlength=Tri.fresh[,7]
Trifreshmass=Tri.fresh[,8]
Trifreshfamily=Tri.fresh[,11]

Tri.fresh.Hydro <- subset(newTri, condition ==3 & Family == "Hydropsychidae")
Tri.fresh.Phry <- subset(newTri, condition ==3 & Family == "Phryganeidae")
Tri.fresh.Poly <- subset(newTri, condition ==3 & Family == "Polycentropodidae")
Tri.fresh.Rhya <- subset(newTri, condition ==3 & Family == "Rhyacophilidae")
HydrofreshSA=Tri.fresh.Hydro[,5]
Hydrofreshlength=Tri.fresh.Hydro[,7]
Hydrofreshmass=Tri.fresh.Hydro[,8]
Hydrofreshfamily=Tri.fresh.Hydro[,11]
PhryfreshSA=Tri.fresh.Phry[,5]
Phryfreshlength=Tri.fresh.Phry[,7]
Phryfreshmass=Tri.fresh.Phry[,8]
Phryfreshfamily=Tri.fresh.Phry[,11]
PolyfreshSA=Tri.fresh.Poly[,5]
Polyfreshlength=Tri.fresh.Poly[,7]
Polyfreshmass=Tri.fresh.Poly[,8]
Polyfreshfamily=Tri.fresh.Poly[,11]
RhyafreshSA=Tri.fresh.Rhya[,5]
Rhyafreshlength=Tri.fresh.Rhya[,7]
Rhyafreshmass=Tri.fresh.Rhya[,8]
Rhyafreshfamily=Tri.fresh.Rhya[,11]

Tri.alcohol <- subset(newTri, condition == 2 & length < 100)
TrialcoholSA=Tri.alcohol[,5]
Trialcohollength=Tri.alcohol[,7]
Trialcoholmass=Tri.alcohol[,8]

Tri.alcohol.Hydro <- subset(newTri, condition ==2 & Family == "Hydropsychidae")
Tri.alcohol.Phry <- subset(newTri, condition ==2 & Family == "Phryganeidae")
Tri.alcohol.Poly <- subset(newTri, condition ==2 & Family == "Polycentropodidae")
Tri.alcohol.Rhya <- subset(newTri, condition ==2 & Family == "Rhyacophilidae")
Tri.alcohol.Bra <- subset(newTri, condition ==2 & Family == "Bracgycentridae" & length < 100)
HydroalcoholSA=Tri.alcohol.Hydro[,5]
Hydroalcohollength=Tri.alcohol.Hydro[,7]
Hydroalcoholmass=Tri.alcohol.Hydro[,8]
Hydroalcoholfamily=Tri.alcohol.Hydro[,11]
PhryalcoholSA=Tri.alcohol.Phry[,5]
Phryalcohollength=Tri.alcohol.Phry[,7]
Phryalcoholmass=Tri.alcohol.Phry[,8]
Phryalcoholfamily=Tri.alcohol.Phry[,11]
PolyalcoholSA=Tri.alcohol.Poly[,5]
Polyalcohollength=Tri.alcohol.Poly[,7]
Polyalcoholmass=Tri.alcohol.Poly[,8]
Polyalcoholfamily=Tri.alcohol.Poly[,11]
RhyaalcoholSA=Tri.alcohol.Rhya[,5]
Rhyaalcohollength=Tri.alcohol.Rhya[,7]
Rhyaalcoholmass=Tri.alcohol.Rhya[,8]
Rhyaalcoholfamily=Tri.alcohol.Rhya[,11]
BraalcoholSA=Tri.alcohol.Bra[,5]
Braalcohollength=Tri.alcohol.Bra[,7]
Braalcoholmass=Tri.alcohol.Bra[,8]
Braalcoholfamily=Tri.alcohol.Bra[,11]

Tri.ice <- subset(newTri, condition == 1)
TriiceSA=Tri.ice[,5]
Triicelength=Tri.ice[,7]
Triicemass=Tri.ice[,8]

Tri.ice.Hydro <- subset(newTri, condition ==1 & Family == "Hydropsychidae")
Tri.ice.Poly <- subset(newTri, condition ==1 & Family == "Polycentropodidae")
Tri.ice.Rhya <- subset(newTri, condition ==1 & Family == "Rhyacophilidae")
HydroiceSA=Tri.ice.Hydro[,5]
Hydroicelength=Tri.ice.Hydro[,7]
Hydroicemass=Tri.ice.Hydro[,8]
Hydroicefamily=Tri.ice.Hydro[,11]
PolyiceSA=Tri.ice.Poly[,5]
Polyicelength=Tri.ice.Poly[,7]
Polyicemass=Tri.ice.Poly[,8]
Polyicefamily=Tri.ice.Poly[,11]
RhyaiceSA=Tri.ice.Rhya[,5]
Rhyaicelength=Tri.ice.Rhya[,7]
Rhyaicemass=Tri.ice.Rhya[,8]
Rhyaicefamily=Tri.ice.Rhya[,11]

#Linear model fresh Trichoptera

lm.Trifresh=lm(Trifreshmass~TrifreshSA)
lm.Trifresh
anova(lm.Trifresh)
summary(lm.Trifresh)
plot(TrifreshSA, Trifreshmass, main= "Trichoptera Fresh Mass vs. Surface Area", xlab="Trichoptera Fresh Surface Area (sq mm)", ylab="Trichoptera Fresh Mass (mg)") 
lines(Tri.fresh$SA.after..mm2., fitted(lm.Trifresh), col="blue")
residTrifresh=resid(lm.Trifresh)
residTrifresh
predictTrifresh=predict(lm.Trifresh)
predictTrifresh
plot(predictTrifresh,residTrifresh, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

lm.Trifreshlength=lm(Trifreshmass~Trifreshlength)
lm.Trifreshlength
anova(lm.Trifreshlength)
summary(lm.Trifreshlength)
plot(Trifreshlength, Trifreshmass, main= "Trichoptera Fresh Mass vs. Length", xlab="Trichoptera Fresh Length (mm)", ylab="Trichoptera Fresh Mass (mg)")
lines(Tri.fresh$length.after..mm., fitted(lm.Trifreshlength), col="blue")
residTrifreshlength=resid(lm.Trifreshlength)
residTrifreshlength
predictTrifreshlength=predict(lm.Trifreshlength)
predictTrifreshlength
plot(predictTrifreshlength,residTrifreshlength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Linear model Hydro
lm.Hydrofresh=lm(Hydrofreshmass~HydrofreshSA)
lm.Hydrofresh
anova(lm.Hydrofresh)
summary(lm.Hydrofresh)
plot(HydrofreshSA, Hydrofreshmass, main= "Hydropsychidae Fresh Mass vs. Surface Area", xlab="Hydropsychidae Fresh Surface Area (sq mm)", ylab="Hydropsychidae Fresh Mass (mg)") 
lines(Tri.fresh.Hydro$SA.after..mm2., fitted(lm.Hydrofresh), col="blue")
residHydrofresh=resid(lm.Hydrofresh)
residHydrofresh
predictHydrofresh=predict(lm.Hydrofresh)
predictHydrofresh
plot(predictHydrofresh,residHydrofresh, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

lm.Hydrofreshlength=lm(Hydrofreshmass~Hydrofreshlength)
lm.Hydrofreshlength
anova(lm.Hydrofreshlength)
summary(lm.Hydrofreshlength)
plot(Hydrofreshlength, Hydrofreshmass, main= "Hydropsychidae Fresh Mass vs. Length", xlab="Hydropsychidae Fresh Length (mm)", ylab="Hydropsychidae Fresh Mass (mg)") 
lines(Tri.fresh.Hydro$length.after..mm., fitted(lm.Hydrofreshlength), col="blue")
residHydrofreshlength=resid(lm.Hydrofreshlength)
residHydrofreshlength
predictHydrofreshlength=predict(lm.Hydrofreshlength)
predictHydrofreshlength
plot(predictHydrofreshlength,residHydrofreshlength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Linear model alcohol Trichoptera
lm.Trialcohol=lm(Trialcoholmass~TrialcoholSA)
lm.Trialcohol
anova(lm.Trialcohol)
summary(lm.Trialcohol)
plot(TrialcoholSA, Trialcoholmass, main= "Trichoptera Alcohol Mass vs. Surface Area", xlab="Trichoptera Alcohol Surface Area (sq mm)", ylab="Trichoptera Alcohol Mass (mg)")
lines(Tri.alcohol$SA.after..mm2., fitted(lm.Trialcohol), col="red")
residTrialcohol=resid(lm.Trialcohol)
residTrialcohol
predictTrialcohol=predict(lm.Trialcohol)
predictTrialcohol
plot(predictTrialcohol,residTrialcohol, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

lm.Trialcohollength=lm(Trialcoholmass~Trialcohollength)
lm.Trialcohollength
anova(lm.Trialcohollength)
summary(lm.Trialcohollength)
plot(Trialcohollength, Trialcoholmass,main= "Trichoptera Alcohol Mass vs. Length", xlab="Trichoptera Alcohol Length (mm)", ylab="Trichoptera Alcohol Mass (mg)")
lines(Tri.alcohol$length.after..mm., fitted(lm.Trialcohollength), col="red")
residTrialcohollength=resid(lm.Trialcohollength)
residTrialcohollength
predictTrialcohollength=predict(lm.Trialcohollength)
predictTrialcohollength
plot(predictTrialcohollength,residTrialcohollength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Linear model Hydro
lm.Hydroalcohol=lm(Hydroalcoholmass~HydroalcoholSA)
lm.Hydroalcohol
anova(lm.Hydroalcohol)
summary(lm.Hydroalcohol)
plot(HydroalcoholSA, Hydroalcoholmass, main= "Hydropsychidae Alcohol Mass vs. Surface Area", xlab="Hydropsychidae Alcohol Surface Area (sq mm)", ylab="Hydropsychidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Hydro$SA.after..mm2., fitted(lm.Hydroalcohol), col="red")
residHydroalcohol=resid(lm.Hydroalcohol)
residHydroalcohol
predictHydroalcohol=predict(lm.Hydroalcohol)
predictHydroalcohol
plot(predictHydroalcohol,residHydroalcohol, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

lm.Hydroalcohollength=lm(Hydroalcoholmass~Hydroalcohollength)
lm.Hydroalcohollength
anova(lm.Hydroalcohollength)
summary(lm.Hydroalcohollength)
plot(Hydroalcohollength, Hydroalcoholmass, main= "Hydropsychidae Alcohol Mass vs. Length", xlab="Hydropsychidae Alcohol Length (mm)", ylab="Hydropsychidae Alcohol Mass (mg)") 
lines(Tri.alcohol.Hydro$length.after..mm., fitted(lm.Hydroalcohollength), col="red")
residHydroalcohollength=resid(lm.Hydroalcohollength)
residHydroalcohollength
predictHydroalcohollength=predict(lm.Hydroalcohollength)
predictHydroalcohollength
plot(predictHydroalcohollength,residHydroalcohollength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")



#Linear model ice Trichoptera
lm.Triice=lm(Triicemass~TriiceSA)
lm.Triice
anova(lm.Triice)
summary(lm.Triice)
plot(TriiceSA, Triicemass, main= "Trichoptera Ice Mass vs. Surface Area", xlab="Trichoptera Ice Surface Area (sq mm)", ylab="Trichoptera Ice Mass (mg)")
lines(Tri.ice$SA.after..mm2., fitted(lm.Triice), col="green")
residTriice=resid(lm.Triice)
residTriice
predictTriice=predict(lm.Triice)
predictTriice
plot(predictTriice,residTriice, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")


lm.Triicelength=lm(Triicemass~Triicelength)
lm.Triicelength
anova(lm.Triicelength)
summary(lm.Triicelength)
plot(Triicelength, Triicemass, main= "Trichoptera Ice Mass vs. Length", xlab="Trichoptera Ice Length (mm)", ylab="Trichoptera Ice Mass (mg)")
lines(Tri.ice$length.after..mm., fitted(lm.Triicelength), col="green")
residTriicelength=resid(lm.Triicelength)
residTriicelength
predictTriicelength=predict(lm.Triicelength)
predictTriicelength
plot(predictTriicelength,residTriicelength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Linear model ice Hydro
lm.Hydroice=lm(Hydroicemass~HydroiceSA)
lm.Hydroice
anova(lm.Hydroice)
summary(lm.Hydroice)
plot(HydroiceSA, Hydroicemass, main= "Hydropsychidae Ice Mass vs. Surface Area", xlab="Hydropsychidae Ice Surface Area (sq mm)", ylab="Hydropsychidae Ice Mass (mg)")
lines(Tri.ice.Hydro$SA.after..mm2., fitted(lm.Hydroice), col="green")
residHydroice=resid(lm.Hydroice)
residHydroice
predictHydroice=predict(lm.Hydroice)
predictHydroice
plot(predictHydroice,residHydroice, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")


lm.Hydroicelength=lm(Hydroicemass~Hydroicelength)
lm.Hydroicelength
anova(lm.Hydroicelength)
summary(lm.Hydroicelength)
plot(Hydroicelength, Hydroicemass, main= "Hydropsychidae Ice Mass vs. Length", xlab="Hydropsychidae Ice Length (mm)", ylab="Hydropsychidae Ice Mass (mg)")
lines(Tri.ice.Hydro$length.after..mm., fitted(lm.Hydroicelength), col="green")
residHydroicelength=resid(lm.Hydroicelength)
residHydroicelength
predictresidHydroicelength=predict(lm.Hydroicelength)
predictresidHydroicelength
plot(predictresidHydroicelength,residHydroicelength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")

#Transform
lnTrifreshSA=log(TrifreshSA)
lnTrifreshlength=log(Trifreshlength)
lnTrifreshmass=log(Trifreshmass)
lnTrialcoholSA=log(TrialcoholSA)
lnTrialcohollength=log(Trialcohollength)
lnTrialcoholmass=log(Trialcoholmass)
lnTriiceSA=log(TriiceSA)
lnTriicelength=log(Triicelength)
lnTriicemass=log(Triicemass)
lnHydrofreshSA=log(HydrofreshSA)
lnHydrofreshlength=log(Hydrofreshlength)
lnHydrofreshmass=log(Hydrofreshmass)
lnHydroalcoholSA=log(HydroalcoholSA)
lnHydroalcoholmass=log(Hydroalcoholmass)
lnHydroalcohollength=log(Hydroalcohollength)
lnHydroiceSA=log(HydroiceSA)
lnHydroicelength=log(Hydroicelength)
lnHydroicemass=log(Hydroicemass)

#ln model fresh Trichoptera

lnTrifresh=lm(lnTrifreshmass~lnTrifreshSA)
lnTrifresh
anova(lnTrifresh)
summary(lnTrifresh)
plot(lnTrifreshSA, lnTrifreshmass, main= "Ln Trichoptera Fresh Mass vs. Ln Surface Area", xlab="Ln Trichoptera Fresh Surface Area (sq mm)", ylab="Ln Trichoptera Fresh Mass (mg)") 
lines(lnTrifreshSA, fitted(lnTrifresh), col="blue")
residlnTrifresh=resid(lnTrifresh)
residlnTrifresh
predictlnTrifresh=predict(lnTrifresh)
predictlnTrifresh
plot(predictlnTrifresh,residlnTrifresh, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residlnTrifresh)

lnTrifreshlengthmodel=lm(lnTrifreshmass~lnTrifreshlength)
lnTrifreshlengthmodel
anova(lnTrifreshlengthmodel)
summary(lnTrifreshlengthmodel)
plot(lnTrifreshlength, lnTrifreshmass, main= "Ln Trichoptera Fresh Mass vs. Ln Length", xlab="Ln Trichoptera Fresh length (mm)", ylab="Ln Trichoptera Fresh Mass (mg)") 
lines(lnTrifreshlength, fitted(lnTrifreshlengthmodel), col="blue")
residlnTrifreshlength=resid(lnTrifreshlengthmodel)
residlnTrifreshlength
predictlnTrifreshlength=predict(lnTrifreshlengthmodel)
predictlnTrifreshlength
plot(predictlnTrifreshlength,residlnTrifreshlength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residlnTrifreshlength)

#Ln model Hydro
lnHydrofresh=lm(lnHydrofreshmass~lnHydrofreshSA)
lnHydrofresh
anova(lnHydrofresh)
summary(lnHydrofresh)
plot(lnHydrofreshSA, lnHydrofreshmass, main= "Ln Hydropsychidae Fresh Mass vs. Ln Surface Area", xlab="Ln Hydropsychidae Fresh Surface Area (sq mm)", ylab="Ln Hydropsychidae Fresh Mass (mg)") 
lines(lnHydrofreshSA, fitted(lnHydrofresh), col="blue")
residlnHydrofresh=resid(lnHydrofresh)
residlnHydrofresh
predictlnHydrofresh=predict(lnHydrofresh)
predictlnHydrofresh
plot(predictlnHydrofresh,residlnHydrofresh, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residlnHydrofresh)

lnHydrofreshlengthmodel=lm(lnHydrofreshmass~lnHydrofreshlength)
lnHydrofreshlengthmodel
anova(lnHydrofreshlengthmodel)
summary(lnHydrofreshlengthmodel)
plot(lnHydrofreshlength, lnHydrofreshmass, main= "Ln Hydropsychidae Fresh Mass vs. Length", xlab="Ln Hydropsychidae Fresh Length (mm)", ylab="Ln Hydropsychidae Fresh Mass (mg)") 
lines(lnHydrofreshlength, fitted(lnHydrofreshlengthmodel), col="blue")
residlnHydrofreshlength=resid(lnHydrofreshlengthmodel)
residlnHydrofreshlength
predictlnHydrofreshlength=predict(lnHydrofreshlengthmodel)
predictlnHydrofreshlength
plot(predictlnHydrofreshlength,residlnHydrofreshlength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residlnHydrofreshlength)

#ln model alcohol Trichoptera

lnTrialcohol=lm(lnTrialcoholmass~lnTrialcoholSA)
lnTrialcohol
anova(lnTrialcohol)
summary(lnTrialcohol)
plot(lnTrialcoholSA, lnTrialcoholmass, main= "Ln Trichoptera Alcohol Mass vs. Ln Surface Area", xlab="Ln Trichoptera Alcohol Surface Area (sq mm)", ylab="Ln Trichoptera Alcohol Mass (mg)") 
lines(lnTrialcoholSA, fitted(lnTrialcohol), col="red")
residlnTrialcohol=resid(lnTrialcohol)
residlnTrialcohol
predictlnTrialcohol=predict(lnTrialcohol)
predictlnTrialcohol
plot(predictlnTrialcohol,residlnTrialcohol, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residlnTrialcohol)

lnTrialcohollengthmodel=lm(lnTrialcoholmass~lnTrialcohollength)
lnTrialcohollengthmodel
anova(lnTrialcohollengthmodel)
summary(lnTrialcohollengthmodel)
plot(lnTrialcohollength, lnTrialcoholmass, main= "Ln Trichoptera Alcohol Mass vs. Ln Length", xlab="Ln Trichoptera Alcohol length (mm)", ylab="Ln Trichoptera Alcohol Mass (mg)") 
lines(lnTrialcohollength, fitted(lnTrialcohollengthmodel), col="red")
residlnTrialcohollength=resid(lnTrialcohollengthmodel)
residlnTrialcohollength
predictlnTrialcohollength=predict(lnTrialcohollengthmodel)
predictlnTrialcohollength
plot(predictlnTrialcohollength,residlnTrialcohollength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residlnTrialcohollength)

#Ln model Hydro
lnHydroalcohol=lm(lnHydroalcoholmass~lnHydroalcoholSA)
lnHydroalcohol
anova(lnHydroalcohol)
summary(lnHydroalcohol)
plot(lnHydroalcoholSA, lnHydroalcoholmass, main= "Ln Hydropsychidae Alcohol Mass vs. Ln Surface Area", xlab="Ln Hydropsychidae Alcohol Surface Area (sq mm)", ylab="Ln Hydropsychidae Alcohol Mass (mg)") 
lines(lnHydroalcoholSA, fitted(lnHydroalcohol), col="red")
residlnHydroalcohol=resid(lnHydroalcohol)
residlnHydroalcohol
predictlnHydroalcohol=predict(lnHydroalcohol)
predictlnHydroalcohol
plot(predictlnHydroalcohol,residlnHydroalcohol, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residlnHydroalcohol)

lnHydroalcohollengthmodel=lm(lnHydroalcoholmass~lnHydroalcohollength)
lnHydroalcohollengthmodel
anova(lnHydroalcohollengthmodel)
summary(lnHydroalcohollengthmodel)
plot(lnHydroalcohollength, lnHydroalcoholmass, main= "Ln Hydropsychidae Alcohol Mass vs. Length", xlab="Ln Hydropsychidae Alcohol Length (mm)", ylab="Ln Hydropsychidae Alcohol Mass (mg)") 
lines(lnHydroalcohollength, fitted(lnHydroalcohollengthmodel), col="red")
residlnHydroalcohollength=resid(lnHydroalcohollengthmodel)
residlnHydroalcohollength
predictlnHydroalcohollength=predict(lnHydroalcohollengthmodel)
predictlnHydroalcohollength
plot(predictlnHydroalcohollength,residlnHydroalcohollength, main= "Residuals vs. Predicted Response", xlab ="Predicted Response", ylab="Residuals")
qqnorm(residlnHydroalcohollength)
