#Uca Na regression information

#Obtaining Na slope from animals 1 through 8 for Na

#1
xyplot(Na~time, data = subset(uca_efflux, Animal == 1 ))
Na.1.lm = lm(Na~time, data = subset(uca_efflux, Animal == 1 ))
anova(Na.1.lm)
summary(Na.1.lm)
coef(Na.1.lm)

#2
xyplot(Na~time, data = subset(uca_efflux, Animal == 2 ))
Na.2.lm = lm(Na~time, data = subset(uca_efflux, Animal == 2 ))
anova(Na.2.lm)
summary(Na.2.lm)
coef(Na.2.lm)

#3
xyplot(Na~time, data = subset(uca_efflux, Animal == 3 & time !=5 ))
Na.3.lm = lm(Na~time, data = subset(uca_efflux, Animal == 3 & time !=5))
anova(Na.3.lm)
summary(Na.3.lm)
coef(Na.3.lm)

#4
xyplot(Na~time, data = subset(uca_efflux, Animal == 4 ))
Na.4.lm = lm(Na~time, data = subset(uca_efflux, Animal == 4 ))
anova(Na.4.lm)
summary(Na.4.lm)
coef(Na.4.lm)

#5
xyplot(Na~time, data = subset(uca_efflux, Animal == 5 ))
Na.5.lm = lm(Na~time, data = subset(uca_efflux, Animal == 5 ))
anova(Na.5.lm)
summary(Na.5.lm)
coef(Na.5.lm)

#6
xyplot(Na~time, data = subset(uca_efflux, Animal == 6 ))
Na.6.lm = lm(Na~time, data = subset(uca_efflux, Animal == 6 ))
anova(Na.6.lm)
summary(Na.6.lm)
coef(Na.6.lm)

#7
xyplot(Na~time, data = subset(uca_efflux, Animal == 7 ))
Na.7.lm = lm(Na~time, data = subset(uca_efflux, Animal == 7 ))
anova(Na.7.lm)
summary(Na.7.lm)
coef(Na.7.lm)

#8
xyplot(Na~time, data = subset(uca_efflux, Animal == 8 ))
Na.8.lm = lm(Na~time, data = subset(uca_efflux, Animal == 8 ))
anova(Na.8.lm)
summary(Na.8.lm)
coef(Na.8.lm)
