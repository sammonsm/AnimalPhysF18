#Uca NH4 regression information

#Obtaining NH4 slope from animals 1 through 8 for NH4

#1
xyplot(NH4~time, data = subset(uca_efflux, Animal == 1 ))
NH4.1.lm = lm(NH4~time, data = subset(uca_efflux, Animal == 1 ))
anova(NH4.1.lm)
summary(NH4.1.lm)
coef(NH4.1.lm)

#2
xyplot(NH4~time, data = subset(uca_efflux, Animal == 2 ))
NH4.2.lm = lm(NH4~time, data = subset(uca_efflux, Animal == 2 ))
anova(NH4.2.lm)
summary(NH4.2.lm)
coef(NH4.2.lm)

#3
xyplot(NH4~time, data = subset(uca_efflux, Animal == 3))
NH4.3.lm = lm(NH4~time, data = subset(uca_efflux, Animal == 3))
anova(NH4.3.lm)
summary(NH4.3.lm)
coef(NH4.3.lm)

#4
xyplot(NH4~time, data = subset(uca_efflux, Animal == 4 ))
NH4.4.lm = lm(NH4~time, data = subset(uca_efflux, Animal == 4 ))
anova(NH4.4.lm)
summary(NH4.4.lm)
coef(NH4.4.lm)

#5
xyplot(NH4~time, data = subset(uca_efflux, Animal == 5 ))
NH4.5.lm = lm(NH4~time, data = subset(uca_efflux, Animal == 5 ))
anova(NH4.5.lm)
summary(NH4.5.lm)
coef(NH4.5.lm)

#6
xyplot(NH4~time, data = subset(uca_efflux, Animal == 6 ))
NH4.6.lm = lm(NH4~time, data = subset(uca_efflux, Animal == 6 ))
anova(NH4.6.lm)
summary(NH4.6.lm)
coef(NH4.6.lm)

#7
xyplot(NH4~time, data = subset(uca_efflux, Animal == 7 ))
NH4.7.lm = lm(NH4~time, data = subset(uca_efflux, Animal == 7 ))
anova(NH4.7.lm)
summary(NH4.7.lm)
coef(NH4.7.lm)

#8
xyplot(NH4~time, data = subset(uca_efflux, Animal == 8 & time !=15))
NH4.8.lm = lm(NH4~time, data = subset(uca_efflux, Animal == 8 & time !=15))
anova(NH4.8.lm)
summary(NH4.8.lm)
coef(NH4.8.lm)
