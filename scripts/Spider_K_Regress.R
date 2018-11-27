#Spider K regression information

#Obtaining K slope from animals 1 through 8 for K

#1
xyplot(K~time, data = subset(spider_efflux, Animal == 1 ))
K.1.lm = lm(K~time, data = subset(spider_efflux, Animal == 1 ))
anova(K.1.lm)
summary(K.1.lm)
coef(K.1.lm)

#2
xyplot(K~time, data = subset(spider_efflux, Animal == 2 ))
K.2.lm = lm(K~time, data = subset(spider_efflux, Animal == 2 ))
anova(K.2.lm)
summary(K.2.lm)
coef(K.2.lm)

#3
xyplot(K~time, data = subset(spider_efflux, Animal == 3 & time !=5 ))
K.3.lm = lm(K~time, data = subset(spider_efflux, Animal == 3 & time !=5))
anova(K.3.lm)
summary(K.3.lm)
coef(K.3.lm)

#4
xyplot(K~time, data = subset(spider_efflux, Animal == 4 ))
K.4.lm = lm(K~time, data = subset(spider_efflux, Animal == 4 ))
anova(K.4.lm)
summary(K.4.lm)
coef(K.4.lm)

#5
xyplot(K~time, data = subset(spider_efflux, Animal == 5 ))
K.5.lm = lm(K~time, data = subset(spider_efflux, Animal == 5 ))
anova(K.5.lm)
summary(K.5.lm)
coef(K.5.lm)

#6
xyplot(K~time, data = subset(spider_efflux, Animal == 6 ))
K.6.lm = lm(K~time, data = subset(spider_efflux, Animal == 6 ))
anova(K.6.lm)
summary(K.6.lm)
coef(K.6.lm)

#7
xyplot(K~time, data = subset(spider_efflux, Animal == 7 ))
K.7.lm = lm(K~time, data = subset(spider_efflux, Animal == 7 ))
anova(K.7.lm)
summary(K.7.lm)
coef(K.7.lm)

#8
xyplot(K~time, data = subset(spider_efflux, Animal == 8 & time !=5))
K.8.lm = lm(K~time, data = subset(spider_efflux, Animal == 8 & time !=5))
anova(K.8.lm)
summary(K.8.lm)
coef(K.8.lm)
