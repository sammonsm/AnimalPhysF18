#Libraries
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)

#Import data
morph.data<-read.csv("./data/Morphology_2018.csv", na.strings=c("","NA"))
View(morph.data)

#Puttin in dem NAs!!!


#Exploriatiating

#Making a new variable: claw.vol!
mutate(morph.data,
       A.cm = A/10,
       C.cm = C/10,
       D.cm = D/10,
       thickness.cm = thickness/10,
       claw.vol = (A.cm-D.cm)*C.cm*thickness.cm
)
#New colums with data in cm
morph.data$A.cm <- morph.data$A/10
morph.data$C.cm <- morph.data$C/10
morph.data$D.cm <- morph.data$D/10
morph.data$thickness.cm <- morph.data$thickness/10
#Claw Area in cm^2
morph.data$claw.area <- (morph.data$A.cm-morph.data$D.cm)*morph.data$C.cm

#Claw Volume in cm^3
morph.data$claw.vol <- (morph.data$A.cm-morph.data$D.cm)*morph.data$C.cm*morph.data$thickness.cm

#Distribution of Claw Volumes
histogram(~claw.vol, breaks=6, data=morph.data, xlab = expression(paste("Claw Volume cm"^"3")), col="gold")

#Does claw volume differ in lefthanded versus righthanded crabs?
#get descriptive stats
favstats(claw.vol~ Hand, data = morph.data)
#run a t-test
t.test(claw.vol~ Hand, data = morph.data)



#ggplot2 goodness
ggplot(data=morph.data, aes(claw.vol)) + 
  geom_histogram(bins=6)+
  xlab("Claw Volume (cm^3)") +
  ylab("Number of Males")


#plotting dat claw volume against dat boody mass
ggplot()+
  geom_point(data=morph.data, aes(x = log(mass), y = log(claw.vol)), color = "forest green") +
  geom_smooth(data=morph.data, aes(x = log(mass), y = log(claw.vol)), method = "lm", se = FALSE, color= "black")+
  xlab("Log of Body Mass (g)")+
  ylab("Log of Major Cheliped Volume (cm^3)")

#plotting dat claw area against dat boody mass
ggplot()+
  geom_point(data=morph.data, aes(x = log(mass), y = log(claw.area)), color = "forest green") +
  geom_smooth(data=morph.data, aes(x = log(mass), y = log(claw.area)), method = "lm", se = FALSE, color= "black")+
  xlab("Log of Body Mass (g)")+
  ylab("Log of Major Cheliped Area (cm^2)")


