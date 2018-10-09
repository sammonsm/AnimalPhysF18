#Libraries
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)
#Color libs
library(wesanderson)

#Import data and puttin in dem NAs!!!
morph.data<-read.csv("./data/Morphology_2018.csv", na.strings=c("","NA"))
View(morph.data)

#Exploriatiating

#Making a new variable: claw.vol!
#Proof of concept
mutate(morph.data,
       A.cm = A/10,
       C.cm = C/10,
       D.cm = D/10,
       thickness.cm = thickness/10,
       claw.vol = (A.cm-D.cm)*C.cm*thickness.cm
)

#Actual new colums with data in cm
morph.data$A.cm <- morph.data$A/10
morph.data$B.cm <- morph.data$B/10
morph.data$C.cm <- morph.data$C/10
morph.data$D.cm <- morph.data$D/10
morph.data$E.cm <- morph.data$E/10
morph.data$F.cm <- morph.data$F/10
morph.data$thickness.cm <- morph.data$thickness/10
#Claw Area in cm^2
morph.data$claw.area <- (morph.data$A.cm-morph.data$D.cm)*morph.data$C.cm

#Carapace Area in cm^2
morph.data$carapace.area <- (morph.data$E.cm*morph.data$F.cm)

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
#distribution of body masses
ggplot(data=morph.data, aes(mass)) + 
  geom_histogram(bins=6, fill="forest green", color= "black")+
  xlab("Body Mass (g)") +
  ylab("Number of Males")+
  ggtitle("Distribution of Body Masses of Male Fiddler Crabs")

favstats(~mass, data=morph.data)


#plotting claw volume against body mass
ggplot()+
  geom_point(data=morph.data, aes(x = log(mass), y = log(claw.vol)), color = "forest green") +
  geom_smooth(data=morph.data, aes(x = log(mass), y = log(claw.vol)), method = "lm", se = TRUE, color= "black")+
  xlab("Log of Body Mass (g)")+
  ylab("Log of Major Cheliped Volume (cm^3)")


#plotting claw area against body mass
ggplot()+
  geom_point(data=morph.data, aes(x = log(mass), y = log(claw.area)), color = "forest green") +
  geom_smooth(data=morph.data, aes(x = log(mass), y = log(claw.area)), method = "lm", se = TRUE, color= "black")+
  xlab("Log of Body Mass (g)")+
  ylab("Log of Major Cheliped Area (cm^2)")

#plotting carapace area against body mass
ggplot()+
  geom_point(data=morph.data, aes(x = log(mass), y = log(carapace.area)), color = "forest green") +
  geom_smooth(data=morph.data, aes(x = log(mass), y = log(carapace.area)), method = "lm", se = TRUE, color= "black")+
  xlab("log(body mass (g))")+
  ylab("log(carapace area (cm^2))")
  

#All the plots?
#Making a way to look at all morphology measures

#Not great, but does something!
ggplot(morph.data, aes((log(mass)))) + 
  geom_point(aes(y = log(A.cm)), size = 2, colour = "red") +
  geom_point(aes(y = log(C.cm)), size = 2, colour = "orange")+
  geom_point(aes(y = log(D.cm)), size = 2, colour = "yellow")+
  geom_point(aes(y = log(thickness.cm)), size = 2, colour = "green")+
  geom_point(aes(y = log(claw.vol)), size = 2, colour = "blue")+
  geom_point(aes(y = log(claw.area)), size = 2, colour = "purple")

#Creating a working df of transformed data

#Subsetting the data from morph.data that is needed

#Designating which variables will be subset
working.morph.data.vars <- c("mass","A.cm","B.cm","C.cm","D.cm","E.cm","F.cm","thickness.cm","claw.area","claw.vol")

working.morph.data.vars2 <- c("Hand","A.cm","B.cm","C.cm","D.cm","E.cm","F.cm","thickness.cm","claw.area","claw.vol")

working.morph.data.vars3 <- c("mass","A.cm","B.cm","C.cm","D.cm","E.cm","F.cm","thickness.cm")

working.morph.data.vars4 <-c("Hand","A.cm","B.cm","C.cm","D.cm","E.cm","F.cm","thickness.cm")

#Creating a new df with just the variables of "working.morph.data.vars"
working.morph.data <- morph.data[working.morph.data.vars]
View(working.morph.data)

#Another one to do bw plots with handidness
working.morph.data2 <- morph.data[working.morph.data.vars2]
View(working.morph.data2)

#Another one with only cm measures (no area or volume)
working.morph.data3 <- morph.data[working.morph.data.vars3]
View(working.morph.data3)

#No are or volume, but has handidness
working.morph.data4 <-morph.data[working.morph.data.vars4]
View(working.morph.data4)

#Transforming the new working df using r package "dplyr"/"tidyr" (not really sure which ¯\_(ツ)_/¯)
help("gather")
#THESE WORK!!!!
long.morph.data <- working.morph.data %>% gather(morph.descriptor, morph.value, A.cm:claw.vol)
View(long.morph.data)

#One with handidness
long.morph.data2 <- working.morph.data2 %>% gather(morph.descriptor, morph.value, A.cm:claw.vol)
#Ommitting NAs that I thought were already gone
new.long.morph.data2 <- na.omit(long.morph.data2)

#One with only cm measures (no area or volume)
long.morph.data3 <- working.morph.data3 %>% gather(morph.descriptor, morph.value, A.cm:thickness.cm)
View(long.morph.data3)

#Handidness without volume or area
long.morph.data4 <- working.morph.data4 %>% gather(morph.descriptor, morph.value, A.cm:thickness.cm)
long.morph.data4 <- na.omit(long.morph.data4)
View(long.morph.data4)

#Creating a labellar (still fiddling with this as of 9/25/18)
Morph.Names <- c(
  `A.cm` = "Claw Length (cm)",
  `B.cm` = "Dactyl Length (cm)",
  `C.cm` = "Claw Height (cm)",
  `D.cm` = "Pollex Length (cm)",
  `E.cm` = "Carapace Length (cm)",
  `F.cm` = "Carapace Width (cm)",
  `thickness.cm` = "Claw Thickness (cm)",
  `claw.area` = "Anterior Major Claw Area (cm^2)",
  `claw.vol` = "Anterior Major Claw Volume (cm^3)"
)

#Creating a facet wrapped figure of log(mass):log(morph.descriptor) for A.cm:claw.vol
ggplot(long.morph.data, aes(x=log(mass), y=log(morph.value))) +
  geom_point() +
  xlab("log(mass)") +
  ylab("log(morph value)") + 
  ggtitle("Scaling of Body Mass to Morphological Values in Fiddler Crabs")+
  facet_wrap(~morph.descriptor, scales = "free")+
  geom_smooth(data=long.morph.data, aes(x = log(mass), y = log(morph.value)), method = "lm", se = TRUE, color= "purple")
  
#Creating a facet wrapped figure of log(mass):log(morph.descriptor) for A.cm:thickness.cm
ggplot(long.morph.data3, aes(x=log(mass), y=log(morph.value))) +
  geom_point() +
  xlab("log(body mass (g))") +
  ylab("log(morph length (cm))") + 
  facet_wrap(~morph.descriptor, scales = "free")+
  geom_smooth(data=long.morph.data3, aes(x = log(mass), y = log(morph.value)), method = "lm", se = TRUE, color= "purple")

#Facetted figure of morph data bw plots with handedness because why not
ggplot(new.long.morph.data, aes(x=Hand, y=morph.value, fill=Hand)) +
  geom_boxplot() +
  ylab("Morphological Value") + 
  ggtitle("Handedness Across Morphological Traits")+
  facet_wrap(~morph.descriptor)+
  scale_fill_discrete(name="Major Cheliped",
                      breaks=c("R", "L"),
                      labels=c("Right-Handed", "Left-Handed"))

#Facetted figure of morph data without area or volume, bw plots with handedness
ggplot(long.morph.data4, aes(x=Hand, y=morph.value, fill=Hand)) +
  geom_boxplot() +
  ylab("Morph Length (cm)") + 
  ggtitle("Handedness Across Morphological Traits")+
  facet_wrap(~morph.descriptor)+
  scale_fill_discrete(name="Major Cheliped",
                      breaks=c("R", "L"),
                      labels=c("Right-Handed", "Left-Handed"))


#The really long way
A.cm.lm <- lm(log(A.cm)~log(mass), data = working.morph.data)
B.cm.lm <- lm(log(B.cm)~log(mass), data = working.morph.data)
C.cm.lm <- lm(log(C.cm)~log(mass), data = working.morph.data)
D.cm.lm <- lm(log(D.cm)~log(mass), data = working.morph.data)
E.cm.lm <- lm(log(E.cm)~log(mass), data = working.morph.data)
F.cm.lm <- lm(log(F.cm)~log(mass), data = working.morph.data)
thickness.cm.lm <- lm(log(thickness.cm)~log(mass), data = working.morph.data)
carapace.area.lm <- lm(log(carapace.area)~log(mass), data = morph.data)


#Stats
summary(A.cm.lm)
summary(B.cm.lm)
summary(C.cm.lm)
summary(D.cm.lm)
summary(E.cm.lm)
summary(F.cm.lm)
summary(thickness.cm.lm)
summary(carapace.area.lm)

