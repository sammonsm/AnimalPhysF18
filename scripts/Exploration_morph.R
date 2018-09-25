#Libraries
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)

#Color libs
# Install
install.packages("wesanderson")
# Load
library(wesanderson)

#Import data and puttin in dem NAs!!!
morph.data<-read.csv("./data/Morphology_2018.csv", na.strings=c("","NA"))
View(morph.data)

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
morph.data$B.cm <- morph.data$B/10
morph.data$C.cm <- morph.data$C/10
morph.data$D.cm <- morph.data$D/10
morph.data$E.cm <- morph.data$E/10
morph.data$F.cm <- morph.data$F/10
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
#distribution of body masses
ggplot(data=morph.data, aes(mass)) + 
  geom_histogram(bins=6, fill="forest green", color= "black")+
  xlab("Body Mass (g)") +
  ylab("Number of Males")+
  ggtitle("Distribution of Body Masses of Male Fiddler Crabs")


#plotting claw volume against body mass
ggplot()+
  geom_point(data=morph.data, aes(x = log(mass), y = log(claw.vol)), color = "forest green") +
  geom_smooth(data=morph.data, aes(x = log(mass), y = log(claw.vol)), method = "lm", se = FALSE, color= "black")+
  xlab("Log of Body Mass (g)")+
  ylab("Log of Major Cheliped Volume (cm^3)")


#plotting claw area against body mass
ggplot()+
  geom_point(data=morph.data, aes(x = log(mass), y = log(claw.area)), color = "forest green") +
  geom_smooth(data=morph.data, aes(x = log(mass), y = log(claw.area)), method = "lm", se = FALSE, color= "black")+
  xlab("Log of Body Mass (g)")+
  ylab("Log of Major Cheliped Area (cm^2)")

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

#Creating a new df with just the variables of "working.morph.data.vars"
working.morph.data <- morph.data[working.morph.data.vars]
View(working.morph.data)

#Another one to do bw plots with handidness
working.morph.data2 <- morph.data[working.morph.data.vars2]
View(working.morph.data2)

#Transforming the new working df using r package "dplyr"/"tidyr" (not really sure which ¯\_(ツ)_/¯)
help("gather")
#THIS WORKS!!!!
long.morph.data <- working.morph.data %>% gather(morph.descriptor, morph.value, A.cm:claw.vol)
View(long.morph.data)

#One with handidness
long.morph.data2 <- working.morph.data2 %>% gather(morph.descriptor, morph.value, A.cm:claw.vol)

#Ommitting NAs that I thought were already gone
new.long.morph.data2 <- na.omit(long.morph.data2)


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
ggplot()+
  geom_point(long.morph.data, aes)

ggplot(long.morph.data, aes(x=log(mass), y=log(morph.value))) +
  geom_point() +
  xlab("log(mass)") +
  ylab("log(morph value)") + 
  ggtitle("Scaling of Body Mass to Morphological Values in Fiddler Crabs")+
  facet_wrap(~morph.descriptor, scales = "free")+
  geom_smooth(data=long.morph.data, aes(x = log(mass), y = log(morph.value)), method = "lm", se = TRUE, color= "purple")
  
#Facetted figure of morph data bw plots with handidness because why not
ggplot(new.long.morph.data2, aes(x=Hand, y=morph.value, fill=Hand)) +
  geom_boxplot() +
  ylab("Morphological Value") + 
  ggtitle("Handidness Across Morphological Traits")+
  facet_wrap(~morph.descriptor)+
  scale_fill_discrete(name="Major Cheliped",
                      breaks=c("R", "L"),
                      labels=c("Right-Handed", "Left-Handed"))

#Extracting summary statistics on each model in order to look at equations for lines
morph.info<-
  long.morph.data %>%
  subset(morph.descriptor)%>%
  morph.value.lm <- lm(log(morph.value)~log(mass), data = long.morph.data)%>%
  summary(morph.value.lm)

morph.info

#The really long way
A.cm.lm <- lm(log(A.cm)~log(mass), data = working.morph.data)

#Stats
summary(A.cm.lm)


