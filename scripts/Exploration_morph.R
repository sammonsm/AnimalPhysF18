#Libraries
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)

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

#All the plots?
#Making a way to look at all morphology measures

ggplot(morph.data, aes((log(mass)))) + 
  geom_point(aes(y = log(A.cm)), size = 2, colour = "red") +
  geom_point(aes(y = log(C.cm)), size = 2, colour = "orange")+
  geom_point(aes(y = log(D.cm)), size = 2, colour = "yellow")+
  geom_point(aes(y = log(thickness.cm)), size = 2, colour = "green")+
  geom_point(aes(y = log(claw.vol)), size = 2, colour = "blue")+
  geom_point(aes(y = log(claw.area)), size = 2, colour = "purple")

#Creating a working DF of transformed data

#Subsetting the data from morph.data that is needed

#Designating which variables will be subset
working.morph.data.vars <- c("mass","A.cm","B.cm","C.cm","D.cm","E.cm","F.cm","thickness.cm","claw.area","claw.vol")

#Creating a new df with just the variables of "working.morph.data.vars"
working.morph.data <- morph.data[working.morph.data.vars]
View(working.morph.data)

#Transforming the new working df using r package "dplyr"/"tidyr"
help("gather")
#THIS FUCKING WORKS!!!!
long.morph.data <- working.morph.data %>% gather(morph.descriptor, morph.value, A.cm:claw.vol)
View(long.morph.data)


#Creating a labellar
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
  geom_smooth(data=long.morph.data, aes(x = log(mass), y = log(morph.value)), method = "lm", se = FALSE, color= "purple")
  

#Extracting summary statistics on each model in order to look at equations for lines
morph.info<-
  long.morph.data %>%
  group_by(morph.descriptor) %>%
  summarize( = mean(weight),
            min_weight = min(weight))

#The really long way
xyplot(log(A.cm) ~ log(mass), data = working.morph.data, type=c("p","r")) 

#Stats
summary(lm(log(claw_area) ~ log(mass), data = morph))


