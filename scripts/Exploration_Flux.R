#Flux exploration (influx and efflux)

#Import data
flux <- read.csv("./data/Flux_Data_Biol244_102318 - Sheet1.csv")
View(flux)
ion_efflux <- read.csv("./data/Efflux datasheet - Sheet1.csv")
View(ion_efflux)
influx<- read.csv("./data/flux_Rb - Sheet 1.csv")
View(influx)
#Libraries
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)
library(stringr)

#Create new dataframe columns on "flux" that designates team by A,B,C,D, or E and treatment 1-21 (1-16=efflux, 17-21=influx)

flux$team = str_sub(flux$Sample,1,1) #start at first character and include one character
flux$treat = str_sub(flux$Sample,3) #start at the third character and include all the rest of the characters
flux$team = as.factor(flux$team) #makes this column into a factor
flux$treat = as.numeric(flux$treat) #makes this column numeric


#Assigns correct salinity treatment based on our treatment codes
#Uses a loop structure
#Uses this syntax to indicate location in the dataframe:  dataframename[rownumber, columnnumber] 
                 for (i in 1:20)
                 {if(flux.Rb[i, 10] == 17) flux.Rb[i, 11] = 20 
                 #add another line to assign the remaining 20 SW values
                 if(flux.Rb[i, 10] == 18) flux.Rb[i, 11] = 20
                 }
                 flux.Rb$SW = as.factor(flux.Rb$SW) #makes SW a factor
                 
                 
                 #calculate Rb flux rate from [Rb] data and a new column in flux.Rb
                 #do proper statistical and graphical analysis.  Consider subsetting the data to ignore clear outliers.
                 #remove outliers
                 flux.Rb = subset(flux.Rb, team != "A")
                 
                 
                 #Now, make a new dataframe with only the efflux data (see above for strategy).  Start with the original dataset
                 flux.efflux = subset(flux, treat < 16.5)
                 
                 #Use the following code to get SW and Time columns based upon our data codes
                 flux.efflux$SW = 100 #makes a new column temporarily filled with 100 for each SW value
                 flux.efflux$time = 0 #makes a new column temporarily filled with 0 for each time value
                 
                 #Now, put the right levels into the SW column (which is in column 11) based on the treatment codes (in column 10).  Check to make sure those are true for your datasheet.
                 #This uses the “|” symbol to mean “or”.  
                 for (i in 1:100)
                 {
                   if(flux.efflux[i, 10] == 1 | flux.efflux[i, 10] == 2 | flux.efflux[i, 10] == 3 | flux.efflux[i, 10] == 4 | flux.efflux[i, 10] == 5 | flux.efflux[i, 10] == 6 | flux.efflux[i, 10] == 7 | flux.efflux[i, 10] == 8 ) flux.efflux[i, 11] = 20
                 }
                 #Now, put the right levels into the Time column (which is in column 12) based on the treatment codes (in column 10)
                 # 0 Minutes
                 for (i in 1:100)
                 {
                   if(flux.efflux[i, 10] == 1 | flux.efflux[i, 10] == 5 | flux.efflux[i, 10] == 9 | flux.efflux[i, 10] == 13) flux.efflux[i, 12] = 0 
                 }
                 # 10 Minutes
                 for (i in 1:100)
                 {
                   if(flux.efflux[i, 10] == 2 | flux.efflux[i, 10] == 6 | flux.efflux[i, 10] == 10 | flux.efflux[i, 10] == 14) flux.efflux[i, 12] = 10 
                 }
                 # 20 Minutes
                 for (i in 1:100)
                 {
                   if(flux.efflux[i, 10] == 3 | flux.efflux[i, 10] == 7 | flux.efflux[i, 10] == 11 | flux.efflux[i, 10] == 15) flux.efflux[i, 12] = 20 
                 }
                 # 30 Minutes
                 for (i in 1:100)
                 {
                   if(flux.efflux[i, 10] == 4 | flux.efflux[i, 10] == 8 | flux.efflux[i, 10] == 12 | flux.efflux[i, 10] == 16) flux.efflux[i, 12] = 30 
                 }

#Add a column for Animal number and fill it in.  We used the same code for the last analysis.  Notice that we put this value in column 13.  Make sure that is correct.
flux.efflux$Animal = NA
for (i in 1:20)
  {for (j in 1:4)
  {k = ((i - 1)*4 + j)
  flux.efflux[k, 13] = i}
}                 

#plot all the data for one group for one ion (Na)
xyplot(Na~time, groups = Animal, data = subset(flux.efflux, team == "A"))

#plot just one animal
xyplot(Na~time, data = subset(flux.efflux, Animal == 1 ))
xyplot(Na~time, data = subset(flux.efflux, Animal == 1 & time > 5)) #remove time zero (if needed)
xyplot(Na~time, data = subset(flux.efflux, Animal == 1 & time != 20)) #remove time 20 (if needed)

#Setting SW as a character
influx$SW = as.character(influx$SW)

#Removing the 2 outliers using subset function (one at ~45, the other at ~2.9)
influx.1 <- subset(influx, Rb < 1)
View(influx.1)

#Influx Plot
ggplot(influx.1, aes(x=SW, y=Rb))+
  geom_boxplot()+
  ylab("Rb influx (μmol/g BW * hr)")+
  xlab("Salt Water Concentration %")+
  theme_bw()

#Statistics
favstats(Rb~SW, data = influx.1)

influx.1.lm = lm(Rb~SW, data = influx.1)
anova(influx.1.lm)
summary(influx.1.lm)

#Counting the number of observations
nrow(filter(influx.1, SW == 20))
nrow(filter(influx.1, SW == 100))

#Creating a working df for ion efflux slope data
working.efflux.data.vars <- c("SW","Na","NH4","K")
working.efflux.data <- ion_efflux[working.efflux.data.vars]
View(working.efflux.data)

long.efflux.data <- working.efflux.data %>% gather(ion, rate, Na:K)
View(long.efflux.data)

long.efflux.data$SW = as.character(long.efflux.data$SW)


ggplot(long.efflux.data, aes(x=ion, y=rate, fill = SW)) +
  geom_boxplot()+
  facet_wrap(~ion, scales = "free")+
  ylab("Efflux Rate (μmol/g BW * hr)")+
  xlab("Ion")

#Statistics

#Favstats
favstats(rate~ion|SW, data = long.efflux.data)
#Develop linear model for ANOVA
efflux.lm = lm(rate~ion*SW, data = long.efflux.data)
#Summary
summary(effulx.lm)
#ANOVA
anova(efflux.lm)
TukeyHSD(efflux.lm)


#creating a working df for ion efflux conc data
working.efflux.conc.data.vars <- c("SW","Na", "NH4", "K","Mg","Ca","time")
working.efflux.conc.data <- flux.efflux[working.efflux.conc.data.vars]
View(working.efflux.conc.data)

long.efflux.conc.data <- working.efflux.conc.data %>% gather(ion, conc, Na:Ca)
View(long.efflux.conc.data)
efflux.conc.end <-subset(long.efflux.conc.data, long.efflux.conc.data$time > 25)

long.efflux.conc.data$time = as.character(long.efflux.conc.data$time)
long.efflux.conc.data$SW = as.character(long.efflux.conc.data$SW)

ggplot(long.efflux.conc.data, aes(x=ion, y=conc, fill=time)) +
  geom_boxplot()+
  facet_wrap(~ion, scales = "free")+
  ylab("[Ion] (μM)")+
  xlab("Ion")

View(efflux.conc.end)

ggplot(efflux.conc.end, aes(x=ion, y=conc, fill=SW)) +
  geom_boxplot()+
  facet_wrap(~ion, scales = "free")+
  ylab("[Ion] (μM)")+
  xlab("Ion")

#Favstats
favstats(conc~ion|SW, data = efflux.conc.end)
#Develop linear model for ANOVA
efflux.end.lm = lm(conc~ion*SW, data = efflux.conc.end)
#ANOVA
anova(efflux.end.lm)
TukeyHSD(efflux.end.lm)


