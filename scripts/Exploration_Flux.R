#Flux exploration (influx and efflux)

#Import data
flux <- read.csv("./data/Flux_Data_Biol244_102318 - Sheet1.csv")
View(flux)

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


#Makes a new data frame with only the Rb uptake data

flux.Rb = subset(flux, treat > 16.5)
#Gets rid of flux media control
flux.Rb = subset(flux.Rb, treat < 20.5) 
#makes a new column temporarily filled with 100 for each SW value
flux.Rb$SW = 100 


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

#Obtaining slope from animals 11 and 12 for Na
#11
xyplot(Na~time, data = subset(flux.efflux, Animal == 11 ))
Na.11.lm = lm(Na~time, data = subset(flux.efflux, Animal == 11 ))
anova(Na.11.lm)
summary(Na.11.lm)
coef(Na.11.lm)
#12
xyplot(Na~time, data = subset(flux.efflux, Animal == 12 ))
Na.12.lm = lm(Na~time, data = subset(flux.efflux, Animal == 12 ))
anova(Na.12.lm)
summary(Na.12.lm)
coef(Na.12.lm)

#Obtaining slope from animals 11 and 12 for NH4
#11
xyplot(NH4~time, data = subset(flux.efflux, Animal == 11 ))
NH4.11.lm = lm(NH4~time, data = subset(flux.efflux, Animal == 11 ))
anova(NH4.11.lm)
summary(NH4.11.lm)
coef(NH4.11.lm)
#12
xyplot(NH4~time, data = subset(flux.efflux, Animal == 12 ))
NH4.12.lm = lm(NH4~time, data = subset(flux.efflux, Animal == 12 ))
anova(NH4.12.lm)
summary(NH4.12.lm)
coef(NH4.12.lm)

#Obtaining slope from animals 11 and 12 for K
#11
xyplot(K~time, data = subset(flux.efflux, Animal == 11 ))
K.11.lm = lm(K~time, data = subset(flux.efflux, Animal == 11 ))
anova(K.11.lm)
summary(K.11.lm)
coef(K.11.lm)
#12
xyplot(K~time, data = subset(flux.efflux, Animal == 12 & time!=20))
K.12.lm = lm(K~time, data = subset(flux.efflux, Animal == 12 & time !=20 ))
anova(K.12.lm)
summary(K.12.lm)
coef(K.12.lm)
