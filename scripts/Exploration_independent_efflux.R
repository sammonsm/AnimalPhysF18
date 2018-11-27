#Exploring independent Efflux Experiment
#Mia Fox, Katie Lensmayer, Ryder Sammons

#Import Data

#All efflux data

#Spider
spider_efflux <- read.csv("./data/Biol_244_data_110818_spider - Sheet1.csv")
View(spider_efflux)
#Uca
uca_efflux <- read.csv("./data/Biol_244_data_110818_uca - Sheet1.csv")
View(uca_efflux)

#Crab information
crab_info <- read.csv("./data/crab_data_6_11_2018 - Sheet1.csv")
View(crab_info)

#Ion efflux slope data
uca_ion_slope <- read.csv("./data/uca_ion_regression_11_08_18 - Sheet1.csv")
View(uca_ion_slope)

spider_ion_slope <- read.csv("./data/spider_ion_regression_11_08_18 - Sheet1.csv")
View(spider_ion_slope)

all_ion_slope <- read.csv("./data/all_ion_regression_11_08_18 - Sheet1.csv")
View(all_ion_slope)

#Libraries
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)
library(stringr)


#UCA DATAFRAME MODULATION----

#Adding in salinity treatments (all 100) and times of measurements for each sample

#Use the following code to get SW and Time columns based upon our data codes
uca_efflux$SW = 100 #makes a new column temporarily filled with 100 for each SW value
uca_efflux$time = 0 #makes a new column temporarily filled with 0 for each time value

#UCA TIMES
#Now, put the right levels into the Time column (which is in column 12) based on the treatment codes (in column 10)
# 0 Minutes
for (i in 1:32)
{
  if(uca_efflux[i, 8] == 1 | uca_efflux[i, 8] == 5 | uca_efflux[i, 8] == 9 | uca_efflux[i, 8] == 13 | uca_efflux[i, 8] == 17 | uca_efflux[i, 8] == 21 | uca_efflux[i, 8] == 25 | uca_efflux[i, 8] == 29) uca_efflux[i, 10] = 0 
}
# 5 Minutes
for (i in 1:32)
{
  if(uca_efflux[i, 8] == 2 | uca_efflux[i, 8] == 6 | uca_efflux[i, 8] == 10 | uca_efflux[i, 8] == 14 | uca_efflux[i, 8] == 18 | uca_efflux[i, 8] == 22 | uca_efflux[i, 8] == 26 | uca_efflux[i, 8] == 30) uca_efflux[i, 10] = 5
}
# 15 Minutes
for (i in 1:32)
{
  if(uca_efflux[i, 8] == 3 | uca_efflux[i, 8] == 7 | uca_efflux[i, 8] == 11 | uca_efflux[i, 8] == 15 | uca_efflux[i, 8] == 19 | uca_efflux[i, 8] == 23 | uca_efflux[i, 8] == 27 | uca_efflux[i, 8] == 31) uca_efflux[i, 10] = 15
}
# 25 Minutes
for (i in 1:32)
{
  if(uca_efflux[i, 8] == 4 | uca_efflux[i, 8] == 8 | uca_efflux[i, 8] == 12 | uca_efflux[i, 8] == 16 | uca_efflux[i, 8] == 20 | uca_efflux[i, 8] == 24 | uca_efflux[i, 8] == 28 | uca_efflux[i, 8] == 32) uca_efflux[i, 10] = 25
}

#Add a column for Animal number and fill it in.  We used the same code for the last analysis.
uca_efflux$Animal = NA
for (i in 1:8)
{for (j in 1:4)
{k = ((i - 1)*4 + j)
uca_efflux[k, 11] = i}
}

#SPIDER DATAFRAME MODULATION----

#Use the following code to get SW and Time columns based upon our data codes
spider_efflux$SW = 100 #makes a new column temporarily filled with 100 for each SW value
spider_efflux$time = 0 #makes a new column temporarily filled with 0 for each time value

#SPIDER TIMES
#Now, put the right levels into the Time column (which is in column 12) based on the treatment codes (in column 10)
# 0 Minutes
for (i in 1:32)
{
  if(spider_efflux[i, 8] == 1 | spider_efflux[i, 8] == 5 | spider_efflux[i, 8] == 9 | spider_efflux[i, 8] == 13 | spider_efflux[i, 8] == 17 | spider_efflux[i, 8] == 21 | spider_efflux[i, 8] == 25 | spider_efflux[i, 8] == 29) spider_efflux[i, 10] = 0 
}
# 5 Minutes
for (i in 1:32)
{
  if(spider_efflux[i, 8] == 2 | spider_efflux[i, 8] == 6 | spider_efflux[i, 8] == 10 | spider_efflux[i, 8] == 14 | spider_efflux[i, 8] == 18 | spider_efflux[i, 8] == 22 | spider_efflux[i, 8] == 26 | spider_efflux[i, 8] == 30) spider_efflux[i, 10] = 5
}
# 15 Minutes
for (i in 1:32)
{
  if(spider_efflux[i, 8] == 3 | spider_efflux[i, 8] == 7 | spider_efflux[i, 8] == 11 | spider_efflux[i, 8] == 15 | spider_efflux[i, 8] == 19 | spider_efflux[i, 8] == 23 | spider_efflux[i, 8] == 27 | spider_efflux[i, 8] == 31) spider_efflux[i, 10] = 15
}
# 25 Minutes
for (i in 1:32)
{
  if(spider_efflux[i, 8] == 4 | spider_efflux[i, 8] == 8 | spider_efflux[i, 8] == 12 | spider_efflux[i, 8] == 16 | spider_efflux[i, 8] == 20 | spider_efflux[i, 8] == 24 | spider_efflux[i, 8] == 28 | spider_efflux[i, 8] == 32) spider_efflux[i, 10] = 25
}

#FIXING SPIDER SAMPLE 1S TIMES TO REFLECT ERROR IN EXPERIMENT (EFFLUX WAS SAMPLED AT t=0,5,10,20)
#Now, put the right levels into the Time column (which is in column 12) based on the treatment codes (in column 10)
# 0 Minutes
for (i in 1:4)
{
  if(spider_efflux[i, 8] == 1) spider_efflux[i, 10] = 0 
}
# 5 Minutes
for (i in 1:32)
{
  if(spider_efflux[i, 8] == 2) spider_efflux[i, 10] = 5 
}
# 10 Minutes
for (i in 1:32)
{
  if(spider_efflux[i, 8] == 3) spider_efflux[i, 10] = 10 
}
# 20 Minutes
for (i in 1:32)
{
  if(spider_efflux[i, 8] == 4) spider_efflux[i, 10] = 20 
}

#Add a column for Animal number and fill it in.  We used the same code for the last analysis.
spider_efflux$Animal = NA
for (i in 1:8)
{for (j in 1:4)
{k = ((i - 1)*4 + j)
spider_efflux[k, 11] = i}
}


#PLOTS----

#Uca
#Creating a working df for ion efflux slope data
working_uca_efflux_data_vars <- c("SW","mNa","mNH4","mK")
working_uca_efflux_data <- uca_ion_slope[working_uca_efflux_data_vars]
View(working_uca_efflux_data)

long_uca_slope_data <- working_uca_efflux_data %>% gather(ion, slope, mNa:mK)
View(long_uca_slope_data)

long.efflux.data$SW = as.character(long.efflux.data$SW)

ggplot(long_uca_slope_data, aes(x=ion, y=slope)) +
  geom_boxplot()+
  facet_wrap(~ion, scales = "free")+
  ylab("[Ion] (μM)")+
  xlab("Ion")

#Spider
#Creating a working df for ion efflux slope data
working_spider_efflux_data_vars <- c("SW","mNa","mNH4","mK")
working_spider_efflux_data <- spider_ion_slope[working_spider_efflux_data_vars]
View(working_spider_efflux_data)

long_spider_slope_data <- working_spider_efflux_data %>% gather(ion, slope, mNa:mK)
View(long_spider_slope_data)

long.efflux.data$SW = as.character(long.efflux.data$SW)

ggplot(long_spider_slope_data, aes(x=ion, y=slope)) +
  geom_boxplot()+
  facet_wrap(~ion, scales = "free")+
  ylab("[Ion] (μM)")+
  xlab("Ion")

#All crab
working_all_efflux_data_vars <- c("Genus","Na","NH4","K")
working_all_efflux_data <- all_ion_slope[working_all_efflux_data_vars]
View(working_all_efflux_data)

long_all_slope_data <- working_all_efflux_data %>% gather(ion, rate, Na:K)
View(long_all_slope_data)

long_all_slope_data$Genus = as.character(long_all_slope_data$Genus)

ggplot(long_all_slope_data, aes(x=ion, y=rate, fill = Genus)) +
  geom_boxplot()+
  facet_wrap(~ion, scales = "free")+
  ylab("Efflux Rate (μmol/g BW * hr)")+
  xlab("Ion")

#Statistics
fav_stats(rate~ion|Genus, data=long_all_slope_data)

all.crab.1.lm = lm(rate~ion*Genus, data = long_all_slope_data)
anova(all.crab.1.lm)
summary(all.crab.1.lm)
TukeyHSD(all.crab.1.lm)
