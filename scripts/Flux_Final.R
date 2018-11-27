#TOP----

#Flux Final
#Experiments C include both class and independent data

#IMPORT DATA----

#Class efflux df
ion_efflux <- read.csv("./data/Efflux datasheet - Sheet1.csv")
View(ion_efflux)
#Class influx df
influx<- read.csv("./data/flux_Rb - Sheet 1.csv")
View(influx)
#Independent experiments crab information df
crab_info <- read.csv("./data/crab_data_6_11_2018 - Sheet1.csv")
View(crab_info)
#Independent experiments ion slope df
all_ion_slope <- read.csv("./data/all_ion_regression_11_08_18 - Sheet1.csv")
View(all_ion_slope)


#LIBRARIES----
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)
library(stringr)

#CLASS FLUX EXPERIMENTS----

#CLASS INFLUX----

#Setting SW as a character
influx$SW = as.character(influx$SW)

#Removing the 2 outliers using subset function (one at ~45, the other at ~2.9)
influx.1 <- subset(influx, Rb < 1)
View(influx.1)

#FIGURE 1----

#Influx Plot
Fig1 <- ggplot(influx.1, aes(x=SW, y=Rb))+
  geom_boxplot()+
  ylab("Rb influx (μmol/g BW * hr)")+
  xlab("Salt Water Concentration %")+
  theme_bw()

Fig1

#CLASS EFFLUX----

#Creating a working df for ion efflux slope data
working.efflux.data.vars <- c("SW","Na","NH4","K")
working.efflux.data <- ion_efflux[working.efflux.data.vars]
View(working.efflux.data)

#Gathering data using tidyr
long.efflux.data <- working.efflux.data %>% gather(ion, rate, Na:K)
View(long.efflux.data)

#Setting SW as a character
long.efflux.data$SW = as.character(long.efflux.data$SW)

#FIGURE 2----

#Uca Efflux Plot
Fig2 <- ggplot(long.efflux.data, aes(x=ion, y=rate, fill = SW)) +
  geom_boxplot()+
  facet_wrap(~ion, scales = "free")+
  ylab("Efflux Rate (μmol/g BW * hr)")+
  xlab("Ion")

Fig2

#INDEPENDENT FLUX EXPERIMENTS----

#Setting a working dataframe
working_all_efflux_data_vars <- c("Genus","Na","NH4","K")
working_all_efflux_data <- all_ion_slope[working_all_efflux_data_vars]
View(working_all_efflux_data)

#Gathering data using tidyr
long_all_slope_data <- working_all_efflux_data %>% gather(ion, rate, Na:K)
View(long_all_slope_data)

#Setting genus as a character
long_all_slope_data$Genus = as.character(long_all_slope_data$Genus)

#FIGURE 3----

#Uca and Labinia Efflux Plot
Fig3 <- ggplot(long_all_slope_data, aes(x=ion, y=rate, fill = Genus)) +
  geom_boxplot()+
  facet_wrap(~ion, scales = "free")+
  ylab("Efflux Rate (μmol/g BW * hr)")+
  xlab("Ion")

Fig3

#STATISTICS----

#Figure 1

#Favstats
favstats(Rb~SW, data = influx.1)
#Making a linear model
influx.1.lm = lm(Rb~SW, data = influx.1)
#ANOVA
anova(influx.1.lm)
#Summary
summary(influx.1.lm)
#Counting the number of observations
nrow(filter(influx.1, SW == 20))
nrow(filter(influx.1, SW == 100))

#Figure 2

#Favstats
favstats(rate~ion|SW, data = long.efflux.data)
#Making a linear model
efflux.lm = lm(rate~ion*SW, data = long.efflux.data)
#ANOVA
anova(efflux.lm)
#Summary
summary(effulx.lm)
#TukeyHSD
TukeyHSD(efflux.lm)

#Figure 3

#Favstats
fav_stats(rate~ion|Genus, data=long_all_slope_data)
#Making a linear model
all.crab.1.lm = lm(rate~ion*Genus, data = long_all_slope_data)
#ANOVA
anova(all.crab.1.lm)
#Summary
summary(all.crab.1.lm)
#TukeyHSD
TukeyHSD(all.crab.1.lm)

#BOTTOM----