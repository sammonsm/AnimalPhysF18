#TOP----
#Osmoregulation and Volume Regulation Final

#LIBRARIES----
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)

#IMPORT DATA----

#Import osmolarity data
uca_osmo <- read.csv("./data/Uca_Osmolarity_2018.csv", na.strings = c("","NA"))
View(uca_osmo)
spider_osmo <- read.csv("./data/Spider_crab_osmolarity_2018.csv", na.strings = c("","NA"))
View(spider_osmo)

#Import volume data
vol_all <- read.csv("./data/Volume_all.csv", na.strings = c("","NA"))
View(vol_all)

#DATA TRANSFORMATION----

#Spider crab osmolarity transformation 
#New column with percent change
spider_osmo$percent_change <- (spider_osmo$Post_Osm - spider_osmo$Pre_Osm)/spider_osmo$Pre_Osm*100

#Working variables
working.spider.vars <- c("Group", "Pre_Osm", "Post_Osm")
#Working data frame
working.spider.osmo <- spider_osmo[working.spider.vars]

#New data frame: spider.osmo.good (dplyr)
spider.osmo.good <- working.spider.osmo %>% gather(Pre.Post, Osmo, Pre_Osm:Post_Osm)
View(spider.osmo.good)

#VOLUME TRANSFORMATION----

#Creating a new colum for %change through for loops
#calculate % change
vol_all$Percent = NA
#Insert number of rows for n.  This starts a loop, with the variable i from 1 to n.  
#It will do everything in the squiggly brackets n times
#a key syntax: dataset_name[r,c].  You can specify a row and column this way
for (i in 1:408) 
{	if(vol_all[i,5] == 0) n = 0
if(vol_all[i,5] == 10) n = 1
if(vol_all[i,5] == 20) n = 2
if(vol_all[i,5] == 30) n = 3
#calculate correct j based on i and n.
j = i-n
vol_all[i,7] = (vol_all[i,6]/vol_all[j,6])*100-100}

#First, each animal needs a number, so we can restrict the regression analysis to just that animal.  
#We know that each animal has 4 lines (one for each sample time).  So we can use code like this:
#This makes a new column titled animal
vol_all$Animal = NA
for (i in 1:102) #where 102 = n = the number of animals
{for (j in 1:4) #this is pretty tricky – we are putting one loop inside another.  
  #We run this inside loop 4 times because we want to list the same number four times.
{k = ((i - 1)*4 + j) #this calculates the row based upon i and j. 
vol_all[k, 8] = i} #this puts the animal number (i) into the calculated row.
}

#TIME COURSE UCA----

#Plot time courses for each species
#Note: par.settings requires that you’ve run a theme template
xyplot(Percent~Time, groups = Salinity, data = subset(vol_all, Species == "Uca"), auto.key = list(corner = c(0.05, 0.95)), jitter.x = T, )

#Run regression analyses and put onto plot
#Remove outliers
g50=subset(vol_all, Percent<50 & Percent>-50)


#makeFun (make function) from mosaicCore
#Provides an easy mechanism for creating simple "mathematical" functions via a formula interface.

#Uca 50
#run these after doing a xyplot for Uca
Uca.50 = lm(Percent~Time, data = subset(vol_all, Species == "Uca" & Salinity == 500))

#Uca 60
Uca.60 = lm(Percent~Time, data = subset(vol_all, Species == "Uca" & Salinity == 1000))

#Uca 70
Uca.70 = lm(Percent~Time, data = subset(vol_all, Species == "Uca" & Salinity == 1500))

#Uca Summaries
summary(Uca.50)
summary(Uca.60)
summary(Uca.70)

#Uca funciton 50, 60, 70 plots
#50
Uca.50.fun = makeFun(Uca.50)
plotFun(Uca.50.fun, add = T, col = "light blue", lwd = 2)
#60
Uca.60.fun = makeFun(Uca.60)
plotFun(Uca.60.fun, add = T, col = "hot pink", lwd = 2)
#70
Uca.70.fun = makeFun(Uca.70)
plotFun(Uca.70.fun, add = T, col = "green", lwd = 2)

#TIME COURSE NEREIS----

#Repeat for Nereis
#50
Nereis.50 = lm(Percent~Time, data = subset(g50, Species == "Nereis" & Salinity == 500))
#60
Nereis.60 = lm(Percent~Time, data = subset(g50, Species == "Nereis" & Salinity == 500))
#70
Nereis.70 = lm(Percent~Time, data = subset(g50, Species == "Nereis" & Salinity == 500))

#Nereis summaries
summary(Nereis.50)
summary(Nereis.60)
summary(Nereis.70)

#Nereis funciton 50, 60, 70 plots
#50
Nereis.50.fun = makeFun(Nereis.50)
plotFun(Nereis.50.fun, add = T, col = "purple", lwd = 2)
#60
Nereis.60.fun = makeFun(Nereis.50)
plotFun(Nereis.60.fun, add = T, col = "blue", lwd = 2)
#70
Nereis.70.fun = makeFun(Nereis.70)
plotFun(Nereis.50.fun, add = T, col = "green", lwd = 2)


#Modeling data with all the variables for both species 
model.1 = lm(Percent ~ Time*Salinity*Species, data = g50)
#ANOVA
anova(model.1)

#REGRESSION DATA FRAME----

#Makes sample and year into characters and creates a new empty dataset for the regression data
vol_all$Species = as.character(vol_all$Species) #needs to be a character
vol_all$Year = as.character(vol_all$Year) #needs to be a character
vol_all.data.sum=data.frame(sample = as.numeric(), slope = as.numeric(), 
                            R2 =as.numeric(), Pvalue = as.numeric(), 
                            SEslope = as.numeric(), Salinity = as.numeric(), 
                            Species = as.numeric(), Year = as.numeric())

#Species needs to be a character too in the new dataframe
vol_all.data.sum$Species = as.character(vol_all.data.sum$Species) 

#Do regressions using a loop
for (i in 1:102) #starts a loop, with the variable i from 1 to 30.  It will do everything in the squiggly brackets thirty times.
{index = as.character(i) #we need index to be a character in the next step
regress = lm(Percent ~ Time, data = subset(vol_all, Animal == index)) #regression just for animal 1
summary(regress) 
coef <- summary(regress)$coef #creates a table of the output from the regression
R = summary(regress)$r.squared #puts the R2 value from the regression in variable “R”
index = as.numeric(index) #converts index back to a numeric variable

vol_all.data.sum[index,1] = index #writes the sample # (index) into the first column of the row corresponding to the sample.
vol_all.data.sum[index,2] = coef[2,1] #writes slope into the second column
vol_all.data.sum[index,3] = R #writes R squared into the third column
vol_all.data.sum[index,4] = coef[2,4] #writes the p value into the fourth column
vol_all.data.sum[index,5] = coef[2,2] #writes the slope into the fifth column
vol_all.data.sum[index,6] = vol_all[(index-1)*4+1, 4] #salinity
vol_all.data.sum[index,7] = vol_all[(index-1)*4+1, 2] #species
vol_all.data.sum[index,8] = vol_all[(index-1)*4+1, 1] #year 
}

#Write the regression summary into a .csv file
write.csv(vol_all.data.sum, "vol_allRegress.csv")

#VOLUME PLOTS----

#Subset data from regression summary data frame
#Remove slope outliers 
#Regression without outliers
good.regression=subset(vol_all.data.sum, slope<3)
good.regression$Salinity=as.factor(good.regression$Salinity)


#Plotting slope data by species and salinity treatment
Fig1 <- bwplot(slope ~ Salinity | Species, data = good.regression, xlab = "Salinity" , ylab = "Change in Volume")

Fig1


#OSMOLALITY PLOTS----

#BW plot showing the hemolymph osmolality values for spiders crabs pre and post acute treatment in 20% Seawater
Fig2 <- ggplot(spider.osmo.good, aes(x=Pre.Post, y=Osmo))+
  geom_boxplot()+
  ylab("Hemolymph Osmolality (mOsm)")+
  xlab("Post/Pre Treatment")+
  theme_bw()

Fig2

#BW plot showing hemolymph osmolality values for fiddler crabs subject to acute and chronic treatments in 20% and 100% ASW
Fig3 <- ggplot(uca_osmo, aes(x=Time, y=Osmolarity), fill=Salinity)+
  geom_boxplot()+
  xlab("Treatment (Acute/Chronic)")+
  ylab("Hemolymph Osmolality (mOsm)")+
  facet_wrap(~Salinity)+
  theme_bw()

Fig3

#STATISTICS----

#Figure 1 Statistics

#Favstats
favstats(slope~Salinity|Species, data=good.regression)
#Develop linear model for ANOVA
Fig1.lm = lm(slope~Salinity*Species, data=good.regression)
#ANOVA
anova(Fig1.lm)

#Interaction plot
stripplot(slope~Salinity, groups=Species, auto.key=TRUE, jitter.data=TRUE, type=c("p","a"), data=good.regression)

#Figure 2 Statistics

#Favstats
favstats(Osmo~Pre.Post, data = spider.osmo.good)
#Develop linear model for ANOVA
Fig2.lm = lm(Osmo~Pre.Post, data = spider.osmo.good)
#ANOVA
anova(Fig2.lm)

#Figure 3 Statistics

#Favstats
favstats(Osmolarity~Time|Salinity, data = uca_osmo)
#Develop linear model for ANOVA
Fig3.lm = lm(Osmolarity~Time*Salinity, data=uca_osmo)
#ANOVA
anova(Fig3.lm)

#BOTTOM----