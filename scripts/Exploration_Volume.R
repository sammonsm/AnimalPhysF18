#Exploration for Volume experiment in Nereis and Uca----

#Libaries
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)

#Import data
vol_all <- read.csv("./data/vol_all_all.csv", na.strings = c("","NA"))
View(vol_all)
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

View(vol_all)


#That new new code----
#plot time courses for each species
#note: par.settings requires that you’ve run a theme template
xyplot(Percent~Time, groups = Salinity, data = subset(vol_all, Species == "Uca"), auto.key = list(corner = c(0.05, 0.95)), jitter.x = T, )

#you may want to add: ylim = c(min,max) 
#to the graphing command where min and max represent the range for the y-axis in order to have the same y-axis for both species.

#run regress analyses and put onto plot


#remove outliers
g50=subset(vol_all, Percent<50 & Percent>-50)
#run these after doing a xyplot for Uca
Uca.50 = lm(Percent~Time, data = subset(vol_all, Species == "Uca" & Salinity == 500))
summary(Uca.50)
Uca.50.fun = makeFun(Uca.50)
plotFun(Uca.50.fun, add = T, col = "light blue", lwd = 2)


Uca.60 = lm(Percent~Time, data = subset(vol_all, Species == "Uca" & Salinity == 1000))
summary(Uca.60)
Uca.60.fun = makeFun(Uca.60)
plotFun(Uca.60.fun, add = T, col = "hot pink", lwd = 2)


Uca.70 = lm(Percent~Time, data = subset(vol_all, Species == "Uca" & Salinity == 1500))
summary(Uca.70)
Uca.70.fun = makeFun(Uca.70)
plotFun(Uca.70.fun, add = T, col = "green", lwd = 2)

#Repeat for Nereis


xyplot(Percent~Time, groups = Salinity, data = subset(g50, Species == "Nereis"), auto.key = list(corner = c(0.05, 0.95)), jitter.x = T, )
Nereis.50 = lm(Percent~Time, data = subset(g50, Species == "Nereis" & Salinity == 500))
summary(Nereis.50)
Nereis.50.fun = makeFun(Nereis.50)
plotFun(Nereis.50.fun, add = T, col = "purple", lwd = 2)


Nereis.60 = lm(Percent~Time, data = subset(g50, Species == "Nereis" & Salinity == 500))
summary(Nereis.60)
Nereis.60.fun = makeFun(Nereis.50)
plotFun(Nereis.60.fun, add = T, col = "blue", lwd = 2)


Nereis.70 = lm(Percent~Time, data = subset(g50, Species == "Nereis" & Salinity == 500))
summary(Nereis.70)
Nereis.70.fun = makeFun(Nereis.70)
plotFun(Nereis.50.fun, add = T, col = "green", lwd = 2)


#modeling data with all the variables.  
model.1 = lm(Percent ~ Time*Salinity*Species, data = g50)
anova(model.1)

#Makes sample and year into characters and creates a new empty dataset for the regression data
vol_all$Species = as.character(vol_all$Species) #needs to be a character
vol_all$Year = as.character(vol_all$Year) #needs to be a character
vol_all.data.sum=data.frame(sample = as.numeric(), slope = as.numeric(), 
                           R2 =as.numeric(), Pvalue = as.numeric(), 
                           SEslope = as.numeric(), Salinity = as.numeric(), 
                           Species = as.numeric(), Year = as.numeric())

#Species needs to be a character too in the new dataframe
vol_all.data.sum$Species = as.character(vol_all.data.sum$Species) 

#code like this would allow you to evaluate one animal at a time.  (Try for a couple, but don’t do all 102!)
regress = lm(Percent ~ Time, data = subset(vol_all, Animal == 2)) #where index is the animal number that you wish to analyze
summary(regress)
coef <- summary(regress)$coef
View(coef) #view coefficient table
R = summary(regress)$r.squared
R #view R-squared value

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



#write the regression summary into a .csv file
write.csv(vol_all.data.sum, "vol_allRegress.csv")


#subsetdata
#Remove slope outliers 
#regression without outliers
good.regression=subset(vol_all.data.sum, slope<3)

good.regression$Salinity=as.factor(good.regression$Salinity)

#plotting slope data.  
bwplot(slope ~ Salinity | Species, data = good.regression, xlab = "Salinity" , ylab = "Change in Volume")


xyplot(slope ~ Salinity, groups = Species, data = good.regression, auto.key = list(corner=c(0.95, 0.95)))

require(plyr) #need this package to easily change factor levels
levels(good.regression$Species) = c("Nereis", "Uca")  #changes factor levels

#Use “lm” to run statistical models on vol_all
#Currently this subsetting does not work :)

salty_subset <- subset(good.regression, Species= "Uca")
View(salty_subset)
salty=lm(slope ~ Salinity, data = salty_subset
anova(salty)

View(good.regression)
#Use “lm” to run statistical models as above
Nereis_lm=lm(slope ~ Salinity, data = subset(good.regression, Species= "Nereis"))
anova(Nereis_lm)
