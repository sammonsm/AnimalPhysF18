#Top
#Exploration for Osmolarity experiment in Uca and Spider Crabs

#Libaries
library(ggplot2)
library(lattice)
library(mosaic)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)

#Import data
uca_osmo <- read.csv("./data/Uca_Osmolarity_2018.csv", na.strings = c("","NA"))
View(uca_osmo)
spider_osmo <- read.csv("./data/Spider_crab_osmolarity_2018.csv", na.strings = c("","NA"))
View(spider_osmo)


#New column with percent change
spider_osmo$percent_change <- (spider_osmo$Post_Osm - spider_osmo$Pre_Osm)/spider_osmo$Pre_Osm*100

#Spider Transformation (but not like Peter Parker)
#working data frame
working.spider.vars <- c("Group", "Pre_Osm", "Post_Osm")
working.spider.osmo <- spider_osmo[working.spider.vars]
spider.osmo.good <- working.spider.osmo %>% gather(Pre.Post, Osmo, Pre_Osm:Post_Osm)
View(spider.osmo.good)

#Spider_Osmo_BW produces a BW plot showing the osmolarity values for spiders crabs pre and post treatment in 20% Seawater
Spider_Osmo_BW <- ggplot(spider.osmo.good, aes(x=Pre.Post, y=Osmo))+
  geom_boxplot()+
  ylab("Osmolarity")+
  xlab("Post/Pre Treatment")+
  theme_bw()

Spider_Osmo_BW

#ANOVA
spider.osmo.lm = lm(Osmo~Pre.Post, data=spider.osmo.good)
anova(spider.osmo.lm)

#Stats
fav_stats(spider_osmo$Pre_Osm)
fav_stats(spider_osmo$Post_Osm)
