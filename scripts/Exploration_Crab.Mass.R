#Crab Mass Fun!

#Packages
library(ggplot2)

#Import data for crab mass
crab.mass<-read.csv("./data/crabmass.csv")
View(crab.mass)

#Plotting
ggplot(data=crab.mass, aes(x=Sex, y=Mass)) +
  geom_boxplot() +
  xlab("Sex") +
  ylab("Mass")


ggplot(crab.mass, aes(x=Sex, y=Mass, fill=Purple)) + 
  geom_boxplot() +
  ylab("Mass")+
  xlab("Sex") +
  scale_fill_discrete(name="Purple",)
                      breaks=c("Y", "N") +
                      scale_fill_manual(values=c("forestgreen","red") +
                      labels=c("Yes Purple Spot", "No Purple Spot"))
