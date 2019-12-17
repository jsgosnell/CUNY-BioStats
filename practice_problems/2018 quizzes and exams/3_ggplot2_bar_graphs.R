#quiz

sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
library(plyr)
sleep$Primate <- revalue (sleep$Primate, c("Y" = "Yes", "N" = "No"))
library(Rmisc)
gestation_by_primate<- summarySE(sleep, measurevar = "Gestation", groupvars = "Primate", na.rm = T)
#look at it
gestation_by_primate
library(ggplot2)
ggplot(gestation_by_primate
       , aes(x=Primate, y=Gestation)) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=Gestation-ci, ymax=Gestation+ci), size=1.5) +
  ylab("Total gestation time (days)")+ggtitle("Primates have longer gestation periods")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))



