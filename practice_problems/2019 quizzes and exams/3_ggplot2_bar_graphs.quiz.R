#quiz

sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
require(plyr)
sleep$Primate <- revalue (sleep$Primate, c("Y" = "Yes", "N" = "No"))

#use summarySE fromm Rmisc
library(Rmisc)
brainwt_by_primate<- summarySE(sleep, measurevar = "BrainWt", groupvars = "Primate", na.rm = T)
#look at it
brainwt_by_primate
require(ggplot2)
ggplot(brainwt_by_primate
       , aes_string(x="Primate", y="BrainWt")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=BrainWt-ci, ymax=BrainWt+ci), size=1.5) +
  ylab("Brain Weight(g)")+ggtitle("Brain weight is not higher in primates")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


