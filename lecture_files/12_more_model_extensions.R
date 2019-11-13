#code for gamma distribution####
#death data from crawley
deaths <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/deaths.csv")
str(deaths)
library(Rmisc)
summarySE(deaths, measurevar = "death", groupvars = "treatment")
#graph it####
library(ggplot2)
ggplot(summarySE(deaths, measurevar = "death", groupvars = "treatment"), 
       aes ( x= treatment, y = death)) +
  geom_col() +
  geom_errorbar(aes(ymin = death - ci, ymax = death + ci))

#wrong order!
deaths$treatment <- factor(deaths$treatment, c("control", "low", "high"))

sizes <- factor(sizes, levels = c("small", "medium", "large"))



#
#lm approach#####
