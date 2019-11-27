#belows just done for data setup - students can start on line 15!
# poison <- read.table("http://www.statsci.org/data/general/poison.txt", header = T)
# summary(poison)
# poison$Poison <- as.factor(poison$Poison)
# poison$Treatment <- as.factor(poison$Treatment)
# 
# poison$Poison <- paste("poison", poison$Poison, sep = "_")
# poison$Treatment <- paste("treatment", poison$Treatment, sep = "_")
# poison$Time <- log(poison$Time)
# 
# write.csv(poison, "poison.csv")



poison <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/poison.csv")

#graph
#bar chart with error bars ####
library(Rmisc)
function_output <- summarySE(poison, measurevar = "Time", groupvars = 
                               c("Poison", "Treatment")) 

library(ggplot2)
ggplot(function_output, aes(x=Treatment, y=Time,color=Poison, 
                                   shape = Poison)) +
  geom_errorbar(aes(ymin=Time-ci, ymax=Time+ci), size=1.5) + #use width = to set change bar width
  geom_line(aes(group=Poison, linetype = Poison), size=2) +
  geom_point(size = 5) +
  #  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Survival Time")+ 
  xlab("Treatment") + 
  ggtitle("Poison type and treatment impact survival")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#build model
poison_model <- lm(Time ~ Poison*Treatment, poison)
#check assumptions
par(mfrow =c(2,2))
plot(poison_model)
library(car)
Anova(poison_model, type = "III")

#drop interaction
poison_model <- lm(Time ~ Poison + Treatment, poison)
Anova(poison_model, type = "III")

library(multcomp)
poison_comparison <- glht(poison_model, linfct = mcp(Poison = "Tukey"))
summary(poison_comparison)