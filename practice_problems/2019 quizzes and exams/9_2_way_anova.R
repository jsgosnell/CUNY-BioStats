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

moths <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/moths.csv")
str(moths)

#graph
#bar chart with error bars ####
library(Rmisc)
function_output <- summarySE(moths, measurevar = "Moths", groupvars = 
                               c("Location", "Lure")) 

library(ggplot2)
ggplot(function_output, aes(x=Location, y=Moths,color=Lure, 
                                   shape = Lure)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin=Moths-ci, ymax=Moths+ci), size=1.5) + 
  geom_line(aes(group=Lure, linetype = Lure), size=2) +
  ylab("Captured Moths")+ 
  xlab("Location") + 
  ggtitle("Location and lure impact trap success")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#or a bar chart

ggplot(function_output, aes(x=Location, y=Moths,fill=Lure, 
                            shape = Lure)) +
  geom_col(position = "dodge", size = 4, width = .5)  +
  geom_errorbar(aes(ymin=Moths-ci, ymax=Moths+ci), size=1.5, position = position_dodge(.5)) + 
  geom_line(aes(group=Lure, linetype = Lure), size=2) +
  ylab("Captured Moths")+ 
  xlab("Location") + 
  ggtitle("Location and lure impact trap success")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#build model
trap_model <- lm(Moths ~ Lure*Location, moths)
#check assumptions
par(mfrow =c(2,2))
plot(trap_model)
library(car)
Anova(trap_model, type = "III")

#drop interaction
trap_model <- lm(Moths ~ Lure+Location, moths)
plot(trap_model)
Anova(trap_model, type = "III")

library(multcomp)
moths_comparison <- glht(trap_model, linfct = mcp(Location = "Tukey"))
summary(moths_comparison)