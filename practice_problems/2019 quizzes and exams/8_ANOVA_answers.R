sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)
summary(sport[sport$Sex=="male", "Sport",])
sport_subset <- sport[sport$Sex=="male" & sport$Sport %in% c("BBall", "WPolo","T400m"),]
#clean up labels
library(plyr)
sport_subset$Sport <- revalue(sport_subset$Sport, c("BBall" = "basketball",
                                                    "WPolo" = "Water polo",
                                                    "T400m" = "Track"))
#fit model
sport_BMI <- lm(BMI ~ Sport, sport_subset) 

#plot to check assumptions
par(mfrow = c(2,2))
plot(sport_BMI) #looks ok
summary(sport_BMI)
library(car)
Anova(sport_BMI, type = "III")

#multiple comparisons
library(multcomp)
bmi_comparison <- glht(sport_BMI, linfct = mcp(Sport = "Tukey"))
summary(bmi_comparison)

#plot
library(Rmisc)
function_output <- summarySE(sport_subset, measurevar = "BMI", 
                             groupvars = c("Sport")) #close enough
library(ggplot2)

function_output$comparison <- "NA"
#enter by hand for small groups by comparing function_output means with multcomp 
#output (usually tukey)
function_output
summary(bmi_comparison)
#not all different, have to be careful
function_output$comparison <- c("a", "b", "a")
ggplot(function_output, aes(x=Sport, y=BMI, color = Sport)) +
  geom_point(aes(fill=Sport), size = 3) +
  geom_errorbar(aes(ymin=BMI-ci, ymax=BMI+ci), size=1.5) +
  ylab("BMI")+ggtitle("BMI of different male Australian Athletes")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20, face = "italic"), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  #offset from mean by hand
  geom_text(aes(x = Sport, y = BMI + 2, label = comparison), size = 28, 
color = "black") +
  guides(fill = F, color = F)
