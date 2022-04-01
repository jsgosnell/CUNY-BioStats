#1#####
binom.test(6,38, .33)
library(binom)
binom.confint(6,38, .33)

#3####
temp <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQx69c3IWKOnVQKaI841yapNV3A87iQgCaHR0-QzOuvmjKoud5wbNCN6cdcyU6caOKJR573j_RbLZ3q/pub?gid=73875089&single=true&output=csv")
t.test(temp$Temperature_2009, temp$Temperature_2019, paired = T)
#NOT
t.test(temp$Temperature_2009, temp$Temperature_2019)

#4####
# growth <-data.frame(Stage = c(rep("hatchling", 75), rep("juvenile", 75), rep("adult", 75)), 
#                     Average_May_temperature = c(runif(n=75*3, min = 60, max = 80)))
# growth$Mass_added_kg <- c(growth$Average_May_temperature[1:75]*.2 + rnorm(n=75,mean=0, sd = 1),
#                                       rnorm(n=75*2, mean = 8.5, sd = 1))
#write.csv(growth, "growth.csv", row.names = F)

growth <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQx69c3IWKOnVQKaI841yapNV3A87iQgCaHR0-QzOuvmjKoud5wbNCN6cdcyU6caOKJR573j_RbLZ3q/pub?gid=744682552&single=true&output=csv")
growth$Stage <- factor(growth$Stage, levels = c("hatchling", "juvenile", "adult"))
require(Rmisc)
growth_summary <- summarySE(growth, measurevar = "Mass_added_kg", groupvars = "Stage")

require(ggplot2)
ggplot(growth_summary, aes(x=Stage, y=Mass_added_kg, color = Stage)) +
  geom_point(aes(fill=Stage), size = 3) +
  geom_errorbar(aes(ymin=Mass_added_kg-ci, ymax=Mass_added_kg+ci), size=1.5) +
  ylab("Mass added (kg)")+ggtitle("Mass added by different stages of eagles in 2019")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20, face = "italic"), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#or
ggplot(growth_summary, aes(x=Stage, y=Mass_added_kg)) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=Mass_added_kg-ci, ymax=Mass_added_kg+ci), size=1.5) +
  ylab("Mass added (kg)")+ggtitle("Mass added by different stages of eagles in 2019")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20, face = "italic"), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
#or
ggplot(growth, aes(x=Stage, y=Mass_added_kg)) +
  geom_point(size = 3) +
  ylab("Mass added (kg)")+ggtitle("Mass added by different stages of eagles in 2019")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20, face = "italic"), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
#or
ggplot(growth, aes(x=Stage, y=Mass_added_kg)) +
  geom_boxplot(size = 3) +
  ylab("Mass added (kg)")+ggtitle("Mass added by different stages of eagles in 2019")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20, face = "italic"), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
#or
ggplot(growth, aes(x=Mass_added_kg, fill=Stage)) +
  geom_histogram(size = 3) +
  ylab("Mass added (kg)")+ggtitle("Mass added by different stages of eagles in 2019")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20, face = "italic"), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  facet_wrap(~Stage, ncol=1)



#5####
fit_growth <- lm(Mass_added_kg~Stage, growth)
par(mfrow=c(2,2))
plot(fit_growth)
library(car)
Anova(fit_growth, type = "III")
library(multcomp)
summary(glht(fit_growth, linfct = mcp(Stage="Tukey")))

#6####
growth_temp <-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQx69c3IWKOnVQKaI841yapNV3A87iQgCaHR0-QzOuvmjKoud5wbNCN6cdcyU6caOKJR573j_RbLZ3q/pub?gid=1025785097&single=true&output=csv")
ggplot(growth_temp, aes(x=Average_May_temperature, y=Mass_added_kg, color = Stage,
                   shape = Stage)) +
  geom_point(size = 3) +
  ylab("Mass added (kg)")+
  xlab(expression(bold(paste("Average May temperature", ( degree~F))))) +  
#  ggtitle("Impact of temperature on eagle growth varies by stage")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  geom_smooth(method = "lm", se = F)

#7####
# size <- data.frame(chick_size = rnorm (8,8,2))
# size$adult_size <- size$chick_size*4 +rnorm(8,8,12)
# write.csv(size, "size.csv", row.names = F)

size <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQx69c3IWKOnVQKaI841yapNV3A87iQgCaHR0-QzOuvmjKoud5wbNCN6cdcyU6caOKJR573j_RbLZ3q/pub?gid=194504518&single=true&output=csv")
size_fit <-lm(adult_size ~ chick_size, size)
par(mfrow = c(2,2))
plot(size_fit)
summary(size_fit)

#8#####
ggplot(size, aes(x=chick_size, y=adult_size)) +
  geom_point(size = 3) +
  ylab("Adult size (kg)")+
  xlab("Chick size (kg)")+
  #  ggtitle("Impact of temperature on eagle growth varies by stage")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  geom_smooth(method = "lm", se = F)

