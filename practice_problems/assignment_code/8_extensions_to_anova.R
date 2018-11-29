#1####
##angry moods example #
angry <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/angry_moods.csv")
head(angry)
angry$Gender <- as.factor(angry$Gender)
angry$Sports <- as.factor(angry$Sports)

angry_gender <- lm(Anger_Expression ~ Sports * Gender, angry)
plot(angry_gender)
require(car)
Anova(angry_gender, type = "III")

#remove interaction since not significant
angry_gender <- lm(Anger_Expression ~ Sports + Gender, angry)
plot(angry_gender)
Anova(angry_gender, type = "III") #only differs among those who play sports

#2####
#
##what if we care about other factor
pulse <- read.table("http://www.statsci.org/data/oz/ms212.txt", header = T)
pulse$change <- pulse$Pulse2 - pulse$Pulse1
pulse$Exercise <-as.factor(pulse$Exercise)
pulse$Gender <- as.factor(pulse$Gender)

exercise <- lm(change ~ Gender * Exercise, pulse[pulse$Ran == 1, ])
summary(exercise)
Anova(exercise, type = "III")

#rerun without interaction
exercise <- lm(change ~ Gender + Exercise, pulse[pulse$Ran == 1, ])
summary(exercise)
Anova(exercise, type = "III") #no significance

#3####
pulse$Ran <- as.factor(pulse$Ran)
levels(pulse$Ran) <- c("ran", "sat")
running <- lm(Pulse2 ~ Gender * Ran, pulse)
Anova(running, type = "III") 

#consider without interaction
running <- lm(Pulse2 ~ Gender + Ran, pulse)
Anova(running, type = "III")  #running mattered
summary(running) #running increased pulse
