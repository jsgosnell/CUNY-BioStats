#1####
##angry moods example #
angry <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/angry_moods.csv?attredirects=0&d=1")
head(angry)
angry$Gender <- as.factor(angry$Gender)
angry$Sports <- as.factor(angry$Sports)

angry_gender <- lm(Anger_Expression ~ Sports * Gender, angry)
plot(angry_gender)
require(car)
Anova(angry_gender, type = "III")

angry_gender <- lm(Anger_Expression ~ Sports + Gender, angry)
plot(angry_gender)
Anova(angry_gender, type = "III")

#run again without interaction
angry_gender <- lm(Anger_Expression ~ Sports + Gender, angry)
Anova(angry_gender, type = "III")


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
running <- lm(Pulse2 ~ Gender * Ran, pulse)
summary(running)
Anova(running, type = "III") 

#consider without interaction
running <- lm(Pulse2 ~ Gender + Ran, pulse)
summary(running)
Anova(running, type = "III")
