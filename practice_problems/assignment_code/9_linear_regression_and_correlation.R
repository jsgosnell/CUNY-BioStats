#1####
#
##what if we care about other factor
pulse <- read.table("http://www.statsci.org/data/oz/ms212.txt", header = T)
pulse$change <- pulse$Pulse2 - pulse$Pulse1
pulse$Exercise <-as.factor(pulse$Exercise)
pulse$Gender <- as.factor(pulse$Gender)

exercise <- lm(change ~ Age, pulse[pulse$Ran == 1, ])
par(mfrow =c (2,2))
plot(exercise)
require(car)
Anova(exercise, type = "III")
summary(exercise)

exercise <- lm(change ~ Weight, pulse[pulse$Ran == 1, ])
par(mfrow =c (2,2))
plot(exercise)
Anova(exercise, type = "III")
summary(exercise)

exercise <- lm(change ~ Height, pulse[pulse$Ran == 1, ])
par(mfrow =c (2,2))
plot(exercise)
Anova(exercise, type = "III")
summary(exercise)

#2####
height <- read.table("http://www.statsci.org/data/general/stature.txt", 
                     header = T)
head(height)
metacarp_relationship <- lm(Stature ~ MetaCarp, height)
plot(metacarp_relationship)
Anova(metacarp_relationship, type = "III")
summary(metacarp_relationship)

#3####
medals <- read.table(header = T, "http://www.statsci.org/data/oz/medals.txt")
head(medals)
medals$total <- medals$Summer + medals$Winter
population_medals <- lm(total ~ Population, medals)
plot(population_medals)
Anova(population_medals, type = "III")

#4####
#still using medals
summer_medals <- lm(Summer ~ Latitude, medals)
plot(summer_medals)
Anova(summer_medals, type = "III")
summary(summer_medals)

winter_medals <- lm(Winter ~ Latitude, medals)
plot(winter_medals)
Anova(winter_medals, type = "III")
summary(winter_medals)

#5####
#fev####
fev <- read.table("http://www.statsci.org/data/general/fev.txt", header = T)
head(fev)
fev_height <- lm(FEV ~ Height, fev)
plot(fev_height)
Anova(fev_height)
summary(fev_height)

fev_age <- lm(FEV ~ Age, fev)
plot(fev_age)
Anova(fev_age)
summary(fev_age)

#6####
require(ggplot2)
#age plot####
ggplot(fev, aes_string(x="Age", y="FEV")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  ylab("FEV")+ggtitle("FEV increases with age")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#height plot####
ggplot(fev, aes_string(x="Height", y="FEV")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  ylab("FEV")+ggtitle("FEV increases with height")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))



#gender plot ####

#bar chart with error bars ####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  #  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

function_output <- summarySE(fev, measurevar="FEV", groupvars =
                               c("Sex"))

ggplot(function_output, aes_string(x="Sex", y="mean")) +
  geom_col(size = 3) +
  ylab("FEV") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
