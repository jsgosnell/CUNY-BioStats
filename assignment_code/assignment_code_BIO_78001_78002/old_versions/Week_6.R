#1####

fertilizer <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/fertilizer.csv")
t.test(height ~ fertilizer, fertilizer)
fert_lm <- lm(height ~ fertilizer, fertilizer)
plot(fert_lm)
summary(fert_lm)#difference is due to approximatin
require(car)
Anova(fert_lm, type = "III")

#2####
cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$day <- as.factor(cholesterol$day)
head(cholesterol)
cholest <- lm(cholest ~ day, cholesterol)
plot(cholest)
Anova(cholest, type = "III")
summary(cholest)
comp_cholest <- glht(cholest, linfct = mcp(day = "Tukey"))
summary(comp_cholest)

#3####
cane <- read.table("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/cane.txt", header = T)

summary(cane)

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

function_output <- summarySE(cane, measurevar="Tonn.Hect", groupvars =
                               c("DistrictPosition"))

require(ggplot2)
ggplot(function_output, aes_string(x="DistrictPosition", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

impact_district <- lm(Tonn.Hect ~ DistrictPosition, cane)
summary(impact_district)
plot(impact_district)#not really normal...lets bootstrap
require(WRS2)
t1waybt(Tonn.Hect ~ DistrictPosition, cane)
p <- mcppb20(Sepal.Length~Species, iris)
p.adjust(as.numeric(p$comp[,6]), "holm")

require(car)
Anova(impact_district, type = "III")
require(multcomp)
comp_district <- glht(impact_district, linfct = mcp(DistrictPosition = "Tukey"))
summary(comp_district)

#4.fev####
fev <- read.table("http://www.statsci.org/data/general/fev.txt", header = T)
fev_gender <- lm(FEV ~ Sex, fev)
plot(fev_gender) #anova is fine
summary(fev_gender)


#5.exercise####
pulse <- read.table("http://www.statsci.org/data/oz/ms212.txt", header = T)
initial <- lm(Pulse1 ~ Exercise, pulse)
plot(initial)
summary(initial)
Anova(initial, type = "III")

#6.exercise difference ####
pulse$change <- pulse$Pulse2 - pulse$Pulse1
pulse$Exercise <-as.factor(pulse$Exercise)
levels(pulse$Exercise)
change <- lm(change ~ Exercise, pulse[pulse$Ran == 1, ])
plot(change)
summary(change)

#7####
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

#8####
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

#9####
pulse$Ran <- as.factor(pulse$Ran)
levels(pulse$Ran) <- c("ran", "sat")
running <- lm(Pulse2 ~ Gender * Ran, pulse)
Anova(running, type = "III") 

#consider without interaction
running <- lm(Pulse2 ~ Gender + Ran, pulse)
Anova(running, type = "III")  #running mattered
summary(running) #running increased pulse




#10####
#
sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
require(plyr)
sleep$Taxa <- revalue(sleep$Primate, c(Y = "Primate", N = "Non-primate"))
sleep$Taxa <- relevel(sleep$Taxa, "Primate" )

require(ggplot2)

#11####

ggplot(sleep[sleep$BrainWt <1000, ], aes_string(x="BrainWt", y = "TotalSleep")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent sleeping daily") +
  xlab("Braing weight (g)") +
  ggtitle("Time spent sleeping decreases with brain weight") +
  #scale commands help with legends
  scale_colour_manual(name="Type of mammal",values = c("#FFA373","#50486D")) +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(size=3),
        panel.grid.major = element_line(colour = "black"),
        plot.background = element_rect(fill="gray"),
        legend.background = element_rect(fill="gray"))

#12a####
ggplot(sleep
       , aes_string(x="TotalSleep")) +
  geom_histogram() +
  xlab("Total sleep (hours per day")+ggtitle("Variation in sleep levels")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
#12b####

ggplot(sleep
       , aes_string(x="TotalSleep")) +
  geom_histogram() +
  xlab("Total sleep (hours per day")+ggtitle("Variation in sleep levels")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+ 
  facet_wrap(~Taxa)

#12c####

ggplot(sleep[sleep$Taxa == "Primate",]
       , aes_string(x="TotalSleep")) +
  geom_histogram() +
  xlab("Total sleep (hours per day")+ggtitle("Variation in sleep levels")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#13a####
#requires summarySE function
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

sleep_by_primate <- summarySE(sleep, measurevar = "TotalSleep", groupvars = "Taxa", na.rm = T)
#look at it
sleep_by_primate
require(ggplot2)
ggplot(sleep_by_primate
       , aes_string(x="Taxa", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Total sleep (hours per day")+ 
  xlab("Primate?")+ 
  ggtitle("Sleep across different taxa")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) 

#13b####
sleep$Predation <- as.factor(sleep$Predation)
sleep_by_predation <- summarySE(sleep, measurevar = "TotalSleep", 
                                groupvars = "Predation", na.rm = T)
#look at it
sleep_by_predation
require(ggplot2)
ggplot(sleep_by_predation
       , aes_string(x="Predation", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1.5) +
  ylab("Total sleep (hours per day") + 
  ggtitle("Sleep across different predation levels")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#14####
#rename some levels
#have to use quotes around numbers (another reason this is bad idea to start with)
angry$Sports <- revalue(angry$Sports, c( "1" = "Athlete", "2" = "Not athlete"))
angry$Sports <- relevel(angry$Sports, "Not athlete")
angry$Gender <- revalue (angry$Gender, c("1" = "Males", "2" = "Females"))
anger_by_gender_and_athlete <- summarySE(angry, measurevar = "Anger_Expression", 
                                       groupvars = c("Sports", "Gender"), na.rm = T)
#look at it
anger_by_gender_and_athlete
ggplot(anger_by_gender_and_athlete
       , aes_string(x="Sports", y="mean",color="Gender", 
                    shape = "Gender")) +
  geom_point(size = 5) +
  geom_line(aes_string(group="Gender", linetype = "Gender"), size=2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Anger")+
  ggtitle("Athletes show less aggression across genders")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#or

ggplot(anger_by_gender_and_athlete
       , aes_string(x="Sports", y="mean",color="Gender", 
                    shape = "Gender")) +
  geom_bar(aes_string(fill="Gender"), size = 3, stat = "identity", 
           position = position_dodge(.5), width = .5) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),  
                position = position_dodge(0.5), size=1.5, color = "black") +
  ylab("Anger")+
  ggtitle("Athletes show less aggression across genders")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


