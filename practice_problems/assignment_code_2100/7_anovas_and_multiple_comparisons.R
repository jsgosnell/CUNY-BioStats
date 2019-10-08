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
