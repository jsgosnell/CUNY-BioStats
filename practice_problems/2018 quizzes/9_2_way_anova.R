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

#use summarySE to check for variance if wanted

function_output <- summarySE(poison, measurevar = "Time", groupvars = 
                               c("Poison", "Treatment")) 

require(ggplot2)
ggplot(function_output, aes_string(x="Treatment", y="mean",color="Poison", 
                                   shape = "Poison")) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) + #use width = to set change bar width
  geom_line(aes_string(group="Poison", linetype = "Poison"), size=2) +
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