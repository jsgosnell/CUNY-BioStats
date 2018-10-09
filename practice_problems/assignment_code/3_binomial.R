#1a####
#HO: proportion of right-eared people is equal to .5
#HA: proportion of right-eared people is note equal to .5
#1b####
19/25 #sample proportion

#1ci####
sampling_experiment = rbinom(10000, 25, .5)
hist(sampling_experiment, breaks = 0:25, xlab = "# of Right-eared people out of 25", ylab = "Probability of being drawn \n from population of p = 0.5", cex.main = 2, cex.axis = 1.5, cex.lab = 2)

#1cii####
#using each point
one_by_one <- data.frame(number_observed = 0:25)
one_by_one$prob <- dbinom(one_by_one$number_observed,25,.5)
plot(prob~number_observed, one_by_one)

#understand what rbinom, pbinom, dbinom, and qbinom do
using_distribution = dbinom(0:25,25,.5)
using_distribution
sum(using_distribution)
Number_righteared = c(0:25)
pdf = data.frame(Number_righteared, using_distribution)
plot(0:25, using_distribution)

#1di####
#using previous sampling experiment
length(sampling_experiment[sampling_experiment >= 19 | sampling_experiment <= 6])/length(sampling_experiment)
#1dii####
(1-pbinom(18,25,.5)) * 2
#1diii####
binom.test(19,25, p=.5)

#1e####
#confidence interval
require(binom)
binom.confint(x=19, n=25, alpha=.05, method="all")

#1f####
#confidence interval does not include .5 (value under null hypothesis), and p value is less than .05 (reject the null)

#2####
#use sided test as you only care if students helped the dog
binom.test(33,100, alternative="greater", p=.25)

#4####
#source chivers script, then 
monty(strat="stay")
monty(strat="switch")
monty(strat="random")

#6####
#uninformed
triplot(prior = c(1,1), data = c(14,4), where = "topleft")
#prior assumes left handed
triplot(prior = c(5,20), data = c(14,4), where = "topleft")
#prior assumes right handed 
triplot(prior = c(20,5), data = c(14,4), where = "topleft")
#less sure right handed
triplot(prior = c(4,2), data = c(14,4), where = "topleft")

#extra for 78001/78002####


sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")

#1####

require(plyr)
sleep$Taxa <- revalue(sleep$Primate, c(Y = "Primate", N = "Non-primate"))
sleep$Taxa <- relevel(sleep$Taxa, "Primate" )

require(ggplot2)

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

#2a####
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
#2b####

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

#2c####

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

#3a####
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

#3b####
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

#4####
# a 95% confidence interval means if you construct this interval an infinite #
#  of times (based on repeated sampling) that 95% of the intervals you
#   construct would contain the true paramter of interest











