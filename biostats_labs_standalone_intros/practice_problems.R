#lifespan vs gestation####
#
require(ggplot2)
sleep <- read.csv("http://www.jstephengosnell.com/teaching-resources/datasets/sleep.csv?attredirects=0&d=1")
sleep$Predation <- as.factor(sleep$Predation)

#removing outliers in data call
ggplot(sleep[sleep$Predation == 2 & is.na(sleep$Predation) == F 
             & sleep$LifeSpan < 20 & is.na(sleep$LifeSpan)==F,], 
       aes_string(x="LifeSpan", y = "Gestation")) +
  geom_point(size = 4) +
  #below here is ylabel, xlabel, and main title
  xlab("Life span (years)") +
  ylab("Gestation (days)") +
  ggtitle("Gestation time increases with life span") +
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


#sleep vs danger####
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

sleep <- read.csv("http://www.jstephengosnell.com/teaching-resources/datasets/sleep.csv?attredirects=0&d=1")

sleep$Danger <- as.factor(sleep$Danger)
sleep_by_risk <- summarySE(sleep, measurevar = "TotalSleep", groupvars = "Danger", na.rm = T)
#look at it
sleep_by_risk
require(ggplot2)
ggplot(sleep_by_risk
       , aes_string(x="Danger", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1.5) +
  ylab("Total sleep (hours per day")+ggtitle("Sleep across different risk levels")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#voter preference####
#HO: proportion of democratic voters is equal to .46
#HA: proportion of democratic voters is not equal to .46
binom.test(11,20, .46)
#pvalue is .5032
#given this data, we fail to reject HO.  there is not enough data to suggest the 
#proportion of democratic voters differs from 46%
require(binom)
binom.confint(11,20,.46)
#the 95% confidence interval for voter preferences is [.482, .616]
#note in this instance estimation methods lead to different conclusions than
#hypothesis testing


#trilling and distance####
##HO: proportion of chipmunks trilling does not depend on distance
#HA: proportion of chipmunks trilling does depend on distance
chisq.test(matrix(c(16,8,3,18), nrow = 2, byrow = T))
chisq.test(matrix(c(16,8,3,18), nrow=2, byrow = T))$expected
fisher.test(matrix(c(16,8,3,18), nrow=2, byrow = T))
#pvalue is .004541
##given this data, we reject HO.  there is enough data to suggest the 
#proportion of chipmunks trilling does depend on distance.
#
#Australian male athlete weight####
sport <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/sport.csv?attredirects=0&d=1")
##HO: average weight of males training at the facility is no different than average weight of australian male
#HA: average weight of males training at the facility is different than average weight of australian male
##normal based method
t.test(sport[sport$Sex == "male", "Wt"], mu = 85.9)
#p value is .007089
#given this data, we reject HO.there is enough data to suggest that 
#average weight of males training at the facility is different than average weight of australian male
##bootstrap method (no assumptions)
require(simpleboot)
bootstrapjsg=function(data1, data2=NULL, conf=.95, fun=mean, r=10000, null=0)
{
  if (is.null(data2)){
    a=one.boot(na.omit(data1), fun, r)
    confint=boot.ci(a, conf)
    p=length(subset(a$t, abs(a$t-mean(a$t))>=abs(null-mean(a$t))))/r
    output=c(conf, "% Confidence Interval", confint$percent[1,4:5], "p-value", p)
    return(output)}
  if (is.null(data2)==F){
    a=two.boot(na.omit(data1), na.omit(data2), fun, r)
    confint=boot.ci(a, conf)
    p=length(subset(a$t, abs(a$t-mean(a$t))>=abs(null-mean(a$t))))/r
    output=c(conf, "% Percentile Confidence Interval", confint$percent[1,4:5], "p-value", p)
    return(output)}
}
bootstrapjsg(data1=sport[sport$Sex == "male", "Wt"], null=85.9)
#p value is .0054
#given this data, we reject HO.there is enough data to suggest that 
#average weight of males training at the facility is different than average weight of australian male

#Discoveries from 1860 to 1959: plot####
View(discoveries)
#create a new data frame with the year and the corresponding number of discoveries
discovery_data <- data.frame(year=1860:1959, discoveries=discoveries)
#using ggplot
require(ggplot2)
ggplot(discovery_data, aes_string(x = "year", y="discoveries")) +
  geom_point(size = 2) +
  ylab("Number of discoveries")+
  xlab("Year") +
  ggtitle("Major scientific discoveries between 1860 and 1909")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#Discoveries from 1860 to 1959: linear model####
#Ho: no relationship between year and number of discoveries
#Ha: relationship exists between year and number of discoveries
discovery_data <- data.frame(year=1860:1959, discoveries=discoveries)
discovery_linear_model <- lm(discoveries ~ year, discovery_data)
#check assumptions
par(mfrow = c(2,2))
plot(discovery_linear_model)
require(car)
Anova(discovery_linear_model, type = "III")
#significant, use summary to determine trend and r2
summary(discovery_linear_model)
#reject Ho, discoveries appear to decrease over time but year only explains about 
#4% of variance in model

#Sleep Data####
#H0: group does not impact sleep (mean "extra" is same between two groups)
#Ha: group does  impact sleep (mean "extra" is different between two groups)
#understand what the variables in the sleep data are
?sleep
#isolate 'extra' from group 1 and group 2 into separate variables
group1 <- subset(sleep$extra, sleep$group == "1")
group2 <- subset(sleep$extra, sleep$group == "2")
##the default of the t.test function in R performs an unpaired t-test
#make sure to run this as a PAIRED t.test 
t.test(group1, group2, paired=TRUE)
#or you can use formula since these are in the same order
t.test(extra ~ group, sleep, paired = T)
#or you can block!
sleep_blocking <- lm(extra ~ group + factor(ID), sleep)
#check assumptions
par(mfrow = c(2,2))
plot(sleep_blocking)
require(car)
Anova(sleep_blocking, type = "III")
##p value is 0.002833
##therefore, reject H0
##the 10 patients showed a significant difference in the increase hours of sleep between the two different drugs
##if this ran as an unpaired t-test, the results would have showed no significance


#Weights####
#HO:there is no difference in mean weights among the groups
#HA: there is a difference in mean weights among the groups
#enter data 
female <- c(155, 128, 148, 146, 145, 106, 168, 134, 167, 102, 122, 124, 116, 166, 137)
male <- c(184, 155, 195, 219, 165, 151, 159, 213, 217, 188, 168, 190, 178, 214, 209)
#perform an unpaired t-test for the weight of females and males
t.test(female, male, paired=FALSE) #don't need to put in paired=FALSE
##p-value is 2.011e-06
##The weights of 15 females is statistically different from the weights of 15 males.
##Therefore, I would reject the null hypothesis. 


##Distance v. Speed####
#cars is a data frame with 50 observations on 2 variables, speed and distance
?cars
#construct scatterplot of cars to see data
plot(cars)
#perform linear regression between distance and speed
#save regression to L1
L1 <- lm(dist ~ speed, data=cars)
#check assumptions
par(mfrow = c(2,2))
plot(L1)
require(car)
Anova(L1, type = "III")
#summarizes the linear regression model to see slope and r2
summary(L1)
##the Pr(>|F|) of speed shows significance with the triple asterisks, p-value is also 1.49e-12
##there is a positive linear relationship between distance and speed. the model
##explains ~ 65 of the variance in the data
#add fitted line to the plot
ggplot(cars, aes_string(x="speed", y="dist")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  ylab("Distance required to stop") + 
  xlab("Initial speed") +
  ggtitle("Faster cars require more space to stop")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#Orange####
#HO: there is no relationship between circumference and age
#Ha: there is a relationship between circumference and age
#take a look at the data
View(Orange)
#fit data into a linear model
#save run to org
org <-lm(age ~ circumference, data=Orange)
#check model
par(mfrow = c(2,2))
plot(org)
#check for relationship
Anova(org, type = "III")
#summarizes the computed results for slope and r2
summary(org)
##the Pr(>|F|) of circumference shows significance with the triple asterisks, 
##p-value is 1.93e-14. Therefore we reject the null hypothesis.
##there is positive linear relationship between the age and the cirumference of 
##orange trees.  Circumference explains ~ 83% of the variation in age.


#Insect Sprays####
#HO: There is no difference in mean count among the sprays
#HO: There is a difference in mean count among the sprays
#take a look at the InsectSprays dataset 
View(InsectSprays)
#perform anova test between insect count and type of spray
fm1 <- lm(count ~ spray, data = InsectSprays)
#check assumptions
par(mfrow = c(2,2))
plot(fm1)
#check for significance
Anova(fm1, type = "III")
#p < .01, so we reject HO. We now look for which sprays differ from others using
#post-hoc tests
require(multcomp)
post_hoc_spray <- glht(fm1, linfct = mcp(spray = "Tukey"))
summary(post_hoc_spray)
# all comparisons are significant except:  A-B, A-F, B-E, D-C, E-C, E-D
# to plot
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

function_output <- summarySE(InsectSprays, measurevar="count", groupvars =
                               c("spray"))
#could add in blank row for character, look at design, and add manually...
#but hard for this many groups!
#or use cld command to get all comparisons in compact letter display (only for tukey)
post_hoc_spray_cld <- cld(post_hoc_spray)
post_hoc_spray_cld_fortify <- fortify(post_hoc_spray_cld)
#rename lhs to group
names(post_hoc_spray_cld_fortify)[names(post_hoc_spray_cld_fortify) == "lhs"] <- "spray"
function_output <- merge(function_output, post_hoc_spray_cld_fortify)

ggplot(function_output, aes_string(x="spray", y="mean")) +
  geom_col(aes_string(fill="spray"), size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Number of dead insects")+
  xlab("Spray") +
  ggtitle("Insect mortality differs among sprays")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_text(aes(x = spray, y = mean + 6, label = letters), size = 28)
