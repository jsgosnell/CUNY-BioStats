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

