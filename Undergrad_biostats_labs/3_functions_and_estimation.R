#ESTIMATION, BARCHARTS, AND FUNCTIONS ####
#
#Estimating is a key part of statistics and should include the value you are 
#estimating and an estimate of uncertaintly. We'll tie this into statistical tests
#next.  

#Let's go back to the airquality dataset and estimate the average wind speed for each month.
#First, lets change months to factors and label them
str(airquality) #just a reminder
#for safety, lets make a date column before converting month to a factor. Dates are 
#annoying but sometimes you need them 
airquality$Date <- as.Date(paste(airquality$Month, airquality$Day, sep="/"), 
                           format ="%m/%d" )


airquality$Month <- factor(airquality$Month)
levels(airquality$Month) <- c("May","June", "July", "August", "September") 



#revalue gave me issues here with numbers going to characters
#
#Let's just focus on July first
mean(airquality[airquality$Month == "July", "Wind"])
#this is our estimate.  The standard deviation of this estimate is
sd(airquality[airquality$Month == "July", "Wind"]) / nrow(airquality[airquality$Month == "July",])
# is equivalent to
sd(airquality[airquality$Month == "July", "Wind"]) / length(airquality[airquality$Month == "July", "Wind"])
#we also call this the standard error of the mean. If we assume the estimate 
#(not the data!) is normally distributed, we can assume things about uncertainty.
#Namely, we can build a 95% confidence interval around our estimate (meaning the true mean
#is in the range 95 out of 100 times we create a sample).  
#
#These ranges need to be added to barcharts (or anything that shows estimates, which
#barcharts are not really great for) to show uncertainty.  We can do this usign 
#a user-defined function in R, as ggplot2 doesn't have it built in (maybe because
#bar charts are a bad idea?).
#
#FUNCTIONS####
#First, a quick note on functions.  You may never need to write one, but they aren't 
#hard, and you may find one in code you search for online.  In general you can write 
#a function using the function command, which reads in the arguments, and then
#shows what to do with them in curly brackets. For example, 

timestwo <- function (x) {
  x+x
}

#running above creates a function that you can then use...

timestwo(12)
timestwo(c(1,2,3,4))

#More complicated issues involve what the function returns and how it handles the
#arguments. You may see loops(which can also be handy to understand). Examples are given below:
#
#FOR LOOPS####

for(x in 1:10){
  print(x+1)
}
#1:10 gives the sequence that the variable cycles through. This could also be a list
#or something else

#WHILE LOOPS####
x<-1
while(x<10){
  print(x)
  x<-x+x
}
#while loops evaluate a statement instead of going through a list.
#
#We're discussing this because below is a function I'll provide for makign barcharts.
#If you run it once, it will be available throughout an R session
#
#summarySE function####
### Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
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

#BAR PLOTS####
#Now let's use this to get the confidence intervals for each month and plot them
monthly_wind_data <- summarySE(airquality, measurevar = "Wind", groupvars = "Month")
#look at it
monthly_wind_data

#now plot it in ggplot2

require(ggplot2)

ggplot(monthly_wind_data
, aes_string(x="Month", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Wind speed (mph)")+ggtitle("Wind speed over time")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
#or better
ggplot(monthly_wind_data
       , aes_string(x="Month", y="mean")) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Wind speed (mph)")+ggtitle("Wind speed over time")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
        ylim(c(0,13.2)) #watch for truncated axes!

#Below just shows why you might need dates
#What if you wanted to add a line between months?
ggplot(monthly_wind_data
       , aes_string(x="Month", y="mean")) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  geom_line(size = 2)+
  ylab("Wind speed (mph)")+ggtitle("Wind speed over time")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  ylim(c(0,13.2)) #watch for truncated axes!
#This doesn't work because you have factors for months (no logical relationship)
#
#Quick cheat is to just add group aesthetic
#Below just shows why you might need dates
#What if you wanted to add a line between months?
ggplot(monthly_wind_data
       , aes_string(x="Month", y="mean", group = 1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  geom_line(size = 2)+
  ylab("Wind speed (mph)")+ggtitle("Wind speed over time")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  ylim(c(0,13.2)) #watch for truncated axes!




  
  