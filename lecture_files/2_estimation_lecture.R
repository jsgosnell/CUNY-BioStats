#plots from 2. Estimation and probability.ppt


#create "population" of males####

population_size <- 10000
set.seed(42)
population_norm <- data.frame(id = 1:population_size, 
                         height = rnorm(population_size, 70, 3))

require(ggplot2)

ggplot(population_norm, aes_string("height")) + 
  geom_histogram(size=3) +
  xlab("Height (in)")+
  ylab("Frequency")+
  ggtitle("Height of all males in our fake population")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#sample a 100 of these####

sample_1 <- population_norm[sample(nrow(population_norm), 100),]
ggplot(sample_1, aes_string("height")) + 
  geom_histogram(size=3) +
  xlab("Height (in)")+
  ylab("Frequency")+
  ggtitle("Height of 100 random males in our fake population")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
mean(sample_1$height)
sd(sample_1$height)

#now do this a 1000 times and record the mean of each time#####
number_of_samples <- 1000
sample_outcomes <- data.frame(mean = rep(NA, number_of_samples), sd = NA)

for (i in 1:number_of_samples){
  sample_1 <- population_norm[sample(nrow(population_norm), 100),]
  sample_outcomes$mean[i] <- mean(sample_1$height)
  sample_outcomes$sd[i] <- sd(sample_1$height)
  
}

ggplot(sample_outcomes, aes_string("mean")) + 
  geom_histogram(size=3) +
  xlab("Mean height (in)")+
  ylab("Frequency")+
  ggtitle("Mean height of 100 males in our samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
mean(sample_outcomes$mean)
sd(sample_outcomes$mean)

#what if we only sampled 20####
##now do this a 1000 times and record it
number_of_samples <- 1000
sample_outcomes <- data.frame(mean = rep(NA, number_of_samples), sd = NA)

for (i in 1:number_of_samples){
  sample_1 <- population_norm[sample(nrow(population_norm), 20),]
  sample_outcomes$mean[i] <- mean(sample_1$height)
  sample_outcomes$sd[i] <- sd(sample_1$height)
  
}

ggplot(sample_outcomes, aes_string("mean")) + 
  geom_histogram(size=3) +
  xlab("Mean height (in)")+
  ylab("Frequency")+
  ggtitle("Mean height of 20 males in our samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
mean(sample_outcomes$mean)
sd(sample_outcomes$mean)

#what if we only sampled 5####
##now do this a 1000 times and record it
number_of_samples <- 1000
sample_outcomes <- data.frame(mean = rep(NA, number_of_samples), sd = NA)

for (i in 1:number_of_samples){
  sample_1 <- population_norm[sample(nrow(population_norm), 5),]
  sample_outcomes$mean[i] <- mean(sample_1$height)
  sample_outcomes$sd[i] <- sd(sample_1$height)
  
}

ggplot(sample_outcomes, aes_string("mean")) + 
  geom_histogram(size=3) +
  xlab("Mean height (in)")+
  ylab("Frequency")+
  ggtitle("Mean height of 5 males in our samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
mean(sample_outcomes$mean)
sd(sample_outcomes$mean)

#what if underlying data not normal?

#create "population" of males from uniform group ####

population_size <- 10000
set.seed(42)
population_unif <- data.frame(id = 1:population_size, 
                         height = runif(population_size, 60, 80))

ggplot(population_unif, aes_string("height")) + 
  geom_histogram(size=3) +
  xlab("Height (in)")+
  ylab("Frequency")+
  ggtitle("Height of all males in our fake uniform population")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#sample a 100 of these####

sample_unif_1 <- population_unif[sample(nrow(population_unif), 100),]
ggplot(sample_unif_1, aes_string("height")) + 
  geom_histogram(size=3) +
  xlab("Height (in)")+
  ylab("Frequency")+
  ggtitle("Height of 100 random males in our fake uniform population")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#now do this a 1000 times and record the mean each time ####
number_of_samples <- 1000
sample_outcomes <- data.frame(mean = rep(NA, number_of_samples), sd = NA)

for (i in 1:number_of_samples){
  sample_unif_1 <- population_unif[sample(nrow(population_unif), 100),]
  sample_outcomes$mean[i] <- mean(sample_unif_1$height)
  sample_outcomes$sd[i] <- sd(sample_unif_1$height)
  
}

ggplot(sample_outcomes, aes_string("mean")) + 
  geom_histogram(size=3) +
  xlab("Mean height (in)")+
  ylab("Frequency")+
  ggtitle("Mean height of 100 males in our uniform population samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#error bars in practice####
#back to our normal population of males
#using 2 se as rule of thumb
number_of_samples <- 20
sample_outcomes <- data.frame(mean = rep(NA, number_of_samples), sd = NA, se = NA)

for (i in 1:number_of_samples){
  sample_1 <- population_norm[sample(nrow(population_norm), 100),]
  sample_outcomes$mean[i] <- mean(sample_1$height)
  sample_outcomes$sd[i] <- sd(sample_1$height)
  sample_outcomes$se <- sd(sample_1$height)/sqrt(100)
}
sample_outcomes$sample <- as.factor(1:number_of_samples)
ggplot(sample_outcomes
       , aes_string(x="sample", y="mean")) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-(2*se), ymax=mean+(2*se)), size=1.5)+
  geom_hline(aes(yintercept=70)) +
  ylab("Mean")+
  xlab("Sample")+
  ggtitle("Variation in error bars")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#need for clarity

#error bars in practice
#using 2 se as rule of thumb
number_of_samples <- 1
sample_outcomes <- data.frame(mean = rep(NA, number_of_samples), sd = NA, se = NA)

for (i in 1:number_of_samples){
  sample_1 <- population_norm[sample(nrow(population_norm), 100),]
  sample_outcomes$mean[i] <- mean(sample_1$height)
  sample_outcomes$sd[i] <- sd(sample_1$height)
  sample_outcomes$se <- sd(sample_1$height)/sqrt(100)
}
sample_outcomes$sample <- as.factor(1:number_of_samples)

sample_1$sample <- "Data"
sample_1$data <- sample_1$height
onese <- sample_outcomes
onese$sample <- "+- 1 standard error"
onese$data <- onese$mean
onese$bar_length <-  onese$se
twosd <- sample_outcomes
twosd$sample <- "+- 2 standard error ~ \n 95% confidence interval"
twosd$data <- twosd$mean
twosd$bar_length <-  onese$se * 2
onesd <- sample_outcomes
onesd$sample <- "+- 1 standard deviation"
onesd$data <- onesd$mean
onesd$bar_length <-  onese$sd

example_clarity <- merge(sample_1, onese, all.x =T, all.y = T)
example_clarity <- merge(example_clarity, twosd, all.x =T, all.y = T)
example_clarity <- merge(example_clarity, onesd, all.x =T, all.y = T)

require(plyr)
example_clarity$sample <- relevel(as.factor(example_clarity$sample), "Data")

ggplot(example_clarity
       , aes_string(x="sample", y="data")) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-bar_length, ymax=mean+bar_length), size=1.5)+
  ylab("Height")+
  xlab("")+
  ggtitle("Variation in error bar display")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#comparing groups
#
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

#iris example####
#summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
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

function_output <- summarySE(iris, measurevar="Sepal.Length", groupvars =
                               c("Species"))

ggplot(function_output, aes_string(x="Species", y="mean")) +
  geom_col(aes_string(fill="Species"), size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#probabilty####
#
die_roll <- data.frame(Number = 1:6, Probability = rep(1/6,6))
ggplot(die_roll, aes_string(x="Number", y= "Probability")) +
  geom_col(color = "orange", fill="orange") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#normal curve
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, color = "orange") + 
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#fill in portion
#https://dkmathstats.com/plotting-normal-distributions-in-r-using-ggplot2/
# Shading from x = -1 to x = 1 (within one std deviation):

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -1 | x >= 1] <- NA
  return(norm_one_sd)
}

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, color = "orange") + 
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "purple") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#rolling 2 die simulation
number_of_rolls <- 1000
sum_of_rolls <- data.frame(index = 1:number_of_rolls, Sum = NA)
for (i in 1:number_of_rolls){
  dice_roll_trial <- sample(1:6, size = 2, replace = TRUE)
  sum_of_rolls$Sum[i] <- sum(dice_roll_trial)
}
ggplot(sum_of_rolls, aes_string(x="Sum")) +
  geom_histogram(color = "orange", fill="orange") +
  ylab("Frequency") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#use larger number to approximate discrete distribution
require(reshape2)
number_of_rolls <- 100000
sum_of_rolls <- data.frame(index = 1:number_of_rolls, Sum = NA)
for (i in 1:number_of_rolls){
  dice_roll_trial <- sample(1:6, size = 2, replace = TRUE)
  sum_of_rolls$Sum[i] <- sum(dice_roll_trial)
}
sum_of_rolls_df <- dcast(sum_of_rolls, Sum ~ "Probability", length )
sum_of_rolls_df$Probability <- sum_of_rolls_df$Probability/number_of_rolls
ggplot(sum_of_rolls_df, aes_string(x="Sum", y="Probability")) +
  geom_col(color = "orange", fill="orange") +
  ylab("Probability") +
  scale_x_continuous(breaks = c(2:12))+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#MORE DATA MANIPULATION AND INTRODUCTION TO GGPLOT2####
#many extra ggplot2 examples and code are found in the first lecture and 
#accompanying class code (intro_data_lecture under class files)

#Now you should be able to get data in and produce some basic plots. This script
#will focus on more ways to check data and introduce you to ggplot2.
#
#The intro script showed using file.choose and other methods to locate a data file 
#on your machine.  The accompanying problem set also showed you how to load data 
#from the internet.  
#
#lets return to the sleeping dataset
sleep <- read.csv("http://www.jstephengosnell.com/teaching-resources/datasets/sleep.csv?attredirects=0&d=1")
#you can find info on the dataset @ 
#http://www.statsci.org/data/general/sleep.html

#do some initial checks
head(sleep)
dim(sleep)
str(sleep)
summary(sleep)

#start graphical analysis.
pairs(sleep)

#several interesting patterns. Now lets introduce ggplot2 for plotting them. We'll 
#also talk about functions along the way.

#INTRO TO GGPLOT2####
#ggplot2 is a great plotting package that allows a lot of control over your output
#lets do some examples using the sleep dataset
#
#ggplot2 works in layers so you can or subtract as needed. Provided code is verbose here
#so you can see what its doing.

#first, install and call the package
require(ggplot2)

#to make a plot, first set a base layer
#lets start with a scatter plot and focus on relationship between time spent sleeping
#and time spent dreaming
#first, add your layers
dreaming_sleep_relationship <- ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming"))
#Now call the ggplot object you created
dreaming_sleep_relationship
#Nothing plots except the axes. Now you have to add layers. For example, you can add points
dreaming_sleep_relationship <- ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) + 
  geom_point()
dreaming_sleep_relationship
#Note here I'm saving the object, so to see it I call it. You can also just call directly
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point()
#then you get long calls that are easier (maybe) to manipulate
#Now you have a basic plot.  You can use other arguments in geom_layer commands 
#to add to it or themes or functions to change its look

#geom_ layers without any arguments assume you are using the base layers. You can 
#change this to add extra info to a plot. For example, let's color these by primate
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Primate"))

#now we've added information on primates. Note this is different from
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(colour="Primate")
#or
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(colour="blue")
#you have to put things you want to plot in the aes argument area (stands for aesthetics)
#, and anything outside of that changes the entire plot. Also note the 2nd method
#loses the legend as color now conveys no information
#
#this is also a good to talk about renaming factor labels. You may want to change 
#Primate levels to Yes and No for your graph. Lots of ways to do this, but revalue 
#in the plyr package is nice (and we'll use this suite of packages often, same person
#developed ggplot2, plyr, and reshape)
#
require(plyr)
sleep$Taxa <- revalue(sleep$Primate, c(Y = "Primate", N = "Non-primate"))
#notice what I did above. I made a new column from an existing one using a name 
#I might want on a legend
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"))
#I can also just change the legend title directly or change legend text, but often 
#workign with the dataframe is easier for me

#if we wanted these in a different order, we can use relevel to set one as the 
#"first" level (and then do this sequentially to get them in the right order if 
#needed".  You can also change level orders using the factor or ordered functions 
#for multiple levels at once
sleep$Taxa <- relevel(sleep$Taxa, "Primate" )
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"))

# Now lets change the axis labels,sizes, etc using theme function. Again, I left this
# verbose so you can change as needed. Also note the size argument in geom_point
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent dreaming daily") +
  xlab("Average hours spent sleeping daily") +
  ggtitle("Time spent dreaming increases with total sleeping time") +
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

#you can also directly change legend title and colours with the scale_ commands
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent dreaming daily") +
  xlab("Average hours spent sleeping daily") +
  ggtitle("Time spent dreaming increases with total sleeping time") +
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

#in general scale_[whatever you had aes commands]_manual lets you set colors or codes
#to see color codes go to 
#http://sape.inf.usi.ch/quick-reference/ggplot2/colour
#
#You can also facet a graph by type, eg
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent dreaming daily") +
  xlab("Average hours spent sleeping daily") +
  ggtitle("Time spent dreaming increases with total sleeping time") +
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
        legend.background = element_rect(fill="gray")) +
  facet_wrap(~Taxa, ncol = 1)

#notice doing this and legend may be redundant, so instead
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent dreaming daily") +
  xlab("Average hours spent sleeping daily") +
  ggtitle("Time spent dreaming increases with total sleeping time") +
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
        legend.background = element_rect(fill="gray"),
        strip.text.x = element_text(size = 18, colour = "purple")) +
  facet_wrap(~Taxa, ncol = 1) +
  guides(colour=FALSE)
# I also added a theme section to change the facet label
# 
# you can save the most recent plot directly to your working directory using
ggsave("Fig1.jpg")
#or just save using rstudio functionality

#ggplot2 is a great example of needing to undertand basic functionality without 
#having to remember everything.the intro class lecture and accompanying code should
#help you get started.  A few other points that often come up are noted below.

#For histograms, you only need one axis (frequency is calculated automatically)
ggplot(sleep, aes_string(x="Dreaming")) +
  geom_histogram()
#note we can just copy our theme info from above and modify as needed (or ggplot2
#will largely skip un-needed info).  You can also save and name a theme so you 
#don't have to do all this everytime.
ggplot(sleep, aes_string(x="Dreaming")) +
  geom_histogram() + 
  #below here is ylabel, xlabel, and main title
  ylab("Frequency") +
  xlab("Average hours spent dreaming daily") +
  ggtitle("Distribution of hours spent dreaming") +
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
        legend.background = element_rect(fill="gray"),
        strip.text.x = element_text(size = 18, colour = "purple"))


#Barcharts are common but hard to do in ggplot2.  We'll come back to those after
# we discuss estimation as they commonly (and need) to include error bars.
# 
# Finally, remember you can subset the dataframes you feed to the ggplot functions
# (or any other function for that matter). For example, let's just do a histogram 
# of primate sleep.

ggplot(sleep[sleep$Taxa == "Primate",], aes_string(x="Dreaming")) +
  geom_histogram() + 
  #below here is ylabel, xlabel, and main title
  ylab("Frequency") +
  xlab("Average hours spent dreaming daily") +
  ggtitle("Distribution of hours spent dreaming") +
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
        legend.background = element_rect(fill="gray"),
        strip.text.x = element_text(size = 18, colour = "purple"))
#not interesting, but you get the idea
#
##ESTIMATION, BARCHARTS, AND FUNCTIONS ####
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
#If you run it once, it will be available throughout an R session.  You can select the lines and run them.
#Another option is to copy (and keep) functions in a separate script that you "source" at the beginning of
#an R session. You can do this by manually opening file and selecting source button or by using the source()
#function.  Source means runs the entire script.
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

#let's practice source again by opening the central_limit_theorem.R script from
#the Examples folder on github, sourcing it, and looking at the plots it produce. 
#Note they show how going from samples of 1 to higher numbers increases the ability of 
#the means of data from distributions of multiple shapes to approach normality.

