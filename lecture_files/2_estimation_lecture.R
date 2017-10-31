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
