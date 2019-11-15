#plots from 2. Estimation and probability.ppt


#create "population" of males####

population_size <- 10000
set.seed(42)
population_norm <- data.frame(id = 1:population_size, 
                         height = rnorm(population_size, 70, 3))

library(ggplot2)

ggplot(population_norm, aes(height)) + 
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
ggplot(sample_1, aes(height)) + 
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

ggplot(sample_outcomes, aes(mean)) + 
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

ggplot(sample_outcomes, aes(mean)) + 
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

ggplot(sample_outcomes, aes(mean)) + 
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

ggplot(population_unif, aes(height)) + 
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
ggplot(sample_unif_1, aes(height)) + 
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

ggplot(sample_outcomes, aes(mean)) + 
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
       , aes(x=sample, y=mean)) +
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

library(plyr)
example_clarity$sample <- relevel(as.factor(example_clarity$sample), "Data")

ggplot(example_clarity
       , aes(x=sample, y=data)) +
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
library(Rmisc)
#BAR PLOTS####
#Now let's use this to get the confidence intervals for each month and plot them
monthly_wind_data <- summarySE(airquality, measurevar = "Wind", groupvars = "Month")
#look at it
monthly_wind_data

#now plot it in ggplot2

library(ggplot2)

ggplot(monthly_wind_data
       , aes(x=Month, y=Wind)) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=Wind-ci, ymax=Wind+ci), size=1.5) +
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
       , aes(x=Month, y=mean)) +
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

function_output <- summarySE(iris, measurevar="Sepal.Length", groupvars =
                               c("Species"))

ggplot(function_output, aes(x=Species, y=Sepal.Length)) +
  geom_col(aes(fill=Species), size = 3) +
  geom_errorbar(aes(ymin=Sepal.Length-ci, ymax=Sepal.Length+ci), size=1.5) +
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
ggplot(die_roll, aes(x=Number, y= Probability)) +
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
ggplot(sum_of_rolls, aes(x=Sum)) +
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
library(reshape2)
number_of_rolls <- 100000
sum_of_rolls <- data.frame(index = 1:number_of_rolls, Sum = NA)
for (i in 1:number_of_rolls){
  dice_roll_trial <- sample(1:6, size = 2, replace = TRUE)
  sum_of_rolls$Sum[i] <- sum(dice_roll_trial)
}
sum_of_rolls_df <- dcast(sum_of_rolls, Sum ~ "Probability", length )
sum_of_rolls_df$Probability <- sum_of_rolls_df$Probability/number_of_rolls
ggplot(sum_of_rolls_df, aes(x=Sum, y=Probability)) +
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

#probability trees
##below code is unfortunately cumbersome
#from https://daranzolin.github.io/2018-01-07-probability-trees/
#
library(DiagrammeR)

bayes_probability_tree <- function(prior, true_positive, true_negative, label1 = "Prior", 
                                   label2 = "Complimentary Prior", label3 = "True Positive",
                                   label4 = "False Negative", label5 = "False Positive",
                                   label6 = "True Negative") {
  
  if (!all(c(prior, true_positive, true_negative) > 0) && !all(c(prior, true_positive, true_negative) < 1)) {
    stop("probabilities must be greater than 0 and less than 1.",
         call. = FALSE)
  }
  c_prior <- 1 - prior
  c_tp <- 1 - true_positive
  c_tn <- 1 - true_negative
  
  round4 <- purrr::partial(round, digits = 5)
  
  b1 <- round4(prior * true_positive)
  b2 <- round4(prior * c_tp)
  b3 <- round4(c_prior * c_tn)
  b4 <- round4(c_prior * true_negative)
  
  bp <-  round4(b1/(b1 + b3))
  
  labs <- c("X", prior, c_prior, true_positive, c_tp, true_negative, c_tn, b1, b2, b4, b3)
  
  tree <-
    create_graph() %>%
    add_n_nodes(
      n = 11,
      type = "path",
      label = labs,
      node_aes = node_aes(
        shape = "circle",
        height = 1,
        width = 1,
        x = c(0, 3, 3, 6, 6, 6, 6, 8, 8, 8, 8),
        y = c(0, 2, -2, 3, 1, -3, -1, 3, 1, -3, -1))) %>% 
    add_edge(
      from = 1,
      to = 2,
      edge_aes = edge_aes(
        label = label1
      )
    ) %>% 
    add_edge(
      from = 1, 
      to = 3,
      edge_aes = edge_aes(
        label = label2
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 4,
      edge_aes = edge_aes(
        label = label3
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 5,
      edge_aes = edge_aes(
        label = label4
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 7,
      edge_aes = edge_aes(
        label = label5
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 6,
      edge_aes = edge_aes(
        label = label6
      )
    ) %>% 
    add_edge(
      from = 4,
      to = 8,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 5,
      to = 9,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 7,
      to = 11,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 6,
      to = 10,
      edge_aes = edge_aes(
        label = "="
      )
    ) 
  message(glue::glue("The probability of having {label1} after testing {label3} is {bp}"))
  print(render_graph(tree))
  invisible(tree)
}

#first example
bayes_probability_tree(prior = 0.5, true_positive = 0.6, true_negative = 0.9, label1 = "medicine", label2 = "placebo",
                       label3 = "cured", label4 = "not cured",
                       label5 = "cured", label6 = "not cured")

#second example
bayes_probability_tree(prior = 0.0001, true_positive = 0.9, true_negative = 0.999, label1 = "cancer", 
                       label2 = "not cancer",
                       label3 = "positive", 
                       label4 = "negative",
                       label5 = "positive", 
                       label6 = "negative")

#add nodes
.00009 + .00001 + .001 + .9989

(.00009)/(.001+ .00009)

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
sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
#you can find info on the dataset @ 
#http://www.statsci.org/data/general/sleep.html

#do some initial checks
head(sleep)
dim(sleep)
str(sleep)
summary(sleep)

#start graphical analysis
pairs(sleep)

#several interesting patterns. Now lets introduce ggplot2 for plotting them. We'll 
#also talk about functions along the way.

#INTRO TO GGPLOT2####
#ggplot2 is a great plotting package that allows a lot of control over your output
#lets do some examples using the sleep dataset
sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
#
#ggplot2 works in layers so you can or subtract as needed. Provided code is verbose here
#so you can see what its doing.

#first, install and call the package
library(ggplot2)

#to make a plot, first set a base layer
#lets start with a scatter plot and focus on relationship between time spent sleeping
#and time spent dreaming
#first, add your layers
dreaming_sleep_relationship <- ggplot(sleep, aes(x=TotalSleep, y = Dreaming))
#Now call the ggplot object you created
dreaming_sleep_relationship
#Nothing plots except the axes. Now you have to add layers. For example, you can add points
dreaming_sleep_relationship <- ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) + 
  geom_point()
dreaming_sleep_relationship
#Note here I'm saving the object, so to see it I call it. You can also just call directly
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point()
#then you get long calls that are easier (maybe) to manipulate
#Now you have a basic plot.  You can use other arguments in geom_layer commands 
#to add to it or themes or functions to change its look

#geom_ layers without any arguments assume you are using the base layers. You can 
#change this to add extra info to a plot. For example, let's color these by primate
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Primate))

#now we've added information on primates. Note this is different from
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(colour="Primate")
#or
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
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
library(plyr)
sleep$Taxa <- revalue(sleep$Primate, c(Y = "Primate", N = "Non-primate"))
#notice what I did above. I made a new column from an existing one using a name 
#I might want on a legend
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa))
#I can also just change the legend title directly or change legend text, but often 
#workign with the dataframe is easier for me

#if we wanted these in a different order, we can use relevel to set one as the 
#"first" level (and then do this sequentially to get them in the right order if 
#needed".  You can also change level orders using the factor or ordered functions 
#for multiple levels at once
sleep$Taxa <- relevel(sleep$Taxa, "Primate" )
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa))

# Now lets change the axis labels,sizes, etc using theme function. Again, I left this
# verbose so you can change as needed. Also note the size argument in geom_point
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa), size = 4) +
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
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa), size = 4) +
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
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa), size = 4) +
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
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa), size = 4) +
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
ggplot(sleep, aes(x=Dreaming)) +
  geom_histogram()
#note we can just copy our theme info from above and modify as needed (or ggplot2
#will largely skip un-needed info).  You can also save and name a theme so you 
#don't have to do all this everytime.
ggplot(sleep, aes(x=Dreaming)) +
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



# Finally, remember you can subset the dataframes you feed to the ggplot functions
# (or any other function for that matter). For example, let's just do a histogram 
# of primate sleep.

ggplot(sleep[sleep$Taxa == "Primate",], aes(x=Dreaming)) +
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

#Let's go back to the sleep dataset and estimate the average total sleep time speed 
#for each exposure level
#First, lets change exposure to factors and label them
str(sleep) #just a reminder
sleep$Exposure <- factor(sleep$Exposure)
#check levels
levels(sleep$Exposure)
#relabel if you want (just for example here)
levels(sleep$Exposure)<- c("Least","Less", "Average", "More", "Most") 

#Let's just focus on least first to calculate confidence intervals. note we have 
#to remove NA
mean(sleep[sleep$Exposure == "Least", "TotalSleep"], na.rm = T)
#this is our estimate.  The standard deviation of this estimate is
sd(sleep[sleep$Exposure == "Least", "TotalSleep"], na.rm = T) / 
  sqrt(length(sleep[sleep$Exposure == "Least" & is.na(sleep$TotalSleep) == F, "TotalSleep"]))
# is equivalent to
sd(sleep[sleep$Exposure == "Least", "TotalSleep"], na.rm = T) / 
  sqrt(length(na.omit(sleep[sleep$Exposure == "Least", "TotalSleep"])))
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

#BAR PLOTS####
#Now let's use this to get the confidence intervals for each exposure level and plot them
sleep_by_exposure <- summarySE(sleep, measurevar = "TotalSleep", groupvars = "Exposure", na.rm = T)
#look at it
sleep_by_exposure
library(ggplot2)
ggplot(sleep_by_exposure
       , aes(x=Exposure, y=TotalSleep)) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=TotalSleep-ci, ymax=TotalSleep+ci), size=1.5) +
  ylab("Total sleep (hours per day")+ggtitle("Sleep across different taxa")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#or better
ggplot(sleep_by_exposure
       , aes(x=Exposure, y=TotalSleep)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=TotalSleep-ci, ymax=TotalSleep+ci), size=1.5) +
  ylab("Total sleep (hours per day")+ggtitle("Sleep across different taxa")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#let's practice source again by opening the central_limit_theorem.R script from
#the Examples folder on github, sourcing it, and looking at the plots it produce. 
#Note they show how going from samples of 1 to higher numbers increases the ability of 
#the means of data from distributions of multiple shapes to approach normality.

