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

