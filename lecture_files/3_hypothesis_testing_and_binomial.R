#graphs and tables from Hypothesis testing and the binomial lecture
#this is the binomial distribution (one sample does not effect the others, collection of bernoulli trials))

#get one sample####
#
par(mar=c(8,8,8,8))
sampling_experiment = rbinom(1, 18, .5)
hist(sampling_experiment, breaks = 0:20, probability = T, col = "orange",
     xlab = "# of Right-handed frogs out of 20", 
     ylab = "Probability of being drawn \n from population of p = 0.5", 
     cex.main = 2, cex.axis = 1.5, cex.lab = 2)


sampling_experiment_df <- data.frame("Right_Handed" = sampling_experiment)
library(ggplot2)
ggplot(sampling_experiment_df, aes(Right_Handed)) + 
  geom_bar(size=3, width = 1, fill="orange")+
  xlim(0,20) +
  xlab("# of right-handed frogs")+
  ylab("Frequency")+
  ggtitle("Number of right-handed frogs under the null distribution, 1 sample")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  guides(fill = F)



#get two samples####

par(mar=c(8,8,8,8))
sampling_experiment = rbinom(2, 18, .5)
hist(sampling_experiment, breaks = 0:20, probability = T, col = "orange",
     xlab = "# of Right-handed frogs out of 18", 
     ylab = "Probability of being drawn \n from population of p = 0.5", 
     cex.main = 2, cex.axis = 1.5, cex.lab = 2)

sampling_experiment_df <- data.frame("Right_Handed" = sampling_experiment)
ggplot(sampling_experiment_df, aes(Right_Handed)) + 
  geom_bar(size=3, width = 1, fill="orange")+
  xlim(0,20) +
  xlab("# of right-handed frogs")+
  ylab("Frequency")+
  ggtitle("Number of right-handed frogs under the null distribution, 2 samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  guides(fill = F)



#get three samples####
par(mar=c(8,8,8,8))
sampling_experiment = rbinom(3, 18, .5)
hist(sampling_experiment, breaks = 0:20, probability = T, col = "orange",
     xlab = "# of Right-handed frogs out of 18", 
     ylab = "Probability of being drawn \n from population of p = 0.5", 
     cex.main = 2, cex.axis = 1.5, cex.lab = 2)

sampling_experiment_df <- data.frame("Right_Handed" = sampling_experiment)
ggplot(sampling_experiment_df, aes(Right_Handed)) + 
  geom_bar(size=1, width = 1, fill="orange", color="black")+
  xlim(0,20) +
  xlab("# of right-handed frogs")+
  ylab("Frequency")+
  ggtitle("Number of right-handed frogs under the null distribution, 3 samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  guides(fill = F)


#get 10,000  samples####
par(mar=c(8,8,8,8))
sampling_experiment = rbinom(10000, 18, .5)
hist(sampling_experiment, breaks = 0:20, probability = T, col = "orange",
     xlab = "# of Right-handed frogs out of 20", 
     ylab = "Probability of being drawn \n from population of p = 0.5", 
     cex.main = 2, cex.axis = 1.5, cex.lab = 2)

sampling_experiment_df <- data.frame("Right_Handed" = sampling_experiment)
ggplot(sampling_experiment_df, aes(Right_Handed)) + 
  geom_bar(size=1, width = 1, fill="orange", color="black")+
  xlim(0,20) +
  xlab("# of right-handed frogs")+
  ylab("Frequency")+
  ggtitle("Number of right-handed frogs under the null distribution, 10,000 samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  guides(fill = F)


#how often do we see DEVIATION as larger as 14 away####
#using our sample
length(sampling_experiment[sampling_experiment >= 14 | sampling_experiment <= 4])/
  length(sampling_experiment)

sampling_experiment_df <- data.frame("Right_Handed" = sampling_experiment)
sampling_experiment_df$group <- "normal"
sampling_experiment_df[sampling_experiment_df$Right_Handed >=14 | 
                         sampling_experiment_df$Right_Handed <= 4, "group"] <- "extreme"

ggplot(sampling_experiment_df, aes(Right_Handed)) + 
  geom_bar(aes(fill=group), size=3)+
  xlab("# of right-handed frogs")+
  ylab("Frequency")+
  ggtitle("Number of right-handed frogs under the null distribution, 10,000 samples")+
  scale_fill_manual(name="",values = c("purple","orange")) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  guides(fill = F)


#using the binomial distribution####
using_distribution = dbinom(0:18,18,.5)
using_distribution
sum(using_distribution)
Number_righthanded = c(0:18)
pdf = data.frame(Number_righthanded, using_distribution)
plot(0:18, using_distribution)
#for all plots, R takes a best guess at the best type of plot; you can also
#direct it with the type command.  ?plot for more info
barplot(using_distribution, xlab = "# of Right-handed frogs out of 18", ylab = "Probability of being drawn from population of p = 0.5" )

ggplot(pdf, aes(x=Number_righthanded, y=using_distribution)) + 
  geom_bar(size=1, width = 1, fill="orange", color="black", stat = "identity")+
  xlim(0,20) +
  xlab("# of right-handed frogs")+
  ylab("Frequency")+
  ggtitle("Number of right-handed frogs under the binomial distribution")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  guides(fill = F)


#probability of 14 or more####
(1-sum(dbinom(0:13,18,.5))) * 2
#multiply by 2 since symmetrical
#or
(1-pbinom(13,18,.5)) * 2
#remember the one comes from the fact the entire distribution must sum to 1
#or
#using our sample
length(sampling_experiment[sampling_experiment >= 14 | sampling_experiment <= 4])/
  length(sampling_experiment)

#binomial test####
binom.test(x=14, n=18, p=.5)

#explaining sided tests####

#normal curve####
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, fill = "orange",
                geom = "area") + 
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#two-sided test####

#fill in portion
#https://dkmathstats.com/plotting-normal-distributions-in-r-using-ggplot2/
# Shading from x = -1 to x = 1 (within one std deviation):

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -1.96 | x >= 1.96] <- NA
  return(norm_one_sd)
}

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, fill = "purple",
                geom = "area") + 
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#left-tailed test####

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[ x <= -1.65] <- NA
  return(norm_one_sd)
}


ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, fill = "purple",
                geom = "area") + 
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#right-tailed test####

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[ x >= 1.65] <- NA
  return(norm_one_sd)
}


ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, fill = "purple",
                geom = "area") + 
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#pvalues are uniformly distributed####
#example
num_of_samples <- 10000
pvalue <- data.frame(sample = 1:num_of_samples, pvalue=NA)
for (i in 1:num_of_samples){
  x <- rnorm(100)
  pvalue$pvalue[i] <- t.test(x,u=0)$p.value
}

ggplot(pvalue, aes(x=pvalue))+
  geom_histogram()+
  ylab("Frequency") +
  xlab("Observed p-value for population that fits null hypothesis")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


  


#radiologist example####
binom.test(30, 87, .512)

#binom.confint can give variety of bounds (all will show you all the options)
library(binom)
binom.confint(x=30, n=87, conf.level=.95, method="all")

#compare confidence intervals and tests####
using_distribution <- dbinom(0:87,87,.5125)
radiologist <- data.frame (Number = 0:87, Probability = using_distribution)
library(ggplot2)
radiologist$Proportion <- radiologist$Number/87
radiologist$criteria <- "retain"
radiologist$criteria[pbinom(radiologist$Number, 87, .512) < .025] <- "reject"
radiologist$criteria[(1-pbinom(radiologist$Number, 87, .512)) < .025] <- "reject"
proportion_observed = data.frame(Proportion = 30/87, Probability = .04)
ggplot(radiologist, aes(x = Proportion, y = Probability)) + 
  geom_bar(stat="identity", aes(fill = criteria)) + 
  geom_segment(x = .254, xend = .45,y= .04 , yend =.04, size = 2) +
  geom_vline(xintercept = .5125, color = "blue") + geom_vline(xintercept = 30/87, color = "black") +
  geom_point(data= proportion_observed, size = 8) +
  ggtitle("Comparing p-values and confidence intervals for radiologist problem") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  guides(fill = F)



