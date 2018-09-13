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
require(ggplot2)
ggplot(sampling_experiment_df, aes_string("Right_Handed")) + 
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
ggplot(sampling_experiment_df, aes_string("Right_Handed")) + 
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
ggplot(sampling_experiment_df, aes_string("Right_Handed")) + 
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
ggplot(sampling_experiment_df, aes_string("Right_Handed")) + 
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

ggplot(sampling_experiment_df, aes_string("Right_Handed")) + 
  geom_bar(aes_string(fill="group"), size=3, bins = 20)+
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

ggplot(pdf, aes_string(x="Number_righthanded", y="using_distribution")) + 
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

ggplot(pvalue, aes_string(x="pvalue"))+
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
binom.test( 30, 87, .512)

#binom.confint can give variety of bounds (all will show you all the options)
binom.test(x=30, n=87, p=.512, conf.level=.95)

#binom.confint can give variety of bounds (all will show you all the options)
require(binom)
binom.confint(x=30, n=87, alpha=.05, method="all")

#compare confidence intervals and tests####
using_distribution <- dbinom(0:87,87,.5125)
radiologist <- data.frame (Number = 0:87, Probability = using_distribution)
require(ggplot2)
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

#bayesian analysis####
#frog analysis
require(LearnBayes)
#for proportions, we often use beta
#beta curve peaked at .5####
ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = 20, shape2 = 20),
                aes(color = "20, 20"),
                size = 3,geom = "line") + 
  stat_function(fun = dbeta, n = 101, args = list(shape1 = 2, shape2 = 2),
                aes(color = "2, 2"),
                size = 3,geom = "line") + 
  stat_function(fun = dbeta, n = 101, args = list(shape1 = 5, shape2 = 20),
                aes(color = "5, 20"),
                size = 3,geom = "line") +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = 1, shape2 = 1), 
                aes(color = "1, 1"),
                size = 3,geom = "line") + 
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  labs(colour = "Parameters (a, b)")

#triplot####
#peaked at .5 (more certain)
triplot(prior = c(20,20), data = c(14,4), where = "topleft")
#less peaked at .5
triplot(prior = c(2,2), data = c(14,4), where = "topleft")

#get a credible interval####
#for posterior we can simply add shape 1 + success, shape2 + failures
qbeta(c(0.025, 0.975),shape1 =  20+14, shape2 = 20+4)
qbeta(c(0.025, 0.975),shape1 =  2+14, shape2 = 2+4)

#INTRO TO GGPLOT2####
#copied here from earlier script for 78001/78002
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
  length(sleep[sleep$Exposure == "Least" & is.na(sleep$TotalSleep) == F, "TotalSleep"])
# is equivalent to
sd(sleep[sleep$Exposure == "Least", "TotalSleep"], na.rm = T) / 
  length(na.omit(sleep[sleep$Exposure == "Least", "TotalSleep"]))
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
#Now let's use this to get the confidence intervals for each exposure level and plot them
sleep_by_exposure <- summarySE(sleep, measurevar = "TotalSleep", groupvars = "Exposure", na.rm = T)
#look at it
sleep_by_exposure
require(ggplot2)
ggplot(sleep_by_exposure
       , aes_string(x="Exposure", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
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
       , aes_string(x="Exposure", y="mean")) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
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







