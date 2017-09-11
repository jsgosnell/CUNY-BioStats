#graphs and tables shown in class
#this is the binomial distribution (one sample does not effect the others, collection of bernoulli trials))

#get one sample
#
par(mar=c(8,8,8,8))
sampling_experiment = rbinom(1, 20, .5)
hist(sampling_experiment, breaks = 0:20, probability = T, col = "orange",
     xlab = "# of Right-handed frogs out of 20", 
     ylab = "Probability of being drawn \n from population of p = 0.5", 
     cex.main = 2, cex.axis = 1.5, cex.lab = 2)



#get two samples

par(mar=c(8,8,8,8))
sampling_experiment = rbinom(2, 20, .5)
hist(sampling_experiment, breaks = 0:20, probability = T, col = "orange",
     xlab = "# of Right-handed frogs out of 20", 
     ylab = "Probability of being drawn \n from population of p = 0.5", 
     cex.main = 2, cex.axis = 1.5, cex.lab = 2)


#get three samples
par(mar=c(8,8,8,8))
sampling_experiment = rbinom(3, 20, .5)
hist(sampling_experiment, breaks = 0:20, probability = T, col = "orange",
     xlab = "# of Right-handed frogs out of 20", 
     ylab = "Probability of being drawn \n from population of p = 0.5", 
     cex.main = 2, cex.axis = 1.5, cex.lab = 2)
#get 10,000  samples
par(mar=c(8,8,8,8))
sampling_experiment = rbinom(10000, 20, .5)
hist(sampling_experiment, breaks = 0:20, probability = T, col = "orange",
     xlab = "# of Right-handed frogs out of 20", 
     ylab = "Probability of being drawn \n from population of p = 0.5", 
     cex.main = 2, cex.axis = 1.5, cex.lab = 2)

#getting the distribution
using_distribution = dbinom(0:20,20,.5)
using_distribution
sum(using_distribution)
Number_righthanded = c(0:20)
pdf = data.frame(Number_righthanded, using_distribution)
plot(0:20, using_distribution)
#for all plots, R takes a best guess at the best type of plot; you can also
#direct it with the type command.  ?plot for more info
barplot(using_distribution, xlab = "# of Right-handed frogs out of 20", ylab = "Probability of being drawn from population of p = 0.5" )

#probability of 14 or more
(1-sum(dbinom(0:13,20,.5))) * 2
#multiply by 2 since symmetrical
#or
(1-pbinom(13,20,.5)) * 2
#remember the one comes from the fact the entire distribution must sum to 1
#or
#using our sample
length(sampling_experiment[sampling_experiment >= 14 | sampling_experiment <= 6])/
  length(sampling_experiment)


#binomial test
binom.test(x=14, n=20, p=.5)

#radiologist example
binom.test( 30, 87, .512)

#binom.confint can give variety of bounds (all will show you all the options)
binom.test(x=30, n=87, p=.512, conf.level=.95)

#binconf can give variety of bounds (all will show you all the options)
binom.test(x=20, n=220, p=.15, conf.level=.95)
require(binom)
binom.confint(x=20, n=220, alpha=.05, method="all")

#radiologist example
using_distribution <- dbinom(0:87,87,.5125)
radiologist <- data.frame (Number = 0:87, Probability = using_distribution)
require(ggplot2)
radiologist$Proportion <- radiologist$Number/87
radiologist$criteria <- "retain"
radiologist$criteria[pbinom(radiologist$Number, 87, .512) < .025] <- "reject"
radiologist$criteria[(1-pbinom(radiologist$Number, 87, .512)) < .025] <- "reject"
proportion_observed = data.frame(Proportion = 30/87, Probability = .04)
radiologist_plot <- ggplot(radiologist, aes(x = Proportion, y = Probability))
radiologist_plot + geom_bar(stat="identity", aes(fill = criteria)) + geom_segment(x = .254, xend = .45,
                                                                                  y= .04 , yend =.04, size = 2) +
  geom_vline(xintercept = .5125, color = "blue") + geom_vline(xintercept = 30/87, color = "black") +
  geom_point(data= proportion_observed, size = 8) +
  ggtitle("Comparing p-values and confidence intervals for radiologist problem") +
  theme(plot.title = element_text(size = rel(2)))
