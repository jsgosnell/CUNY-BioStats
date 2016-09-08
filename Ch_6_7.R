##Ch 6 -7 lecture/lab

#Whats here
## random R notes - line 15
## code from class - line 45
##lab material - line 105
###binomial test
###building a probability distribution
###sampling exercise
###bayesian review
### ggplot2 - line 125



#you can choose where you store downloaded packages
.libPaths("C:/Dropbox/Stephen/R_libraries_3.2.2")
#you can choose which directory you work in as well
getwd() # to see where R is looking for files
#setwd() to tell it where to look

#if you use dropbox, example below can move between computers
# working_directory <- "genetic_diversity_in_tropical_trees/working_directory-final_data"
#
# if(Sys.info()["effective_user"] %in% c( "SGosnell","sgosnell","Stephen")) {
#   #source file or use profile to set library, etc
#   .libPaths("C:/Dropbox/Stephen/R_libraries_3.2.2")
#   source("C:/Repositories/r_starter_code/R_starter.R")
#   setwd(paste("C:/Dropbox/Stephen/Experiments/Baruch_projects", working_directory, sep="/"))
# } else if (Sys.info()["effective_user"] %in% c( "sgosnell1")) {
#   #server
#   #source file or use profile to set library, etc
#   .libPaths("/home/R_libraries/R_libraries_3.2.2/")
#   source("/home/sgosnell1/Repositories/r_starter_code/R_starter.R")
#   setwd(paste("/home/sgosnell1/Dropbox/Stephen/Experiments/Baruch_projects", working_directory, sep="/"))
#   # } elseif {
#   #   # note you can set additional commands here to have it automatically work on other people's.
#   #   #just uncomment these lines and replace with needed code from above
# } else {
#   print("Default library location used.  Check and change with .libPaths() as needed.")
#   working_directory <- choose.dir(getwd(), "Please select the working directory that contains project data.")
#   setwd(working_directory)
# }


#graphs and tables shown in class
#this is the binomial distribution (one sample does not effect the others, collection of bernoulli trials))

#get one sample
sampling_experiment = rbinom(1, 20, .5)
par(mar=c(8,8,8,8))
hist(sampling_experiment, breaks = 0:20, xlab = "# of Right-handed frogs out of 20", ylab = "Probability of being drawn \n from population of p = 0.5", cex.main = 2, cex.axis = 1.5, cex.lab = 2)
#get two samples
sampling_experiment = rbinom(2, 20, .5)
hist(sampling_experiment, breaks = 0:20, xlab = "# of Right-handed frogs out of 20", ylab = "Probability of being drawn \n from population of p = 0.5", cex.main = 2, cex.axis = 1.5, cex.lab = 2)

#get three samples
sampling_experiment = rbinom(3, 20, .5)
hist(sampling_experiment, breaks = 0:20, xlab = "# of Right-handed frogs out of 20", ylab = "Probability of being drawn \n from population of p = 0.5", cex.main = 2, cex.axis = 1.5, cex.lab = 2)


#get 10,000  samples
sampling_experiment = rbinom(10000, 20, .5)
hist(sampling_experiment, breaks = 0:20, xlab = "# of Right-handed frogs out of 20", ylab = "Probability of being drawn \n from population of p = 0.5", cex.main = 2, cex.axis = 1.5, cex.lab = 2)


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
1-sum(dbinom(0:13,20,.5))
#or
1-pbinom(13,20,.5)
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
require(binom)
binom.test(x=30, n=87, p=.512, conf.level=.95)

#binconf can give variety of bounds (all will show you all the options)
binom.test(x=20, n=220, p=.15, conf.level=.95)
binom.confint(x=20, n=220, alpha=.05, method="all")



#LAB
#sourcing a code is a good way to input functions written by yourself or others
getwd()
#chivers_monty_hall_script.R is available on github
#save it somewhere and source it
monty(strat="stay")
monty(strat="switch")
monty(strat="random")


#Bayes is about combining data
  #we can do for events (Monty Hall)
  #or for distributions
    #this may require sampling procedures to difficulty in solving joint problems

#NEXT  TIME




#intro to ggplot2
#back to airquality!
head(airquality)
require(ggplot2)
#first, set a base layer
plot_layers <- ggplot(airquality, aes_string(x="Temp", y = "Ozone"))
plot_layers
#nothing
#you have to add layers
plot_layers_2 <- plot_layers + geom_line()
plot_layers_2
plot_layers_2 <- plot_layers + geom_point()
plot_layers_2
plot_layers_2 <- plot_layers + geom_point(colour = "Month")
plot_layers_2
#no, you have to set it in aes
plot_layers_2 <- plot_layers + geom_point(aes_string(colour = "Month"))
plot_layers_2
#now you can add other things
plot_layers_3 <- plot_layers_2 + xlab("Temperature")
plot_layers_3
#facet
plot_layers_2 <- plot_layers + geom_point()
plot_layers_2
plot_layers_2 + facet_wrap(~Month)
ggsave("Fig1.jpg")


