#1
binom.test(19,25, p=.5)

#to see null distributions
sampling_experiment = rbinom(10000, 25, .5)
hist(sampling_experiment, breaks = 0:25, xlab = "# of Right-eared people out of 25", ylab = "Probability of being drawn \n from population of p = 0.5", cex.main = 2, cex.axis = 1.5, cex.lab = 2)

one_by_one <- data.frame(number_observed = 0:25)
one_by_one$prob <- dbinom(one_by_one$number_observed,25,.5)
plot(prob~number_observed, one_by_one)


#understand what rbinom, pbinom, dbinom, and qbinom do
using_distribution = dbinom(0:25,25,.5)
using_distribution
sum(using_distribution)
Number_righteared = c(0:25)
pdf = data.frame(Number_righthanded, using_distribution)
plot(0:25, using_distribution)

#to get pvalues
length(sampling_experiment[sampling_experiment >= 19 | sampling_experiment <= 6])/length(sampling_experiment)

(1-pbinom(18,25,.5)) * 2

binom.test(19,25, p=.5)

#confidence interval
require(binom)
binom.confint(x=19, n=25, alpha=.05, method="all")


#2
binom.test(33,100, alternative="greater", p=.25)

#3
#source chivers script
monty(strat="stay")
monty(strat="switch")
monty(strat="random")


