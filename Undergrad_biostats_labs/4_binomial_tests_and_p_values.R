#BINOMIAL TESTS AND P-VALUES ####
#
#This is where the class moves from lots of work to shorter functions. After the 
#longish lecture, the only real outcome is using a binomial test. Here we'll produce
#one by sampling and the actual function. 
#
# example adapted from: http://tx.shu.edu.tw/~purplewoo/Literature/!Methodology/!Distribution_SampleSize/MMU%20-%20Research%20Design,%20Biol%20SciThe%20Binomial%20Distribution%20and%20Proportions.htm
# 
# Assume you are a epidemiologist studying a new disease.  Previous studies have found
# it only occurs in 2% of the population. You have funding to sample 40 random people
# to find traces of the disease.  Will you likely be successful?
# 
# This may look odd, but we can use the binomial distribution to determine our chances
# of success. You are sampling 40 individuals, each has a .02 chance of being infected
# (a success for us, unfortunately), and you want know the chance of getting at least one.
#
# we can phrase this as 1 - chance of getting no infected individuals
1 - pbinom(0,40,.02)
#pbinom gives the probability of getting a certain outcome
#dbinom gives chance of any specific outcome
dbinom(0,40,.02) #same in this case since theres only one case
#so you have a 55%  chance of getting at least one individual - not great.  
#
#However, you carry out the study and find 3 individuals with 
#the disease!  Does this support the infection rate being 2% in the population?
#
#using the binomial test
binom.test(x = 3, n = 40, p = .02) #suggests its unlikely (p < .05) that you get
#this under the null hypothesis
#
#using distribution
#you expect to get 
mu <- 40 * .02
mu # can't go negative, so nothing less extreme!
#so 1 - probability of getting more than 2
1- pbinom(2,40,.02)
#through sampling
sampling_experiment = rbinom(10000, 40, .02)
length(sampling_experiment[sampling_experiment >= 3])/
  length(sampling_experiment)

#Bayesian considerations
#Now that we've developed confidence interval and p-values, lets reconsider how
#Bayesians differ using the classic Monty Hall question and some code.
#Follow this code for assignment.
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
#in the future we'll show how do this using a different process for actually sampling
