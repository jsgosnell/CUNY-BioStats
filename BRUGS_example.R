#Bayesian example
#for more info, https://theoreticalecology.wordpress.com/setting-up-openbugs-and-r/
#requires installation of openbugs (other program) and package for R to to talk to
#open bugs (options are BRugs, R2OpenBUGS)

#we'll demonstrate with the survey data from the MASS package
#example from http://www.r-tutor.com/bayesian-statistics/openbugs
#given survey data on smoking in college, what would we predict percent smokers is
#for general population

require(MASS)
head(survey)

#first, we have to write model and save for openbugs. you could do in txt file and call,
#or do it all in R. R automatically saves and searches in your current working directory

model <- function() {
  #this is openbugs syntax, but thankfully its close to R
  # Prior
  #same as uniform
  p ~ dbeta(1, 1)

  # Likelihood
  #this will come from survey data
  y ~ dbin(p, N)
}


#this just saves model to your working directory
require(R2OpenBUGS)
write.model(model, "model.txt")

#using BRugs gives more options than R2OpenBUGS, but they are similar
require(BRugs)
modelCheck("model.txt")

#now you need your data,
#here,its only the number of smokers/total
#we are trying to find p, so we need N and y
N <- length(na.omit(survey$Smoke))
y <- length(na.omit(survey$Smoke[survey$Smoke == "Never"]))

#whats the data
data <- list("N", "y")
#what are we trying to find
params <- c("p")
#where should we initilize the sampler
inits <- function() { list(p=0.5) }

out <- BRugsFit(modelFile = 'model.txt', data = data, inits = inits,
                parametersToSave = params, numChains = 3, nBurnin = 500,
                nIter = 10000, coda = T)

#basic diagnostics
plot(out)
summary(out)
samplesBgr("*")
require(coda)
gelman.diag(out)





