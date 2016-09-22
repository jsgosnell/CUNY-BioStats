#Bayesian example
#for more info, https://theoreticalecology.wordpress.com/setting-up-openbugs-and-r/
#requires installation of openbugs (other program) and package for R to to talk to
#open bugs (options are BRugs, R2OpenBUGS)
require(BRugs)

#we'll demonstrate with the survey data from the MASS package
#example from http://www.r-tutor.com/bayesian-statistics/openbugs
#given survey data on smoking in college, what would we predict percent smokers is
#for general population

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
    working_directory <- choose.dir(getwd(), "Please select the working directory that contains project data.")
    setwd(working_directory)
# }

require(MASS)
head(survey)

#first, we have to write model and save for openbugs. you could do in txt file and call,
#or do it all in R

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





