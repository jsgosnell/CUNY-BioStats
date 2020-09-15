#this file produces a series of graphs to consider how means of sample date 
#from various types of distributions converge towards a normal distribution

#normal
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), oma = c(5,2,2,2), ask=F)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rnorm(n=n[j]))
  }
  hist(means, main=paste("Normal, n = ",n[j], sep = ""), col = "orange", xlab = "mean of draws")
}
mtext("Distribution of 9000 means of n draws \n from a standard normal (Z) distribution", outer = T, side = 1, 
      cex = 2, line = 4)

readline(prompt="Press [enter] to continue")

#double exponential (laplace)
require(VGAM)
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), oma = c(5,2,2,2), ask=F)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rlaplace(n=n[j]))
  }
  hist(means, main=paste("Double exponential \n n = ",n[j], sep = ""),col = "orange", xlab = "mean of draws")
}
mtext("Distribution of 9000 means of n draws \n from a double exponential distribution", outer = T, side = 1, 
      cex = 2, line = 4)

readline(prompt="Press [enter] to continue")

#uniform
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), oma = c(5,2,2,2), ask=F)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(runif(n=n[j]))
  }
  hist(means, main=paste("Uniform, n = ",n[j], sep = ""), col = "orange", xlab = "mean of draws")
}
mtext("Distribution of 9000 means of n draws \n from a uniform distribution", outer = T, side = 1, 
      cex = 2, line = 4)

readline(prompt="Press [enter] to continue")

#exponential
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), oma = c(5,2,2,2), ask=F)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rexp(n=n[j]))
  }
  hist(means, main=paste("Exponential, n = ",n[j], sep = ""), col = "orange", xlab = "mean of draws")
}
mtext("Distribution of 9000 means of n draws \n from an exponential distribution", outer = T, side = 1, 
      cex = 2, line = 4)

readline(prompt="Press [enter] to continue")

#chi square
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), oma = c(5,2,2,2), ask=F)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rchisq(n=n[j], df=4))
  }
  hist(means, main=paste("Chi square, n = ",n[j], sep = ""), col = "orange", xlab = "mean of draws")
}
mtext("Distribution of 9000 means of n draws \n from a chi square (df = 4) distribution", outer = T, side = 1, 
      cex = 2, line = 4)

readline(prompt="Press [enter] to continue")

#binomial
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), oma = c(5,2,2,2), ask=F)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rbinom(n=n[j], 1, .7))
  }
  hist(means, main=paste("Binomial, n = ",n[j], sep = ""), col = "orange", xlab = "mean of draws")
}
mtext("Distribution of 9000 means of n draws \n from a binomial distribution, p = .7", outer = T, side = 1, cex = 2, line = 4)


