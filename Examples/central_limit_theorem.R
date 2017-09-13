#this file produces a series of graphs to consider how means of sample date 
#from various types of distributions converge towards a normal distribution

#normal
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rnorm(n=n[j]))
  }
  hist(means, main=paste("Normal, n = ",n[j], sep = ""))
}

#double exponential (laplace)
require(VGAM)
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rlaplace(n=n[j]))
  }
  hist(means, main=paste("Double exponential \n n = ",n[j], sep = ""))
}

#uniform
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(runif(n=n[j]))
  }
  hist(means, main=paste("Uniform, n = ",n[j], sep = ""))
}

#exponential
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rexp(n=n[j]))
  }
  hist(means, main=paste("Exponential, n = ",n[j], sep = ""))
}

#chi square
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rchisq(n=n[j], df=4))
  }
  hist(means, main=paste("Chi square, n = ",n[j], sep = ""))
}