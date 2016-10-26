##Ch 10 - 11 lecture/lab

#Normality Simuation
#to help you develop an intuition for when normality assumptions are appropriate,
#we'll consider when the following distributions are approximately normal
#bernoulli, binomial, poisson, normal, chi-square, students t

#in general, generate random samples and compare to normal or actually call the distribution

x <- rnorm(10000, mean = 0, sd = sqrt(10))
#to develop plot
qqnorm(x)
# to add line
qqline(x)
#other examples to help with lab assignment
?rbinom
?rpois
?rnorm
?rchisq
?rt

#full code below (run this in blocks) for several of the groups

#bernoulli
y=0:1
n=1
p=c(.1,.5,.9)
par(mfcol=c(2,2))
for (i in 1:length(p)){
  x=dbinom(y,n,p[i])
  plot(as.factor(y),x, type="h", main=p[i])
}

#or using random selections

par(mfcol=c(2,2))
for (i in 1:length(p)){
  x=rbinom(1000,1,p[i])
  hist(x, main=p[i])
}


#binomial
n=c(2,5,25,50)
p=c(.05, .25, .5, .75, .95)
par(mfcol=c(2,2), ask=T, oma=c(3,3,3,3))
for(j in 1:length(p)){
  for (i in 1:length(n)){
    y=0:n[i]
    x=dbinom(y,n[i],p[j])
    plot(as.factor(y),x, type="h", main=n[i])
    mtext(p[j], outer=T)
  }
}

#poisson
u=c(2,5,10,30,50)
par(mfcol=c(2,2), ask=T)
for(j in 1:length(u)){
  x=dpois(x=0:(u[j]*3),lambda=u[j])
  plot(0:(u[j]*3),x, type="h", main=u[j])
}
}

#normal
sigma=c(10,100)
par(mfcol=c(2,2), ask=T)
for(j in 1:length(sigma)){
  x=rnorm(1000, 0, sqrt(sigma[j]))
  hist(x)
  qqnorm(x)
  qqline(x)
}
}

#t
freedom=c(1,10,30,100)
par(mfcol=c(2,4), ask=T)
for(j in 1:length(freedom)){
  x=rt(1000, df=freedom[j])
  hist(x, main=freedom[j])
  qqnorm(x)
  qqline(x)
}

#part 2
#more importantly, lets consider how the properties of distributions impact the
#ability of sample means to approximate normality

#normal
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rnorm(n=n[j]))
  }
  hist(means, main=n[j])
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
  hist(means, main=n[j])
}

#uniform
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(runif(n=n[j]))
  }
  hist(means, main=n[j])
}

#exponential
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rexp(n=n[j]))
  }
  hist(means, main=n[j])
}

#chi square
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rchisq(n=n[j], df=4))
  }
  hist(means, main=n[j])
}

#tests
#assume we measure speed (m/s) in a new species of cockroach
#we know the mean for all cockroaches is 1.5 m/s

#is this new species different?

a <- c(1.610493, 1.590881, 1.661052, 1.670567, 1.445126, 1.674019, 1.623131,
       1.442264, 1.530706, 1.612147)

require(BSDA)
#if we know the standard deviation is typically .1 m/s
z.test(x=a,mu = 1.5, sigma.x=.1)

#what if we didn't know the population variance?
#one sample test (against a prescribed mean)
t.test(x= a, mu = 1.5)

#are they faster than average?
t.test(x= a, mu = 1.5, alternative = "greater")

#what if normality assumption doesn't seem to be right, but its symmetrical?
#wilcoxon test
wilcox.test(a, mu=1.5)

#what if we have no previous information on the species and a small, odd distribution
#sign test
#install BSDA package
#assume median is the same as mean
SIGN.test(a, md=1.5)

#NOTICE GENERAL DECREASE IN POWER

#another non-parametic option
#bootstrap
#for more info, http://www.ats.ucla.edu/stat/r/library/bootstrap.htm

#to see how this works, lets return to our airquality dataset
boot.smpl <- matrix(airquality$Temp,length(airquality$Temp),1000)
boot.smpl <- apply(boot.smpl,2,sample,r=T)
boot.md <- apply(boot.smpl,2,mean)
quantile(boot.md,c(.025,.975))

#to do it with a function
require(simpleboot)
boot.mean <- one.boot(airquality$Temp, mean, 10000)
boot.ci(boot.mean)

#below is a function I wrote to give p-values for one and 2 sample bootstrap functions

bootstrapjsg=function(data1, data2=NULL, conf=.95, fun=mean, r=10000, null=0)
{
  if (is.null(data2)){
    a=one.boot(na.omit(data1), fun, r)
    confint=boot.ci(a, conf)
    p=length(subset(a$t, abs(a$t-mean(a$t))>=abs(null-mean(a$t))))/r
    output=c(conf, "% Confidence Interval", confint$percent[1,4:5], "p-value", p)
    return(output)}
  if (is.null(data2)==F){
    a=two.boot(na.omit(data1), na.omit(data2), fun, r)
    confint=boot.ci(a, conf)
    p=length(subset(a$t, abs(a$t-mean(a$t))>=abs(null-mean(a$t))))/r
    output=c(conf, "% Percentile Confidence Interval", confint$percent[1,4:5], "p-value", p)
    return(output)}
}

bootstrapjsg(data1=airquality$Temp, null=72)
bootstrapjsg(data1=a, null=1.5)




#FOR THE FUTURE
#2 sample t tests
#you find another new species in the same forest and wonder, is it the same speed?

b <-  c(1.674370, 1.933666, 1.513185, 1.835873, 2.071608, 1.806574, 1.970640,
        1.765954, 1.712839, 1.748505)

#Bayesian example

#two sample t test
t.test(a,b)

#data in long format
speed <- data.frame( speed = c(a,b), species = c(rep("a",10) ,rep("b",10)))
t.test(speed ~ species, data = speed)

#suppose we wanted to see if exposing the cockroaches in species a to predators impacted their speed,
#so we took speeds for each cockroach in the presence and absence of predators

a_after <- c(1.738624, 1.700627, 1.766018, 1.843045, 1.704468, 1.828324,
             2.114843, 1.608361, 1.528089, 1.726083)

speed_predator <- data.frame( speed = c(a,a_after), predator = c(rep("no",10) ,rep("yes",10)))
t.test(speed ~ predator, data = speed_predator)

#but wait, we know these are the same cockroaches
#so we would expect their orginal speed may impact their final speed

t.test(a, a_after, paired = T)

#why do this?
#accounts for samples among individuals

#


#we can extend this to more than 2 populations using an ANOVA
#same as a linear model!  ANOVA just means we have categories of data (which have
#to be correctly input into R)

#our previous example, 3 ways
aov_speed <- aov(speed ~ species, speed)
summary(aov_speed)
#check
t.test(speed ~ species, data = speed)
#difference here is t.test assumes variance is not equal, while ANOVA does opposite
t.test(speed ~ species, data = speed ,var.equal = T)

#you report ANOVAs F(df1, df2), p-value


