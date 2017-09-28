#Normality and t-tests

#qqplots ####
#We've already shown that distributions converge to normality fairly quickly. We
#can also assess normality of data directly using quantile-quantile plots

#in general, generate random samples and compare to normal or actually call the distribution

x <- rnorm(10000, mean = 0, sd = sqrt(10))
#to develop plot
qqnorm(x)
# to add line
qqline(x)
#if data fall near line, it indicates normality

#full code below (run this in blocks) for several of the groups (used in assignment)

#normal
sigma=c(10,100)
par(mfcol=c(2,2), ask=T)
for(j in 1:length(sigma)){
  x=rnorm(1000, 0, sqrt(sigma[j]))
  hist(x)
  qqnorm(x)
  qqline(x)
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

#chi-square
freedom=c(1,10,30,100)
par(mfcol=c(2,4), ask=T)
for(j in 1:length(freedom)){
  x=rchisq(1000, df=freedom[j])
  hist(x, main=freedom[j])
  qqnorm(x)
  qqline(x)
}

#bernoulli
y=0:1
n=1
p=c(.1,.5,.9)
par(mfcol=c(2,2))
for (i in 1:length(p)){
  x=dbinom(y,n,p[i])
  plot(as.factor(y),x, type="h", main=p[i])
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





