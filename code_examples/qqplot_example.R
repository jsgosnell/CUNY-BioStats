#use code below to assess how data from various distributions visually compares 
#to a normal distribution
#
##normal
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