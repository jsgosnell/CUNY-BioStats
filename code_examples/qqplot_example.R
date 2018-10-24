#use code below to assess how data from various distributions visually compares 
#to a normal distribution
#
##normal
sigma=c(1,10,100)
par(mfcol=c(2,3), oma = c(3,2,2,2), ask=F)
for(j in 1:length(sigma)){
  x=rnorm(1000, 0, sqrt(sigma[j]))
  hist(x, main=paste("Normal distribution, variance = ",sigma[j], sep = ""), col = "purple")
  qqnorm(x)
  qqline(x)
}
mtext("1000 Random points drawn from various normal distributions centered at 0", outer = T, side = 1, 
      cex = 2, line = 1)

readline(prompt="Press [enter] to continue")


#t
freedom=c(1,10,30,100)
par(mfcol=c(2,4), oma = c(3,2,2,2), ask=F)
for(j in 1:length(freedom)){
  x=rt(1000, df=freedom[j])
  hist(x, main=paste("t distribution, df = ",freedom[j], sep = ""), col = "purple")
  qqnorm(x)
  qqline(x)
}
mtext("1000 Random points drawn from various t distributions", outer = T, side = 1, 
      cex = 2, line = 1)

readline(prompt="Press [enter] to continue")

#chi-square
freedom=c(1,10,30,100)
par(mfcol=c(2,4), oma = c(3,2,2,2), ask=F)
for(j in 1:length(freedom)){
  x=rchisq(1000, df=freedom[j])
  hist(x, main=paste("chi square distribution, df = ",freedom[j], sep = ""), col = "purple")
  qqnorm(x)
  qqline(x)
}
mtext("1000 Random points drawn from various chi square distributions", outer = T, side = 1, 
      cex = 2, line = 1)

readline(prompt="Press [enter] to continue")

#bernoulli
y=0:1
n=1
p=c(.1,.25, .5,.75, .9)
par(mfcol=c(2,3), oma = c(3,2,2,2), ask=F)
for (i in 1:length(p)){
  x=dbinom(y,n,p[i])
  plot(as.factor(y),x, type="h",main = paste("Bernoulli distribution, p = ",p[j], sep = ""))
}
mtext("Random draw from various Bernoulli distributions", outer = T, side = 1, 
      cex = 2, line = 1)

readline(prompt="Press [enter] to continue")


#binomial
n=c(2,5,25,50)
p=c(.05, .25, .5, .75, .95)
par(mfcol=c(2,2), ask=F, oma=c(3,3,3,3))
for(j in 1:length(p)){
  for (i in 1:length(n)){
    y=0:n[i]
    x=dbinom(y,n[i],p[j])
    plot(as.factor(y),x, type="h", main=paste("Sample size = ", n[i]))
  }
  mtext(paste("n draws from Binomial distributions with p= ", p[j]), outer = T, side = 1, 
        cex = 2, line = 1)
  
  readline(prompt="Press [enter] to continue")
  }

#poisson
u=c(2,5,10,30,50,100)
par(mfcol=c(2,3), ask=F, oma=c(3,3,3,3))
for(j in 1:length(u)){
  x=dpois(x=0:(u[j]*3),lambda=u[j])
  plot(0:(u[j]*3),x, type="h", main=paste("mu = ", u[j]), xlab = "")
}
mtext(paste("Poisson distributions with mu= ", u[j]), outer = T, side = 1, 
      cex = 2, line = 1)