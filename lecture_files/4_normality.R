#graphs and tables from normality lecture


library(ggplot2)

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -1 | x >= 1] <- NA
  return(norm_one_sd)
}

dnorm_two_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -2 | x >= 2] <- NA
  return(norm_one_sd)
}
#standard normal with 1 and 2 se highlighted ####
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, fill = "black",
                geom = "area")+
  stat_function(fun = dnorm_two_sd, geom = "area", fill = "purple") +
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#qqplots ####

x <- rnorm(10000, mean = 0, sd = sqrt(10))
#to develop plot
qqnorm(x)
# to add line
qqline(x)
#if data fall near line, it indicates normality
#
#z-test example####
sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)

library(BSDA)
z.test(sport[sport$Sex == "male", "Ht"], mu = 175.6, sigma.x=7)

qqnorm(sport[sport$Sex == "male", "Ht"])
qqline(sport[sport$Sex == "male", "Ht"])


#increasing sample size####

#normal
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T, cex = 1.2, cex.main = 2)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rnorm(n=n[j]))
  }
  hist(means, main=paste("Normal, n = ",n[j], sep = ""), col = "orange", xlim = c(-5,5))
}

#binomial
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), oma = c(3,2,2,2), ask=F, cex = 1.2, cex.main = 2)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rbinom(n=n[j], 1, .7))
  }
  hist(means, main=paste("Binomial, n = ",n[j], sep = ""), col = "orange")
}
mtext("Draws from a binomial distribution, p = .7", outer = T, side = 1, 
      cex = 3, line = 1)

#spider example
binom.test(31,41)
library(binom)
binom.confint(31,41)


#compare t and normal distribution####
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, color = "orange",
                geom = "line") + 
  stat_function(fun = dt, n = 101, args = list( df= 3), size = 3, color = "purple",
                geom = "line") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#t-test with athlete data####
t.test(sport[sport$Sex == "male", "Ht"], mu = 175.6)


#temperature example####
#show where -1.679 lies on t distribution #####
qt(.975,9)
dt_one_sd <- function(x, df){
  norm_one_sd <- dt(x, df)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -2.26 | x >= 2.262] <- NA
  return(norm_one_sd)
}
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dt, n = 101, args = list( df= 24), size = 3, fill = "purple",
                geom = "area") +
  stat_function(fun = dt_one_sd, args = list( df= 24), geom = "area", fill = "orange") +
    ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#add vline to show sample value####

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dt, n = 101, args = list( df= 24), size = 3, fill = "purple",
                geom = "area") +
  stat_function(fun = dt_one_sd, args = list( df= 24), geom = "area", fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  geom_vline(xintercept = -1.679, color = "blue")

#wilcoxon-test with athlete data####
wilcox.test(sport[sport$Sex == "male", "Ht"], mu = 175.6)

#sign-test with athlete data####
SIGN.test(sport[sport$Sex == "male", "Ht"], md = 175.6)

#bootstrap####
#back to our fake uniform distributin from the estimation_lecture
#
#create "population" of males

population_size <- 10000
set.seed(42)
population_unif <- data.frame(id = 1:population_size, 
                              height = runif(population_size, 60, 80))

ggplot(population_unif, aes(height)) + 
  geom_histogram(size=3) +
  xlab("Height (in)")+
  ylab("Frequency")+
  ggtitle("Height of all males in our fake uniform population")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#sample a 100 of these

sample_unif_1 <- population_unif[sample(nrow(population_unif), 100),]
ggplot(sample_unif_1, aes(height)) + 
  geom_histogram(size=3) +
  xlab("Height (in)")+
  ylab("Frequency")+
  ggtitle("Height of 100 random males in our fake uniform population")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#now do this a 1000 times and record it
number_of_samples <- 1000
sample_outcomes <- data.frame(mean = rep(NA, number_of_samples), sd = NA)

for (i in 1:number_of_samples){
  sample_unif_1 <- population_unif[sample(nrow(population_unif), 100),]
  sample_outcomes$mean[i] <- mean(sample_unif_1$height)
  sample_outcomes$sd[i] <- sd(sample_unif_1$height)
  
}

ggplot(sample_outcomes, aes(mean)) + 
  geom_histogram(size=3) +
  xlab("Mean height (in)")+
  ylab("Frequency")+
  ggtitle("Mean height of 100 males in our uniform population samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
#
#now bootstrap the signal using sample size of 100####
boot.smpl <- matrix(runif(100,60,80),length(runif(100,60,80)),1000)
boot.smpl <- apply(boot.smpl,2,sample,r=T)
boot.md <- data.frame(mean = apply(boot.smpl,2,mean))

ggplot(boot.md, aes(mean)) + 
  geom_histogram(size=3) +
  xlab("Mean height (in)")+
  ylab("Frequency")+
  ggtitle("Bootstrap distribution of mean height for 100 males \n in our uniform population samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#now bootstrap the signal using sample size of 25####
boot.smpl <- matrix(runif(25,60,80),length(runif(25,60,80)),1000)
boot.smpl <- apply(boot.smpl,2,sample,r=T)
boot.md <- data.frame(mean = apply(boot.smpl,2,mean))

ggplot(boot.md, aes(mean)) + 
  geom_histogram(size=3) +
  xlab("Mean height (in)")+
  ylab("Frequency")+
  ggtitle("Bootstrap distribution of mean height for 25 males \n in our uniform population samples")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#bootstrapjsg function####
#source from github
##make sure you have the boot and simpleboot packages installed
source("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/code_examples/bootstrapjsg.R")

#bootstrapping athlete data
bootstrapjsg(data1=sport[sport$Sex == "male", "Ht"], null=175.6)

