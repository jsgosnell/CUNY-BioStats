#graphs and tables from normality and one sample tests portions of lecture####


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

##british spies example ####
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
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_vline(xintercept =.83)

1 - pnorm(.46)

#temperature example####

temp <- c(102, 101,102, 98, 100, 103)

mean(temp)

#z-test examples####
library(BSDA)
z.test(temp, mu = 99, sigma.x = 6)

(1-pnorm(.8165))*2

#plot signal on z distribution####
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
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_vline(xintercept = .81)

#plot confidence interval####
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
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_vline(xintercept = .81) +
  geom_point(aes(x=.81, y = .2), size = 8) +
  geom_segment(aes(x= .81 - 1.96*sqrt(1),  xend = .81 + 1.96*sqrt(1), y=.2, 
                   yend = .2))



sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)

z.test(sport[sport$Sex == "male", "Ht"], mu = 175.6, sigma.x=7)

#qqplots ####

x <- rnorm(10000, mean = 0, sd = sqrt(10))
#to develop plot
qqnorm(x)
# to add line
qqline(x)
#if data fall near line, it indicates normality

#for athlete data
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
bootstrapjsg <- function(data1, data2=NULL, conf=.95, fun=mean, r=10000, null=0)
{
  library(boot)
  one.boot <- function (data, FUN, R, student = FALSE, M, weights = NULL, ...) 
  {
    func.name <- ifelse(is.character(FUN), FUN, deparse(substitute(FUN)))
    extra <- list(...)
    if (func.name == "quantile") {
      if (is.na(match("probs", names(extra)))) 
        stop("'probs' argument must be specified")
      if (length(extra$probs) > 1) 
        stop("can only bootstrap a single quantile")
    }
    func <- match.fun(FUN)
    boot.func <- function(x, idx) {
      fval <- func(x[idx], ...)
      if (student) {
        rs.x <- x[idx]
        b <- one.boot(rs.x, FUN, R = M, student = FALSE, 
                      M = NULL, weights = NULL, ...)
        fval <- c(fval, var(b$t))
      }
      fval
    }
    b <- boot(data, statistic = boot.func, R = R, weights = weights)
    b$student <- student
    structure(b, class = "simpleboot")
  }
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

#bootstrapping athlete data
bootstrapjsg(data1=sport[sport$Sex == "male", "Ht"], null=175.6)

#graphs and tables from Tests for 2 samples of continuous data lecture
#
#secchi data####
secchi <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/secchi.csv")
#plot
#easiest to melt
library(reshape2)
secchi_melted <- melt(secchi)
names(secchi_melted) <- c("Date", "Depth")

library(ggplot2)
ggplot(secchi_melted, aes(x= Date, y = Depth)) +
  geom_point(color = "purple", size = 4) +
  ggtitle("Water clarity measured using Secchi depth") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


secchi$difference <- secchi$Initial - secchi$Final

ggplot(secchi, aes(x= "difference", y = difference)) +
  geom_point(color = "purple", size = 4) +
  ggtitle("Water clarity measured using Secchi depth") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#confidence interval####
library(Rmisc)
difference_summary <- summarySE(secchi, measurevar = "difference")
ggplot(difference_summary, aes(x= .id, y = difference)) +
  geom_point(color = "purple", size = 4) +
  geom_errorbar(aes(ymin= difference - ci, ymax = difference + ci))+
  ggtitle("Water quality has decreased over five years") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=0), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  xlab(NULL)+
  ylab("Difference in water clarity over 5 years")

#example of paired impacts
t.test(secchi$Initial, secchi$Final)
#or 
t.test(Depth ~ Date, secchi_melted)
#vs paired
t.test(secchi$Initial, secchi$Final, paired = T)
#or
t.test(Depth ~ Date, secchi_melted, paired = T) #assumes in proper order!


#smoke####
smoke <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/smoke.csv")
t.test(smoke$Before, smoke$After, paired = T)

#cavity example simulation####
mean_cavity <-(51 * 39.6 + 56 * 43.9)/(51 + 56)
variance_cavity <-((51-1)*9.4 + (56-1)*10.7)/((51 + 56)-2)
#sample
means = data.frame(rep = 1:10000, difference = rep(NA,10000))
for(i in 1:10000){
  means$difference[i] <- mean(rnorm(51, mean_cavity, sd= sqrt(variance_cavity))) - 
    mean(rnorm(56, mean_cavity, sd= sqrt(variance_cavity)))
  
}

ggplot(means, aes(difference)) +
  geom_histogram(aes(y=..count../sum(..count..)), fill = "orange", bins = 15) +
  ggtitle("Signal under null hypothesis") +
  ylab("Probability") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#with line
ggplot(means, aes(difference)) +
  geom_histogram(aes(y=..count../sum(..count..)), fill = "orange", bins = 15) +
  ggtitle("Signal under null hypothesis") +
  ylab("Probability") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_vline(xintercept = 43.9 - 39.6, size = 1.5, color = "orange")

count(abs(means$difference)>= (43.9 - 39.6))

#with t distribution overly
ggplot(means, aes(difference)) +
  geom_histogram(aes(y=..count../sum(..count..)), fill = "orange", bins = 15) +
  stat_function(fun = dt, args = list(df = (56+51-2)),size = 3, color = "green") +   ggtitle("Signal under null hypothesis") +
  ylab("Probability") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_vline(xintercept = 56 - 51, size = 1.5, color = "orange")

2 * (1 - pt(2.213, (56+51-2)))


#unpaired 2-sample test ####
t.test(secchi$Initial, secchi$Final)

#examples of bad data comparison

example_bad <- data.frame(Group = c(rep("A", 25), rep("B",10)), Temperature = 
                            c(rnorm(25,100.2,10), rnorm(10,102.5, 30)))
#make sure summarySE function is loaded
example_bad_summary <- summarySE(example_bad, measurevar = "Temperature", groupvars = "Group")

ggplot(example_bad_summary
       , aes(x=Group, y=Temperature)) +
  geom_point(aes( colour = Group), size = 5) +
  geom_errorbar(aes(ymin=Temperature-se, ymax=Temperature+se), size=1.5) +
  ylab("Temperature (F)")+ggtitle("Temperature based on past vaccine use")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_hline(aes(yintercept=98), size = 3, color = "orange")


#garter snake example####
garter <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/garter.csv")
wilcox.test(Proportion.of.snakes.resistant ~ Locality, garter)

#sign test example####
insect_speciation <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/insect_speciation.csv")
summary(insect_speciation)
insect_speciation$difference <- insect_speciation$Polyandrous_species - 
  insect_speciation$Monandrous_species

insect_speciation$difference

ggplot(insect_speciation, aes(difference)) +
  geom_histogram() +
  ggtitle("Difference in # of species based on promiscuity") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#dependent
library(BSDA)
SIGN.test(insect_speciation$Polyandrous_species, insect_speciation$Monandrous_species)
#same as
SIGN.test((insect_speciation$Polyandrous_species - insect_speciation$Monandrous_species), md = 0)

#bootstrap####

#load bootstrapjsgfunction, which librarys simpleboot

#bootstrapjsg function####
bootstrapjsg <- function(data1, data2=NULL, conf=.95, fun=mean, r=10000, null=0)
{
  library(boot)
  library(simpleboot)
  one.boot <- function (data, FUN, R, student = FALSE, M, weights = NULL, ...) 
  {
    func.name <- ifelse(is.character(FUN), FUN, deparse(substitute(FUN)))
    extra <- list(...)
    if (func.name == "quantile") {
      if (is.na(match("probs", names(extra)))) 
        stop("'probs' argument must be specified")
      if (length(extra$probs) > 1) 
        stop("can only bootstrap a single quantile")
    }
    func <- match.fun(FUN)
    boot.func <- function(x, idx) {
      fval <- func(x[idx], ...)
      if (student) {
        rs.x <- x[idx]
        b <- one.boot(rs.x, FUN, R = M, student = FALSE, 
                      M = NULL, weights = NULL, ...)
        fval <- c(fval, var(b$t))
      }
      fval
    }
    b <- boot(data, statistic = boot.func, R = R, weights = weights)
    b$student <- student
    structure(b, class = "simpleboot")
  }
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


#back to our aussie athlete data
sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)

ggplot(sport, aes(Ferr))+
  geom_histogram() +
  facet_wrap(~Sex) +
  ggtitle("	Plasma ferritin concentration of Australian athletes") +
  xlab("Ferritin concentration")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        strip.text.x = element_text(size = 22))

#have to put in by groups (no formulas!)
bootstrapjsg(sport[sport$Sport == "BBall", "Ferr"], sport[sport$Sport == "Row", "Ferr"])

#permutation####
library(coin)
independence_test(Ferr ~ Sport, sport[sport$Sport %in% c("BBall", "Row"),])

#compare to t-test
t.test(Ferr ~ Sport, sport[sport$Sport %in% c("BBall", "Row"),])


#INTRO TO GGPLOT2####
library(ggplot2)
library(Rmisc)
#ggplot2 is a great plotting package that allows a lot of control over your output
#lets do some examples using the sleep dataset
sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
#
#ggplot2 works in layers so you can or subtract as needed. Provided code is verbose here
#so you can see what its doing.

#first, install and call the package
library(ggplot2)

#to make a plot, first set a base layer
#lets start with a scatter plot and focus on relationship between time spent sleeping
#and time spent dreaming
#first, add your layers
dreaming_sleep_relationship <- ggplot(sleep, aes(x=TotalSleep, y = Dreaming))
#Now call the ggplot object you created
dreaming_sleep_relationship
#Nothing plots except the axes. Now you have to add layers. For example, you can add points
dreaming_sleep_relationship <- ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) + 
  geom_point()
dreaming_sleep_relationship
#Note here I'm saving the object, so to see it I call it. You can also just call directly
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point()
#then you get long calls that are easier (maybe) to manipulate
#Now you have a basic plot.  You can use other arguments in geom_layer commands 
#to add to it or themes or functions to change its look

#geom_ layers without any arguments assume you are using the base layers. You can 
#change this to add extra info to a plot. For example, let's color these by primate
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Primate))

#now we've added information on primates. Note this is different from
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(colour="Primate")
#or
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(colour="blue")
#you have to put things you want to plot in the aes argument area (stands for aesthetics)
#, and anything outside of that changes the entire plot. Also note the 2nd method
#loses the legend as color now conveys no information
#
#this is also a good to talk about renaming factor labels. You may want to change 
#Primate levels to Yes and No for your graph. Lots of ways to do this, but revalue 
#in the plyr package is nice (and we'll use this suite of packages often, same person
#developed ggplot2, plyr, and reshape)
#
library(plyr)
sleep$Taxa <- revalue(sleep$Primate, c(Y = "Primate", N = "Non-primate"))
#notice what I did above. I made a new column from an existing one using a name 
#I might want on a legend
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa))
#I can also just change the legend title directly or change legend text, but often 
#workign with the dataframe is easier for me

#if we wanted these in a different order, we can use relevel to set one as the 
#"first" level (and then do this sequentially to get them in the right order if 
#needed".  You can also change level orders using the factor or ordered functions 
#for multiple levels at once
sleep$Taxa <- relevel(sleep$Taxa, "Primate" )
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa))

# Now lets change the axis labels,sizes, etc using theme function. Again, I left this
# verbose so you can change as needed. Also note the size argument in geom_point
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent dreaming daily") +
  xlab("Average hours spent sleeping daily") +
  ggtitle("Time spent dreaming increases with total sleeping time") +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(size=3),
        panel.grid.major = element_line(colour = "black"),
        plot.background = element_rect(fill="gray"),
        legend.background = element_rect(fill="gray"))

#you can also directly change legend title and colours with the scale_ commands
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent dreaming daily") +
  xlab("Average hours spent sleeping daily") +
  ggtitle("Time spent dreaming increases with total sleeping time") +
  #scale commands help with legends
  scale_colour_manual(name="Type of mammal",values = c("#FFA373","#50486D")) +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(size=3),
        panel.grid.major = element_line(colour = "black"),
        plot.background = element_rect(fill="gray"),
        legend.background = element_rect(fill="gray"))

#in general scale_[whatever you had aes commands]_manual lets you set colors or codes
#to see color codes go to 
#http://sape.inf.usi.ch/quick-reference/ggplot2/colour
#
#You can also facet a graph by type, eg
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent dreaming daily") +
  xlab("Average hours spent sleeping daily") +
  ggtitle("Time spent dreaming increases with total sleeping time") +
  #scale commands help with legends
  scale_colour_manual(name="Type of mammal",values = c("#FFA373","#50486D")) +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(size=3),
        panel.grid.major = element_line(colour = "black"),
        plot.background = element_rect(fill="gray"),
        legend.background = element_rect(fill="gray")) +
  facet_wrap(~Taxa, ncol = 1)

#notice doing this and legend may be redundant, so instead
ggplot(sleep, aes(x=TotalSleep, y = Dreaming)) +
  geom_point(aes(colour=Taxa), size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Average hours spent dreaming daily") +
  xlab("Average hours spent sleeping daily") +
  ggtitle("Time spent dreaming increases with total sleeping time") +
  #scale commands help with legends
  scale_colour_manual(name="Type of mammal",values = c("#FFA373","#50486D")) +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(size=3),
        panel.grid.major = element_line(colour = "black"),
        plot.background = element_rect(fill="gray"),
        legend.background = element_rect(fill="gray"),
        strip.text.x = element_text(size = 18, colour = "purple")) +
  facet_wrap(~Taxa, ncol = 1) +
  guides(colour=FALSE)
# I also added a theme section to change the facet label
# 
# you can save the most recent plot directly to your working directory using
ggsave("Fig1.jpg")
#or just save using rstudio functionality

#ggplot2 is a great example of needing to undertand basic functionality without 
#having to remember everything.the intro class lecture and accompanying code should
#help you get started.  A few other points that often come up are noted below.

#For histograms, you only need one axis (frequency is calculated automatically)
ggplot(sleep, aes(x=Dreaming)) +
  geom_histogram()
#note we can just copy our theme info from above and modify as needed (or ggplot2
#will largely skip un-needed info).  You can also save and name a theme so you 
#don't have to do all this everytime.
ggplot(sleep, aes(x=Dreaming)) +
  geom_histogram() + 
  #below here is ylabel, xlabel, and main title
  ylab("Frequency") +
  xlab("Average hours spent dreaming daily") +
  ggtitle("Distribution of hours spent dreaming") +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(size=3),
        panel.grid.major = element_line(colour = "black"),
        plot.background = element_rect(fill="gray"),
        legend.background = element_rect(fill="gray"),
        strip.text.x = element_text(size = 18, colour = "purple"))



# Finally, remember you can subset the dataframes you feed to the ggplot functions
# (or any other function for that matter). For example, let's just do a histogram 
# of primate sleep.

ggplot(sleep[sleep$Taxa == "Primate",], aes(x=Dreaming)) +
  geom_histogram() + 
  #below here is ylabel, xlabel, and main title
  ylab("Frequency") +
  xlab("Average hours spent dreaming daily") +
  ggtitle("Distribution of hours spent dreaming") +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(size=3),
        panel.grid.major = element_line(colour = "black"),
        plot.background = element_rect(fill="gray"),
        legend.background = element_rect(fill="gray"),
        strip.text.x = element_text(size = 18, colour = "purple"))
#not interesting, but you get the idea
#
##ESTIMATION, BARCHARTS, AND FUNCTIONS ####
#
#Estimating is a key part of statistics and should include the value you are 
#estimating and an estimate of uncertaintly. We'll tie this into statistical tests
#next.  

#Let's go back to the sleep dataset and estimate the average total sleep time speed 
#for each exposure level
#First, lets change exposure to factors and label them
str(sleep) #just a reminder
sleep$Exposure <- factor(sleep$Exposure)
#check levels
levels(sleep$Exposure)
#relabel if you want (just for example here)
levels(sleep$Exposure)<- c("Least","Less", "Average", "More", "Most") 

#Let's just focus on least first to calculate confidence intervals. note we have 
#to remove NA
mean(sleep[sleep$Exposure == "Least", "TotalSleep"], na.rm = T)
#this is our estimate.  The standard deviation of this estimate is
sd(sleep[sleep$Exposure == "Least", "TotalSleep"], na.rm = T) / 
  sqrt(length(sleep[sleep$Exposure == "Least" & is.na(sleep$TotalSleep) == F, "TotalSleep"]))
# is equivalent to
sd(sleep[sleep$Exposure == "Least", "TotalSleep"], na.rm = T) / 
  sqrt(length(na.omit(sleep[sleep$Exposure == "Least", "TotalSleep"])))
#we also call this the standard error of the mean. If we assume the estimate 
#(not the data!) is normally distributed, we can assume things about uncertainty.
#Namely, we can build a 95% confidence interval around our estimate (meaning the true mean
#is in the range 95 out of 100 times we create a sample).  
#
#These ranges need to be added to barcharts (or anything that shows estimates, which
#barcharts are not really great for) to show uncertainty.  We can do this usign 
#a function from the Rmisc package, as ggplot2 doesn't have it built in (maybe because
#bar charts are a bad idea?).
#

sleep_by_exposure <- summarySE(sleep, measurevar = "TotalSleep", groupvars = "Exposure", na.rm = T)
#look at it
sleep_by_exposure
ggplot(sleep_by_exposure
       , aes(x=Exposure, y=TotalSleep)) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=TotalSleep-ci, ymax=TotalSleep+ci), size=1.5) +
  ylab("Total sleep (hours per day")+ggtitle("Sleep across different taxa")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#or better
ggplot(sleep_by_exposure
       , aes(x=Exposure, y=TotalSleep)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=TotalSleep-ci, ymax=TotalSleep+ci), size=1.5) +
  ylab("Total sleep (hours per day")+ggtitle("Sleep across different taxa")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#let's practice source again by opening the central_limit_theorem.R script from
#the Examples folder on github, sourcing it, and looking at the plots it produce. 
#Note they show how going from samples of 1 to higher numbers increases the ability of 
#the means of data from distributions of multiple shapes to approach normality.


#notes on data manipulation####
#long to wide
cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$day <- as.factor(cholesterol$day)
head(cholesterol)
library(reshape2)
#formula gives row ~ columns
#get daily cholesterol for every patient
cholesterol_wide <- dcast(data = cholesterol, formula = patient ~ day, 
                          value.var ="cholest" )
head(cholesterol_wide)
#use fun.aggregate to get other option
#get average cholesterol per patient
#simple function to exclude na's (other option is to melt first and then drop them)
meannona <- function (x) mean(x, na.rm=T)
cholesterol_wide <- dcast(data = cholesterol, formula = patient ~ ., #. means no variable, ... means all variables
                          value.var ="cholest", fun.aggregate = meannona )
head(cholesterol_wide)

#can also name output column by putting in quotes in formula
cholesterol_wide <- dcast(data = cholesterol, formula = patient ~ "cholest", #. means no variable, ... means all variables
                          value.var ="cholest", fun.aggregate = meannona )
head(cholesterol_wide)

#wide to long
cholesterol <- read.table("http://www.statsci.org/data/general/cholestr.txt", header = T)
head(cholesterol)

#id.vars lists independent values to keep
#measure.vars is what you are measuring (not used here, and used rarely)
#variable.name lists what to label things you group
#value.name gives name to value output
cholesterol_long <- melt(cholesterol, id.vars =c())
head(cholesterol_long)

#name outcomes
cholesterol_long <- melt(cholesterol, id.vars =c(), variable.name = "day", 
                         value.name = "cholesterol")
head(cholesterol_long)

#more id variables
sport_melted <- melt(sport, id.vars = c("Sex", "Sport"),
                     variable.name = "measure", 
                     value.name = "value")
head(sport_melted)



