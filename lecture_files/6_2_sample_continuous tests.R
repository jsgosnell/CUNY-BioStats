#graphs and tables from Tests for 2 samples of continuous data lecture
#
#secchi data####
secchi <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/secchi.csv?attredirects=0&d=1")
#plot
#easiest to melt
require(reshape2)
secchi_melted <- melt(secchi)
names(secchi_melted) <- c("Date", "Depth")

require(ggplot2)
ggplot(secchi_melted, aes_string(x= "Date", y = "Depth")) +
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

#example of paired impacts
t.test(secchi$Initial, secchi$Final)
#or 
t.test(Depth ~ Date, secchi_melted)
#vs paired
t.test(secchi$Initial, secchi$Final, paired = T)
#or
t.test(Depth ~ Date, secchi_melted, paired = T) #assumes in proper order!


#smoke####
smoke <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/smoke.csv?attredirects=0&d=1")
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

ggplot(means, aes_string("difference")) +
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
ggplot(means, aes_string("difference")) +
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
ggplot(means, aes_string("difference")) +
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
       , aes_string(x="Group", y="mean")) +
  geom_point(aes_string( colour = "Group"), size = 5) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1.5) +
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
garter <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/garter.csv?attredirects=0&d=1")
wilcox.test(Proportion.of.snakes.resistant ~ Locality, garter)

#sign test example####
insect_speciation <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/insect_speciation.csv?attredirects=0&d=1")
summary(insect_speciation)
insect_speciation$difference <- insect_speciation$Polyandrous_species - 
  insect_speciation$Monandrous_species

insect_speciation$difference

ggplot(insect_speciation, aes_string("difference")) +
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
require(BSDA)
SIGN.test(insect_speciation$Polyandrous_species, insect_speciation$Monandrous_species)
#same as
SIGN.test((insect_speciation$Polyandrous_species - insect_speciation$Monandrous_species), md = 0)

#bootstrap####

#load bootstrapjsgfunction, which requires simpleboot

require(simpleboot)
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

#back to our aussie athlete data
sport <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/sport.csv?attredirects=0&d=1")

ggplot(sport, aes_string("Ferr"))+
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
require(coin)
independence_test(Ferr ~ Sport, sport[sport$Sport %in% c("BBall", "Row"),])

#compare to t-test
t.test(Ferr ~ Sport, sport[sport$Sport %in% c("BBall", "Row"),])

#notes on data manipulation####
#long to wide
cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$day <- as.factor(cholesterol$day)
head(cholesterol)
require(reshape2)
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




