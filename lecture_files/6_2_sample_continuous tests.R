#graphs and tables from Tests for 2 samples of continuous data
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
#dependent
SIGN.test(insect_speciation$Polyandrous_species, insect_speciation$Monandrous_species)
#same as
SIGN.test((insect_speciation$Polyandrous_species - insect_speciation$Monandrous_species), md = 0)












##days of weeks births####
##
##
days_of_week_births <- data.frame(Days = c("Sunday", "Monday", "Tuesday", 
                                           "Wednesday", "Thursday", "Friday", 
                                           "Saturday"), Births = 
                                    c(33,41,63,63,47,56,47))

require(ggplot2)
ggplot(days_of_week_births, aes_string(x= "Days", y = "Births")) +
  geom_col(fill = "orange") +
  ggtitle("Births each day of the week for sample in 1999") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#using the chisq distribution####
using_distribution = dchisq(0:50,6, 1/7)
using_distribution
sum(using_distribution)
x <- c(0:50)
pdf <- data.frame(x, using_distribution)

require(ggplot2)

#just histogram
ggplot(pdf, aes_string(x="x", y = "using_distribution")) +
  geom_col(fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#with curve
ggplot(pdf, aes_string(x="x", y = "using_distribution")) +
  geom_col(fill = "orange") +
  stat_function(fun = dchisq, args = list(df = 6),size = 3, color = "green") + 
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#filled in

dnorm_one_sd <- function(x){
  norm_one_sd <- dchisq(x, df = 6)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x >= qchisq(.95, df = 6)] <- NA
  return(norm_one_sd)
}


ggplot(pdf, aes_string(x="x", y = "using_distribution")) +
  stat_function(fun = dchisq, args = list(df = 6),size = 3, fill = "green", geom = "area") + 
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

#getting p-values
#can input p for each category, or it assumes equal
chisq.test(days_of_week_births$Births)
chisq.test(days_of_week_births$Births, p= rep(1/7, 7))
#or
1-pchisq(15.05,6,1/7)
chisq.test(days_of_week_births$Births, p= rep(1/7, 7), simulate.p.value = T, B=10,000)



#soccer goals ####
soccer_goals <- data.frame(Number_of_goals = factor(0:8), Occurences = 
                                    c(37,47,27,13,2,1,0,0,1))

ggplot(soccer_goals, aes_string(x= "Number_of_goals", y = "Occurences")) +
  geom_col(fill = "orange") +
  xlab("Number of goals") +
  ylab("Occurences") +
  ggtitle("Goals scored per game in 2002 World Cup") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

mu <- sum(as.numeric(as.character(soccer_goals$Number_of_goals)) * soccer_goals$Occurences) /
  sum(soccer_goals$Occurences)

soccer_goals$Expected_prob <- (exp(-mu) *mu^as.numeric(as.character(soccer_goals$Number_of_goals))) /
                            factorial(as.numeric(as.character(soccer_goals$Number_of_goals)))
soccer_goals$Expected_actual <- soccer_goals$Expected_prob * sum(soccer_goals$Occurences)


#could reshape and add legend, but maybe later
ggplot(soccer_goals, aes_string(x= "Number_of_goals", y = "Occurences")) +
  geom_col(fill = "orange", color = "orange") + 
  geom_point(aes_string(y="Expected_actual"), color = "blue", size = 3) +
  xlab("Number of goals") +
  ylab("Occurences") +
  ggtitle("Goals scored per game in 2002 World Cup") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#everest ####
everest <- data.frame(Survived = c("Y","N","Y", "N"),
                      Oxygen = c("Used", "Used", "Not used", "Not used"),
                      Number = c(1045, 32, 88, 8))
#mosaic plot####
#
ggplot(everest, aes_string(x= "Survived", y = "Number")) +
  geom_col(aes_string(fill = "Oxygen")) + 
  xlab("Survived?") +
  ylab("Occurences") +
  ggtitle("Oxygen use impacts Everest descent outcomes") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#make a mosaic
require(reshape2)
number_oxygen <- dcast(everest, Survived ~ "total_per_group", value.var = "Number", sum)
everest <- merge (everest, number_oxygen)
everest$Proportion <- everest$Number/everest$total_per_group
ggplot(everest, aes_string(x= "Survived", y = "Proportion")) +
  geom_col(aes_string(fill = "Oxygen")) + 
  xlab("Survived?") +
  ylab("Proportion") +
  ggtitle("Oxygen use impacts Everest descent outcomes") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#each row is group
#does survival dependon oxygen use?
results <- chisq.test(x = matrix(c(1045, 88, 32, 8), 2, 2, byrow = T))
results$expected
results

#fisher's exact test
fisher.test(x = matrix(c(1045, 88, 32, 8), 2, 2, byrow = T))

#wren example
#does song depend on DNA
chisq.test(x = matrix(c(12, 0, 0, 4), 2, 2, byrow = T))
fisher.test(x = matrix(c(12, 0, 0, 4), 2, 2, byrow = T))

#gtest
require(DescTools)


GTest(x = matrix(c(12, 0, 0, 4), 2, 2, byrow = T))