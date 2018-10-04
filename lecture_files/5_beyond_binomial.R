#graphs and tables from Beyond the Binomial lecture
#
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

#generate chi_sq stats by simulation
#from https://stats.stackexchange.com/questions/14158/how-to-generate-random-categorical-data
n <- 350
num_of_simulations <- 10000
num_of_buckets <- 7
chi_sq_stats <- data.frame(chisq = rep(NA, num_of_simulations))
for(i in 1:num_of_simulations){
simulation <- table(sample(LETTERS[1:num_of_buckets], n, replace=T))
chi_sq_stats$chisq[i] <- chisq.test(simulation)$statistic
}

#plot
require(ggplot2)
#just histogram, scaled to 1
ggplot(chi_sq_stats, aes_string(x="chisq")) +
  geom_histogram(aes(y=..count../sum(..count..)), fill = "orange") +
  ylab(paste("Probablity under ", num_of_simulations, " simulations", 
sep ="")) +
  xlab(expression(chi^2))+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#with curve
ggplot(chi_sq_stats, aes_string(x="chisq")) +
  geom_histogram(aes(y=..count../sum(..count..)), fill = "orange") +
  stat_function(fun = dchisq, args = list(df = 6),size = 3, color = "green") + 
  ylab(paste("Probablity under ", num_of_simulations, " simulations", 
             sep ="")) +
  xlab(expression(chi^2))+
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

ggplot(chi_sq_stats, aes_string(x="chisq")) +
  stat_function(fun = dchisq, args = list(df = 6),size = 3, fill = "green", geom = "area") + 
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "orange") +
  ylab(expression(paste("Probablity under  ", chi^{2}, " distribution", sep = " "))) +
  xlab(expression(chi^2))+
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
1-pchisq(15.24,6)
#can also obtain by simulation (instead of using approximation)
chisq.test(x=days_of_week_births$Births, p= rep(1/7, 7), simulate.p.value = T, B=10000)

#sons example####
#
#chisq.test gives correct stat but wrong degrees of freedom
chisq.test(c(530,1332,582),  p = c(585.3,1221.4,637.3), 
           rescale.p = T)
#without rescaling
chisq.test(c(530,1332,582),  p = c(585.3,1221.4,637.3)/2444)

#goodfit corrects this
require(vcd)
fit_binom <- goodfit(c(rep(0,530),rep(1,1332),rep(2,582)), type = "binomial", 
                     par = list(size = 2), method = "ML")
fit_binom
summary(fit_binom)

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

#this is messier than usual for mean due to table structure
mu <- sum(as.numeric(as.character(soccer_goals$Number_of_goals)) * soccer_goals$Occurences) /
  sum(soccer_goals$Occurences)

#calcuting expected probabilites manually
soccer_goals$Expected_prob <- (exp(-mu) *mu^as.numeric(as.character(soccer_goals$Number_of_goals))) /
                            factorial(as.numeric(as.character(soccer_goals$Number_of_goals)))
#using functions
expected_prob <- dpois(0:8, mu)

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

#to test
chisq.test(soccer_goals$Occurences, p = soccer_goals$Expected_actual, rescale.p = T)
#same as
chisq.test(soccer_goals$Occurences, p = c(dpois(0:7, mu), 1-ppois(7,mu)))
#with goodfit to handle degrees of freedom
#for poisson use table
soccer_table <- as.table(soccer_goals$Occurences)
names(soccer_table) <-soccer_goals$Number_of_goals
fit_pois <- goodfit(soccer_table)
fit_pois$par
summary(fit_pois)

#what assumptions did we violate?
chisq.test(soccer_goals$Occurences, p = c(dpois(0:7, mu), 1-ppois(7,mu)))$expected

#to fix this
soccer_goals_combined <- soccer_goals[1:5,1:2]
soccer_goals_combined
#just add 4th manually (could code)
soccer_goals_combined[5,2] <-sum(soccer_goals[5:9,2])
soccer_goals_combined
#find new mu
mu <- sum(as.numeric(as.character(soccer_goals_combined$Number_of_goals)) * 
            soccer_goals_combined$Occurences) /
  sum(soccer_goals_combined$Occurences)
#run new test
##have to rescale to catch everything above this; could also add up to some 
##larger number to be more accurate
chisq.test(soccer_goals_combined$Occurences, p = dpois(0:4, mu), 
           rescale.p = T)
#or (more accurate)
chisq.test(soccer_goals_combined$Occurences, p = c(dpois(0:3, mu), 
                                                   1 - ppois(3, mu)))
#check assumptions           
chisq.test(soccer_goals_combined$Occurences, p = c(dpois(0:3, mu), 
                                                   1 - ppois(3, mu)))$expected
#looks ok (<20% of groups <5)

soccer_table_new <- as.table(soccer_goals$Occurences[0:5])
names(soccer_table_new) <-soccer_goals$Number_of_goals[0:5]
soccer_table_new[5] <- sum(soccer_table[5:9])
fit_pois <- goodfit(soccer_table_new, method = "ML")
fit_pois$par
summary(fit_pois)



#everest ####
#make a data frame for ggplot2
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

#relative risk
(32/(32+1045))/(8/(8+88))

#likelihood####
#example from http://www.johnmyleswhite.com/notebook/2010/04/21/doing-maximum-likelihood-estimation-by-hand-in-r/
#
#determine most likely value of p for a bernoulli variable
p.parameter <- runif(1)
sequence <- rbinom(10, 1, p.parameter)
#
#write a likelihood function to solve for likelihood
likelihood <- function(sequence, p.parameter)
{
  likelihood <- 1
  
  for (i in 1:length(sequence))
  {
    if (sequence[i] == 1)
    {
      likelihood <- likelihood * p.parameter
    }
    else
    {
      likelihood <- likelihood * (1 - p.parameter)
    }
  }
  
  return(likelihood)
}

#then solve and plot
possible.p <- seq(0, 1, by = 0.001)
library('ggplot2')
likelihood_plot <- qplot(possible.p,
      sapply(possible.p, function (p) {likelihood(sequence, p)}),
      geom = 'line',
      main = paste('Likelihood as a Function of P when p = ', p.parameter),
      xlab = 'P',
      ylab = 'Likelihood')
likelihood_plot +
      theme(axis.title.x = element_text(face="bold", size=28), 
            axis.title.y = element_text(face="bold", size=28), 
            axis.text.y  = element_text(size=20),
            axis.text.x  = element_text(size=20), 
            legend.text =element_text(size=20),
            legend.title = element_text(size=20, face="bold"),
            plot.title = element_text(hjust = 0.5, face="bold", size=32))
  

#gtest####
require(DescTools)
GTest(x = matrix(c(1045, 88, 32, 8), 2, 2, byrow = T))

#relative risk and odds####
fisher.test(x = matrix(c(1045, 88, 32, 8), 2, 2, byrow = T))
#relative risk
(32/(32+1045))/(8/(8+88))

#dolphin activity####
dolphin <- read.table("http://www.statsci.org/data/general/dolpacti.txt", sep="", header = T)
#More info on data @ 
#http://www.statsci.org/data/general/dolpacti.html
#difference in activity among time periods
#easier if you make a table
travel_table <- as.table(matrix(c(6, 28, 38,
                                  6, 4, 5,
                                  14, 0, 9,
                                  13, 56, 10), nrow = 4, byrow = T))
colnames(travel_table) = c("travel", "feed", "social")
rownames(travel_table) = c("morning", "noon", "afternoon", "night")
#now look at it
travel_table
chisq.test(travel_table)
chisq.test(travel_table)$expected #actually ok

require(fifer)
bonf_correct <- chisq.post.hoc(travel_table,
               control = "bonf")
bonf_correct[order(bonf_correct$raw.p),]
holm_correct <- chisq.post.hoc(travel_table,
               control = "holm")
holm_correct[order(holm_correct$raw.p),]
fdr_correct <- chisq.post.hoc(travel_table,
               control = "fdr")
fdr_correct[order(fdr_correct$raw.p),]

#how to use get data in for these tests####

#if data is in long format (not already condensed)
#Let's consider heart attack incidences in the US.  Read in the data from 
#http://statland.org/R/R/heartatk4R.txt", header=T.  
#
heart<-read.table("http://statland.org/R/R/heartatk4R.txt", header=T)
head(heart)
str(heart)


#lets test if heart attacks occur equally across genders, assuming 50% split in population
#how to get tabular data?
table(heart$SEX)
chisq.test(table(heart$SEX))
#same as
chisq.test(c(5065,7779))
#same as
chisq.test(c(5065,7779), p=c(.5,.5))
chisq.test(table(heart$SEX), p=c(.5,.5))

#is there a relationship between gender and diagnosis? to set this  up
table(heart$SEX, heart$DIAGNOSIS)
chisq.test(table(heart$SEX, heart$DIAGNOSIS))

##you can subset data just like always
table(heart[heart$AGE<30, "SEX"], heart[heart$AGE<30, "DIAGNOSIS"])

#matrix reminders####
#putting data in manually
#requires matrix command
#each row is group, so put in by row, specify number of rows, and put byrow = T
table(heart[heart$AGE<30, "SEX"], heart[heart$AGE<30, "DIAGNOSIS"])
#becomes
results <- chisq.test(x = matrix(c(1,3,0,0,0,0,0, 0, 4, 
                                    2,5,0,1,7,0,4, 0, 0), nrow = 2, byrow = T))

#calling up results####
#if you save object you can call p-value
results
#and expected values
results$expected

#same as
chisq.test(x = matrix(c(1,3,0,0,0,0,0, 0, 4, 
                        2,5,0,1,7,0,4, 0, 0), nrow = 2, byrow = T))$expected


#what about more categories
#is smoking independent of exercise
#http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
smoke <- chisq.test(matrix(c(7, 1, 3, #spacing just for visual use
                             87,18,84,
                             12,3,4,
                             9,1,7), nrow = 4, byrow = T))
smoke$expected #too small!
fisher.test(matrix(c(7, 1, 3, #spacing just for visuals
                     87,18,84,
                     12,3,4,
                     9,1,7), nrow = 4, byrow = T))

#what about a dataframe?
#I think this only works for two categories, eg, does sex ration differ among 
#populations
sex_ratio <- data.frame(pop = c("a", "b"), males = c(55, 65), 
                        females = c(25, 27) )
sex_ratio
#not right
chisq.test(sex_ratio$males, sex_ratio$females)
#what about
chisq.test(matrix(c(sex_ratio$males, sex_ratio$females), nrow = 2)) #note byrow = F here by default
#same as
chisq.test(matrix(c(55,25,65,27), byrow = T, nrow = 2))
#or 
chisq.test(cbind(sex_ratio$males, sex_ratio$females))





