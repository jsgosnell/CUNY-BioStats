#figures and tests from ANOVA lecture
#iris example, from first lecture
#
require(ggplot2)

#point plot####
ggplot(iris, aes_string("Species","Sepal.Length")) + 
  geom_point(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#bar chart with error bars ####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  #  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

function_output <- summarySE(iris, measurevar="Sepal.Length", groupvars =
                               c("Species"))

ggplot(function_output, aes_string(x="Species", y="mean")) +
  geom_col(aes_string(fill="Species"), size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#boxplot####
ggplot(iris, aes_string("Species","Sepal.Length")) + 
  geom_boxplot(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#stacked histogram####
ggplot(iris, aes_string("Sepal.Length")) + 
  geom_histogram(aes_string(fill="Species"), size=3) +
  xlab("Sepal Length (cm)")+
  ylab("Frequency")+
  ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#facet####
ggplot(iris, aes_string("Sepal.Length")) + 
  geom_histogram(aes_string(fill="Species"), size=3) +
  xlab("Sepal Length (cm)")+
  ylab("Frequency")+
  ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  facet_wrap(~Species, ncol = 1)

#point plot with means####
ggplot(iris, aes_string("Species","Sepal.Length")) + 
  geom_point(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_hline(aes(yintercept=mean(iris$Sepal.Length)), size = 2, color = "orange")

#point plot with means for each group####
#make extra column for output and rename here
function_output$Sepal.Length <- function_output$mean
ggplot(iris, aes_string("Species","Sepal.Length")) + 
  geom_point(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_errorbar(aes(ymin=mean, ymax=mean), size=1.5, 
                data = function_output, color = "black")

#combine these two ####
ggplot(iris, aes_string("Species","Sepal.Length")) + 
  geom_point(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_errorbar(aes(ymin=mean, ymax=mean), size=1.5, 
                data = function_output, color = "black") +
  geom_hline(aes(yintercept=mean(iris$Sepal.Length)), size = 2, color = "orange")

#show null hypothesis
ggplot(iris, aes_string("Species","Sepal.Length")) + 
  geom_point(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_errorbar(aes(ymin=mean, ymax=mean), size=1.5, 
                data = function_output, color = "black") +
  geom_hline(aes(yintercept=mean(iris$Sepal.Length)), size = 2, color = "orange") +
geom_errorbar(aes(ymin=mean(iris$Sepal.Length), ymax=mean(iris$Sepal.Length)), size=1.5, color = "purple", linetype = 3) 



#p value via simulation ####
#get overall variance and mean estimate
variance_estimate <- sum((function_output$N -1) * (function_output$sd)^2)/(sum(function_output$N)-length(function_output$N))
mean_sepal <- mean(iris$Sepal.Length)
#sample
  ratio <- data.frame(rep = 1:10000, mse = rep(NA,10000), 
                      msg = rep(NA,10000), ratio = rep(NA,10000))
for(i in 1:10000){
    setosa <- rnorm(50, mean_sepal, sd= sqrt(variance_estimate))
    versicolor <- rnorm(50, mean_sepal, sd= sqrt(variance_estimate))
    virginica <- rnorm(50, mean_sepal, sd= sqrt(variance_estimate))
    mean_overall <- mean(c(setosa, versicolor, virginica))
    ratio$mse[i] <- (49 * var(setosa) + 49 * var(versicolor) + 49 * var(virginica))/(150 - 3)
    ratio$msg[i] <- (50 * (mean(setosa)-mean_overall)^2 + 
                 50 * (mean(versicolor)-mean_overall)^2 + 
                 50 * (mean (virginica)-mean_overall)^2)/2
    ratio$ratio[i] <- ratio$msg[i]/ratio$mse[i]
}
  
summary(lm(Sepal.Length~Species, iris))$fstatistic[1]
  
  
ggplot(ratio, aes_string("ratio")) +
    geom_histogram(aes(y=..count../sum(..count..)), fill = "orange", bins = 15) +
    ggtitle("Ratio under null hypothesis") +
    ylab("Probability") +
    theme(axis.title.x = element_text(face="bold", size=28), 
          axis.title.y = element_text(face="bold", size=28), 
          axis.text.y  = element_text(size=20),
          axis.text.x  = element_text(size=20), 
          legend.text =element_text(size=20),
          legend.title = element_text(size=20, face="bold"),
          plot.title = element_text(hjust = 0.5, face="bold", size=32))
  
    
#with f####
#notice i specify degrees of freedom for numerator (df1) and denomitor (df2)
ggplot(ratio, aes_string("ratio")) +
    geom_histogram(aes(y=..count../sum(..count..)), fill = "orange", 
                   breaks = seq(0,15,1)) +
    stat_function(fun = df, args = list(df1 =2, df2 = 147),size = 3, color = "green") +   
    ggtitle("Signal under null hypothesis") +
    ggtitle("Ratio under null hypothesis") +
    ylab("Probability") +
    theme(axis.title.x = element_text(face="bold", size=28), 
          axis.title.y = element_text(face="bold", size=28), 
          axis.text.y  = element_text(size=20),
          axis.text.x  = element_text(size=20), 
          legend.text =element_text(size=20),
          legend.title = element_text(size=20, face="bold"),
          plot.title = element_text(hjust = 0.5, face="bold", size=32))
  
#build model####
iris_anova <- lm(Sepal.Length~Species, iris)
#it creates an object you can manipulate

#check assumptions####
par(mfrow = c(2,2))
plot(iris_anova)

#look at outputs using p-values
summary(iris_anova)

#you can get specific group means by builidng model without intercept
iris_anova_no_intercept <- lm(Sepal.Length~Species - 1, iris)
summary(iris_anova_no_intercept)
#but don't do this in general (R and F stats will be skewed!)
#just use to get group coefficients if needed
#note relationship between model coefficients here as well

#Anova command from car package is needed to give overall group p-value
require(car)
Anova(iris_anova, type = "III")

#since our overall anova was significant, we need to carry out multiple comparisons
#to see what groups are driving the difference
#multcomp
require(multcomp)

#where's the almost difference? use Tukey's HSD for all pairs
require(multcomp)
compare_cont_tukey <- glht(iris_anova, linfct = mcp(Species = "Tukey"))
summary(compare_cont_tukey)

#or you can specify what you want to consider
compare_virginica_only <- glht(iris_anova, linfct = mcp(Species = 
                                                                c("virginica - versicolor = 0", 
                                                                  "virginica - setosa = 0")))
#remember to control for error rates!
summary(compare_virginica_only, test=adjusted("holm")) 
#other options
summary(compare_virginica_only, test=adjusted("fdr")) 
#we can "find" significance more easily, but you need to justify why you did this
#using set contrasts####
#are versicolor and virginica different from setosa
contr <- rbind("setosa - virginica - setosa" = c(-1,.5,.5))
summary(glht(iris_anova, linfct = mcp(Species = contr)))
summary(iris_anova) # this appears to be right

#orthogonal
contr <- rbind("setosa - virginica - setosa" = c(-1,.5,.5),
               "virginica - setosa" = c(0,-1,1))
summary(glht(iris_anova, linfct = mcp(Species = contr)))
summary(iris_anova) # this appears to be right

#R2####
#R2 explains how much variation is explained by our model (groups in this instance)
summary(iris_anova)
#or
summary(iris_anova)$r.squared
#note you can have a low p-value model that also explains only small amount of 
#variation in the data
#
#

#graphical comparison####
#add comparison portion to fuction_output
function_output$comparison <- "NA"
#enter by hand for small groups by comparing function_output means with multcomp 
#output (usually tukey)
function_output
summary(compare_cont_tukey)
#all different here, so
function_output$comparison <- letters[1:3]
ggplot(function_output, aes_string(x="Species", y="mean")) +
  geom_point(aes_string(fill="Species"), size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  #offset from mean by hand
  geom_text(aes(x = Species, y = mean + .35, label = comparison), size = 28)

#another option for labelling if you are doing tukey is to automate (easy for more comparisons)
post_hoc_spray_cld <- cld(compare_cont_tukey)
#fortify to make a dataframe
post_hoc_spray_cld_fortify <- fortify(post_hoc_spray_cld)
#rename lhs to group and then merge (rename to whatever you used as groupvar in 
#summary SE)
names(post_hoc_spray_cld_fortify)[names(post_hoc_spray_cld_fortify) == "lhs"] <- "Species"
function_output <- merge(function_output, post_hoc_spray_cld_fortify)
#plot
ggplot(function_output, aes_string(x="Species", y="mean")) +
  geom_point(aes_string(fill="Species"), size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  #offset from mean by hand
  geom_text(aes(x = Species, y = mean + .35, label = letters), size = 28)


#kruskal.wallis####
kruskal.test(Sepal.Length ~ Species, data = iris)
pairwise.wilcox.test(iris$Sepal.Length, 
                          iris$Species, 
                          p.adjust.method="holm")

#building to bootstrap options####
require(WRS2)
#first, just using trimmed means, which help
t1way(Sepal.Length~Species, iris)
contrasts <- lincon(Sepal.Length~Species, iris)
contrasts

#if we don't trim mean, we use Welch's approximation for ANOVA
t1way(Sepal.Length~Species, iris, tr = 0)
contrasts <- lincon(Sepal.Length~Species, iris, tr = 0)
contrasts

#to actually use the bootstrap version
t1waybt(Sepal.Length~Species, iris)
bootstrap_post_hoc <- mcppb20(Sepal.Length~Species, iris)
#use p.adjust to correct for FWER
p.adjust(as.numeric(bootstrap_post_hoc$comp[,6]), "holm")

#from lecture portion on interactions and blocking####

#cholesterol example####

cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$day <- as.factor(cholesterol$day)
cholesterol$patient <- as.factor(cholesterol$patient)
head(cholesterol)
summary(cholesterol)


#bar chart with error bars ####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  #  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

function_output <- summarySE(cholesterol, measurevar="cholest", groupvars =
                               c("day"), na.rm = T)
require(ggplot2)
ggplot(function_output, aes_string(x="day", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Cholesterol level")+ggtitle("Cholesterol level following a heart attack")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#run an lm####
cholest <- lm(cholest ~ day, cholesterol)
par(mfrow = c(2,2))
plot(cholest)
require(car)
Anova(cholest, type = "III")
summary(cholest)
require(multcomp)
comp_cholest <- glht(cholest, linfct = mcp(day = "Tukey"))
summary(comp_cholest)

#but our data was paired!
#t test from quiz####
t.test(cholesterol[cholesterol$day == "2", "cholest"], 
       cholesterol[cholesterol$day == "4", "cholest"],
       paired = T)

#what about blocking (in addition to extra information on variance)
#to show this is the same as pairing. We can add extra factor with a "+" sign on 
#right-hand side of equation
cholest_blocked <- lm(cholest ~ day + patient, cholesterol[cholesterol$day %in% 
                                                             c("2","4"),])
plot(cholest_blocked)
summary(cholest_blocked)
Anova(cholest_blocked, type = "III") #now has same p-value as paired t-test


#idea can extend to all days
cholest_blocked_all_days <- lm(cholest ~ day + patient, na.omit(cholesterol))
plot(cholest_blocked_all_days)
summary(cholest_blocked_all_days)
Anova(cholest_blocked_all_days, type = "III")

#extension to linear model####
#heres the top row
model.matrix(cholest_blocked_all_days)[1,]
coef(cholest_blocked_all_days)

#Advanced aside and check for Stephen-SKIP
# #compare to aov
# cholest_aov = aov(cholest ~ day + Error(patient), na.omit(cholesterol))
# summary(cholest_aov)
# #works ok here as theres no other other factor. Another factor would require the
# #use of a more complex error designation in aov or lme (what I typically use) 
# because then you need to create the F ratio by dividing mse of treatment by mse
# of within subjects

comp_cholest_blocked_all_days <- glht(cholest_blocked_all_days, linfct = mcp(day = "Tukey"))
summary(comp_cholest_blocked_all_days) #now we see difference

#compare again to
Anova(cholest, type = "III")
summary(comp_cholest)

#what if we care about other factor
#2-way ANOVA ####
memory <- read.table("http://www.statsci.org/data/general/eysenck.txt", header = T)
require(plyr)
memory$Age <- relevel(memory$Age, "Younger")

#graph data with line but no error bar
#add extra call to groupvars in summarySE
function_output <- summarySE(memory, measurevar="Words", groupvars =
                               c("Age", "Process"), na.rm = T)

ggplot(function_output, aes_string(x="Age", y="mean",color="Process", 
                                   shape = "Process")) +
  geom_line(aes_string(group="Process", linetype = "Process"), size=2) +
  geom_point(size = 5) +
  #  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Words remembered")+ 
  xlab("Age") + 
  ggtitle("Process type interacts with age to impact memory")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#build model####
memory_interactions <- lm(Words ~ Age * Process, memory)

#linear model impacts
model.matrix(memory_interactions)[1,]
coefficients(memory_interactions)

#check assumptions
par(mfrow=c(2,2))
plot(memory_interactions)

summary(memory_interactions)
Anova(memory_interactions, type = "III")

#example one-way anova for age due to significant interaction####
memory_interactions_young <- lm(Words ~ Process, memory[memory$Age == "Younger",])
plot(memory_interactions_young)
Anova(memory_interactions_young)
comp_young <- glht(memory_interactions_young, linfct = mcp(Process = "Tukey"))
summary(comp_young)

#all with lsmeans
require(lsmeans)
lsmeans(memory_interactions, pairwise ~ Process | Age)

#interpret coef (subtract intercept for ease)
coef(update(memory_interactions, .~.-1))

#example of no interaction####
#modified version of biomass project
biomass <- read.csv("http://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/biomass_experiment.csv")
function_output <- summarySE(biomass, measurevar="Mass", groupvars =
                               c("PredSize", "PredNumber"), na.rm = T)
#make NA's control
function_output$PredSize <- as.character(function_output$PredSize)
function_output$PredNumber <- as.character(function_output$PredNumber)
function_output[5,1:2]  <- "Control"
#change back to factor to change order
function_output$PredSize <- factor(function_output$PredSize, 
                                   levels = c("Control", "Small", "Large"))
ggplot(function_output, aes_string(x="PredSize", y="mean",color="PredNumber", 
                                   shape = "PredNumber")) +
  geom_point(size = 5) +
  geom_line(aes_string(group="PredNumber", linetype ="PredNumber"), size=2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Oyster mass (g)")+ 
  xlab("Conch Size") + 
  scale_color_discrete(name = "Number of Conchs")+
  scale_shape_discrete(guide=FALSE)+
  scale_linetype_discrete(guide=FALSE)+
  ggtitle("Impacts of conch biomass on oyster growth")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#example of dropping if interactions not significnt
#remove controls here
size_number_interactions <- lm(Mass ~ PredSize * PredNumber, biomass[is.na(biomass$PredNumber) != T, ])
plot(size_number_interactions)
Anova(size_number_interactions, type = "III")

#drop interaction term
size_number_lm <- lm(Mass ~ PredSize + PredNumber, biomass[is.na(biomass$PredNumber) != T, ])
plot(size_number_lm)
Anova(size_number_lm, type = "III")

#other options#
require(WRS2) 
#have to get rid of any empty levels!
oyster_reduced <- oyster[oyster$Predator %!in% c("None"),]
oyster_reduced$Predator <- factor(oyster_reduced$Predator)
oyster_reduced$Exposure <- factor (oyster_reduced$Exposure)
t2way(Mass ~ Predator * Exposure, data = oyster_reduced)
mcp2atm(Mass ~ Predator * Exposure, data = oyster_reduced)


#ADVANCED ASIDE - SKIP
# #main value of multcomp:glht is doing targeted commands, eg looking at model without "none"
# #is conch and crabs more than the combined effect of other two
# conch_crabs_compare <- glht(oyster_lm_predator, linfct = mcp(Predator = c("Conchs_and_crabs - Conchs - Crabs = 0")))
# summary(conch_crabs_compare)
# #but note the estimate is too big
# #the code automatically doubles it, ie if you look at the matrix to find the levels
# model.matrix(oyster_lm_predator)
# #for just order
# model.matrix(oyster_lm_predator)[1,]
# #note order of levels (intercept is the one you are "missing") and define the
# #contrast (a messy but sometimes necessary option) it does
# contr <- rbind("Conchs_and_crabs - Conchs - Crabs" = c(-1, 2, -1, 0))
# summary(glht(oyster_lm_predator, linfct = mcp(Predator = contr)))
# #compare to 
# summary(conch_crabs_compare)
# 
# contr <- rbind("Conchs_and_crabs - Conchs - Crabs" = c(-.5, 1, -.5, 0))
# summary(glht(oyster_lm_predator, linfct = mcp(Predator = contr)))
# 
# #is intercept the issue
# oyster_lm_predator_no_intercept <- update(oyster_lm_predator, . ~ . -1)
# model.matrix(oyster_lm_predator_no_intercept)[1,]
# contr <- rbind("Conchs_and_crabs - Conchs - Crabs" = c(-1, 2, -1, 0))
# summary(glht(oyster_lm_predator_no_intercept, linfct = mcp(Predator = contr)))
# 
# conch_crabs_compare_no_intercept <- glht(oyster_lm_predator_no_intercept, linfct = mcp(Predator = c("Conchs_and_crabs - Conchs - Crabs = 0")))
# summary(conch_crabs_compare_no_intercept)

##INTRO TO GGPLOT2####
#ggplot2 is a great plotting package that allows a lot of control over your output
#lets do some examples using the sleep dataset
sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
#
#ggplot2 works in layers so you can or subtract as needed. Provided code is verbose here
#so you can see what its doing.

#first, install and call the package
require(ggplot2)

#to make a plot, first set a base layer
#lets start with a scatter plot and focus on relationship between time spent sleeping
#and time spent dreaming
#first, add your layers
dreaming_sleep_relationship <- ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming"))
#Now call the ggplot object you created
dreaming_sleep_relationship
#Nothing plots except the axes. Now you have to add layers. For example, you can add points
dreaming_sleep_relationship <- ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) + 
  geom_point()
dreaming_sleep_relationship
#Note here I'm saving the object, so to see it I call it. You can also just call directly
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point()
#then you get long calls that are easier (maybe) to manipulate
#Now you have a basic plot.  You can use other arguments in geom_layer commands 
#to add to it or themes or functions to change its look

#geom_ layers without any arguments assume you are using the base layers. You can 
#change this to add extra info to a plot. For example, let's color these by primate
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Primate"))

#now we've added information on primates. Note this is different from
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(colour="Primate")
#or
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
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
require(plyr)
sleep$Taxa <- revalue(sleep$Primate, c(Y = "Primate", N = "Non-primate"))
#notice what I did above. I made a new column from an existing one using a name 
#I might want on a legend
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"))
#I can also just change the legend title directly or change legend text, but often 
#workign with the dataframe is easier for me

#if we wanted these in a different order, we can use relevel to set one as the 
#"first" level (and then do this sequentially to get them in the right order if 
#needed".  You can also change level orders using the factor or ordered functions 
#for multiple levels at once
sleep$Taxa <- relevel(sleep$Taxa, "Primate" )
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"))

# Now lets change the axis labels,sizes, etc using theme function. Again, I left this
# verbose so you can change as needed. Also note the size argument in geom_point
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
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
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
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
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
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
ggplot(sleep, aes_string(x="TotalSleep", y = "Dreaming")) +
  geom_point(aes_string(colour="Taxa"), size = 4) +
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
ggplot(sleep, aes_string(x="Dreaming")) +
  geom_histogram()
#note we can just copy our theme info from above and modify as needed (or ggplot2
#will largely skip un-needed info).  You can also save and name a theme so you 
#don't have to do all this everytime.
ggplot(sleep, aes_string(x="Dreaming")) +
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

ggplot(sleep[sleep$Taxa == "Primate",], aes_string(x="Dreaming")) +
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
#a user-defined function in R, as ggplot2 doesn't have it built in (maybe because
#bar charts are a bad idea?).
#
#FUNCTIONS####
#First, a quick note on functions.  You may never need to write one, but they aren't 
#hard, and you may find one in code you search for online.  In general you can write 
#a function using the function command, which reads in the arguments, and then
#shows what to do with them in curly brackets. For example, 

timestwo <- function (x) {
  x+x
}

#running above creates a function that you can then use...

timestwo(12)
timestwo(c(1,2,3,4))

#More complicated issues involve what the function returns and how it handles the
#arguments. You may see loops(which can also be handy to understand). Examples are given below:
#
#FOR LOOPS####

for(x in 1:10){
  print(x+1)
}
#1:10 gives the sequence that the variable cycles through. This could also be a list
#or something else

#WHILE LOOPS####
x<-1
while(x<10){
  print(x)
  x<-x+x
}
#while loops evaluate a statement instead of going through a list.
#
#We're discussing this because below is a function I'll provide for makign barcharts.
#If you run it once, it will be available throughout an R session.  You can select the lines and run them.
#Another option is to copy (and keep) functions in a separate script that you "source" at the beginning of
#an R session. You can do this by manually opening file and selecting source button or by using the source()
#function.  Source means runs the entire script.
#
#summarySE function####
### Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  #  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#BAR PLOTS####
#Now let's use this to get the confidence intervals for each exposure level and plot them
sleep_by_exposure <- summarySE(sleep, measurevar = "TotalSleep", 
                               groupvars = "Exposure", na.rm = T)
#look at it
sleep_by_exposure
require(ggplot2)
ggplot(sleep_by_exposure
       , aes_string(x="Exposure", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
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
       , aes_string(x="Exposure", y="mean")) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Total sleep (hours per day")+ggtitle("Sleep across different taxa")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#factorial example plot####

sleep_by_exposure_primate <- summarySE(sleep, measurevar = "TotalSleep", 
                                       groupvars = c("Exposure", "Primate"), na.rm = T)
#look at it
sleep_by_exposure_primate
ggplot(sleep_by_exposure_primate
       , aes_string(x="Exposure", y="mean",color="Primate", 
                    shape = "Primate")) +
   geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Total sleep (hours per day")+
  ggtitle("Sleep across different exposure levels")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#other useful functions


