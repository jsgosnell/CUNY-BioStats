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
          plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  
    
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
  
#build model intercept####
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

#R2####
#R2 explains how much variation is explained by our model (groups in this instance)
summary(iris_anova)
#or
summary(iris_anova)$r.squared
#note you can have a low p-value model that also explains only small amount of 
#variation in the data

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





