####exam#### 

#1
require(binom)
binom.confint(175,200)

#2
binom.test(10,10, .9)

#3
binom.test(120,126, .9)
120/126

binom.test(120,126, .5)


#4
binom.test(120,126, .9, alternative = "greater")

require(ggplot2)
zombie <- data.frame (Survived = c("Original Vaccine", "Second Vaccine - first trial", "Second vaccine - second trial"), Proportion = c(90, 100, 120/126 * 100))
ggplot(zombie, aes_string(x="Survived", y = "Proportion")) + geom_col()
#have to use coord_cartesian becasue ggplot won't even do this natively 
#http://biostat.mc.vanderbilt.edu/wiki/pub/Main/TatsukiRcode/Poster3.pdf
ggplot(zombie, aes_string(x="Survived", y = "Proportion")) + 
  geom_col(aes_string(fill = "Survived")) + 
  coord_cartesian(ylim=c(89,100)) + xlab("Vaccine") + 
  ylab("Percentage not bitten") +
  ggtitle("Outcomes of zombie vaccines") + 
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) + 
  guides(fill = F)


#skewed right

# data made once below
# vaccinated <- data.frame(Temperature = rbeta(15,2,12)+110 + runif(15,0,20 ), Group = "Vaccinated")
# summary(infected)
# unvaccinated <- data.frame(Group = "Not_vaccinated", Temperature = rnorm(15, 98.6, 10))
# zombie_temp <- merge(vaccinated, unvaccinated, all.x = T, all.y = T)

# write.csv(zombie_temp, "impacts_of_vaccine_on_temp.csv", row.names = F)

#above is old (random) data, use this as check
 zombie_temp <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/impacts_of_vaccine_on_temp.csv?attredirects=0&d=1")
head(zombie_temp)
mean(zombie_temp[zombie_temp$Group == "Vaccinated","Temperature"])

mean(zombie_temp[,"Temperature"])


ggplot(zombie_temp[zombie_temp$Group == "Vaccinated",], aes_string(x="Temperature")) + geom_histogram(fill="orange") +
  ylab("Number counted") +
  xlab(expression(paste("Temperature ", degree ~ F, "")))+
  ggtitle("Oral temperature of vaccinated survivors") + 
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

require(BSDA)
z.test(zombie_temp[zombie_temp$Group == "Vaccinated", "Temperature"], mu = 98.6, sigma.x = .7)

t.test(zombie_temp[zombie_temp$Group == "Vaccinated", "Temperature"], mu = 98.6)

#bar chart####

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


require(ggplot2)

require(plyr)

zombie_temp$GroupNew <- revalue(zombie_temp$Group, c("Not_vaccinated" = "Not vaccinated"))


zombie_temp_summary <- summarySE(zombie_temp, 
                                 measurevar = "Temperature", groupvars = "GroupNew", na.rm = T)

ggplot(zombie_temp_summary
       , aes_string(x="GroupNew", y="mean")) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1.5) +
  ylab("Temperature (F)")+ggtitle("Temperature based on past vaccine use")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

