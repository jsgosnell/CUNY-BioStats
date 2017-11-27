#from lecture on regression and correlation

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

#idea can extend to all days
cholest_blocked_all_days <- lm(cholest ~ day + patient, na.omit(cholesterol))
plot(cholest_blocked_all_days)
summary(cholest_blocked_all_days)
Anova(cholest_blocked_all_days, type = "III")

comp_cholest_blocked_all_days <- glht(cholest_blocked_all_days, linfct = mcp(day = "Tukey"))
summary(comp_cholest_blocked_all_days) #now we see difference

#isn't time continous?####
#returns days to numeric
cholesterol$day <- as.numeric(as.character(cholesterol$day))

ggplot(cholesterol, aes_string(x="day", y="cholest")) +
  geom_point(size = 3) +
  ylab("Cholesterol level")+ggtitle("Cholesterol level following a heart attack")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#as a linear model
cholest_regression <- lm(cholest ~ day, na.omit(cholesterol))
par(mfrow = c(2,2))
plot(cholest_regression)
require(car)
Anova(cholest_regression, type = "III")

#extension to linear model####
#heres the top row
model.matrix(cholest_regression)[1,]
coef(cholest_regression)

#plot with lm####
ggplot(cholesterol, aes_string(x="day", y="cholest")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  ylab("Cholesterol level")+ggtitle("Cholesterol level following a heart attack")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#improve ####
ggplot(cholesterol, aes_string(x="day", y="cholest")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  ylab("Cholesterol level") + 
  xlab("Days since heart attack") + 
  ggtitle("Cholesterol level decreases following a heart attack")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#compare again to
Anova(cholest, type = "III")
summary(comp_cholest)

#correlation####
summary(cholest_regression)
cor.test(~ cholest + day, data = na.omit(cholesterol))
#note
(-.2257696)^2 #is equal to 0.05097, multiple r2 from lm output

#spearman rank
monkey <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/monkey.csv?attredirects=0&d=1")
cor.test(~ eggs_per_gram + Dominance_rank, monkey, method = "spearman")

require(coin)
independence_test(cholest ~ day, cholesterol)

#leverage and outliers
cholesterol$source <- "original"
#make outlier
cholesterol_seven <- data.frame(day = "7", cholest = "240", source = "new")
cholesterol_merged <- merge(cholesterol, cholesterol_seven, all = T)
#look at model to make "good" point
coef(cholest_regression)
cholesterol_twenty <- data.frame(day = "20", 
                                 cholest = coef(cholest_regression)[1] +
                                   20 * coef(cholest_regression)[2],
                                 source = "new")
cholesterol_merged <- merge(cholesterol_merged, cholesterol_twenty, all = T)
cholesterol_twenty_outlier <- data.frame(day = "20", 
                                 cholest = coef(cholest_regression)[1] +
                                   20 * coef(cholest_regression)[2] * 5,
                                 source = "new")
cholesterol_merged <- merge(cholesterol_merged, cholesterol_twenty_outlier, all = T)
cholesterol_merged$day <- as.numeric(as.character(cholesterol_merged$day))
cholesterol_merged$cholest <- as.numeric(as.character(cholesterol_merged$cholest))
#plot

ggplot(cholesterol_merged, aes_string(x="day", y="cholest")) +
  geom_point(aes_string(color = "source"), size = 3) +
  geom_smooth(method = "lm") +
  ylab("Cholesterol level") + 
  xlab("Days since heart attack") + 
  ggtitle("Cholesterol level decreases following a heart attack")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

cholest_regression_merged <- lm(cholest ~ day, cholesterol_merged)
plot(cholest_regression_merged)
cholesterol_merged[58,]

cholest_regression_merged <- lm(cholest ~ day, cholesterol_merged[-58,])
plot(cholest_regression_merged)
