#from lecture on regression and correlation

#cholesterol example####

cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$day <- as.factor(cholesterol$day)
cholesterol$patient <- as.factor(cholesterol$patient)
head(cholesterol)
summary(cholesterol)


library(Rmisc)
function_output <- summarySE(cholesterol, measurevar="cholest", groupvars =
                               c("day"), na.rm = T)
library(ggplot2)
ggplot(function_output, aes(x=day, y=cholest)) +
  geom_col(size = 3) +
  geom_errorbar(aes(ymin=cholest-ci, ymax=cholest+ci), size=1.5) +
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

ggplot(cholesterol, aes(x=day, y=cholest)) +
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
library(car)
Anova(cholest_regression, type = "III")

#extension to linear model####
#heres the top row
model.matrix(cholest_regression)[1,]
coef(cholest_regression)

#plot with lm####
ggplot(cholesterol, aes(x=day, y=cholest)) +
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
ggplot(cholesterol, aes(x=day, y=cholest)) +
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

#spearman rank####
monkey <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/monkey.csv")
cor.test(~ eggs_per_gram + Dominance_rank, monkey, method = "spearman")
cor.test(monkey$eggs_per_gram, monkey$Dominance_rank, method = "spearman")


#permutation####
library(coin)
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

ggplot(cholesterol_merged, aes(x=day, y=cholest)) +
  geom_point(aes(color = source), size = 3) +
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
