#from lecture on interactions and blocking

#cholesterol example

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

cholest <- lm(cholest ~ day, cholesterol)
plot(cholest)
Anova(cholest, type = "III")
summary(cholest)
require(multcomp)
comp_cholest <- glht(cholest, linfct = mcp(day = "Tukey"))
summary(comp_cholest)

t.test(cholesterol[cholesterol$day == "2", "cholest"], 
       cholesterol[cholesterol$day == "14", "cholest"], paired = T)

#what about blocking
cholest_blocked <- lm(cholest ~ day + patient, cholesterol[cholesterol$day %in% 
                                                             c("2","14"),])
plot(cholest_blocked)
summary(cholest_blocked)
Anova(cholest_blocked, type = "III")

#extend to all days
cholest_blocked_all_days <- lm(cholest ~ day + patient, cholesterol)
plot(cholest_blocked_all_days)
summary(cholest_blocked_all_days)
Anova(cholest_blocked_all_days, type = "III")
comp_cholest_blocked_all_days <- glht(cholest_blocked_all_days, linfct = mcp(day = "Tukey"))
summary(comp_cholest_blocked_all_days)

#compare again to 
Anova(cholest, type = "III")
summary(comp_cholest)

#what if we care about other factor
#oyster exposure ####
oyster <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/oyster_exposure.csv?attredirects=0&d=1")

#graph
function_output <- summarySE(oyster, measurevar="Mass", groupvars =
                               c("Predator", "Exposure"), na.rm = T)

require(ggplot2)
ggplot(function_output, aes_string(x="Exposure", y="mean",color="Predator", 
                                                      shape = "Predator",linetype="Predator")) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Oyster mass (g)")+ 
  xlab("Days exposed per week") + 
  ggtitle("Impacts of predators on oyster growth")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#with line
ggplot(function_output, aes_string(x="Exposure", y="mean",color="Predator", 
                                   shape = "Predator",linetype="Predator")) +
  geom_point(size = 5) +
  geom_line(aes_string(group="Predator", linetype = "Predator"), size=2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Oyster mass (g)")+ 
  xlab("Days exposed per week") + 
  ggtitle("Impacts of predators on oyster growth")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


oyster_lm <- lm(Mass ~ Predator + Exposure, oyster)
summary(oyster_lm)
require(car)
Anova(oyster_lm, type = "III")
#why?
#having the control level included presents issues here
#you can skip
Anova(oyster_lm, type = "III", singular.ok = T)
#or drop 
##define not in function
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0
oyster_lm <- lm(Mass ~ Predator + Exposure, oyster[oyster$Predator %!in% c("None"),])
Anova(oyster_lm, type = "III")

mass_compare <- glht(oyster_lm, linfct = mcp(Predator = "Tukey"))
summary(mass_compare)

exposure_compare <- glht(oyster_lm, linfct = mcp(Exposure = "Tukey"))
summary(exposure_compare)

#this is ugly (from multcomp extra examples)
#https://cran.r-project.org/web/packages/multcomp/vignettes/multcomp-examples.pdf
K1 <- glht(oyster_lm, mcp(Predator = "Tukey"))$linfct
K2 <- glht(oyster_lm, mcp(Exposure = "Tukey"))$linfct
#simultaneously compare the levels of each factor using
exposure_compare <- glht(oyster_lm, linfct = rbind(K1, K2))
summary(exposure_compare)

#interactions####
oyster_lm <- lm(Mass ~ Predator * Exposure, oyster[oyster$Predator %!in% c("None"),])
Anova(oyster_lm, type = "III")

oyster$tw <- interaction(oyster$Predator, oyster$Exposure)
oyster_lm_2<-lm(Mass~-1+tw, oyster[oyster$Predator %!in% c("None"),])
summary(glht(oyster_lm_2,linfct=mcp(tw="Tukey")))



#for ease
reduced_data <- oyster[oyster$Predator %!in% c("None"),]
reduced_data$Predator <- factor(reduced_data$Predator)
reduced_data$Exposure <- factor(reduced_data$Exposure)
oyster_lm <- lm(Mass ~ Exposure * Predator, reduced_data)


tmp <- expand.grid(Predator = unique(reduced_data$Predator),
                   Exposure = unique(reduced_data$Exposure))
X <- model.matrix(~ Exposure * Predator, data = tmp)
glht(oyster_lm, linfct = X)

Tukey <- contrMat(table(reduced_data$Predator), "Tukey")
K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
rownames(K1) <- paste(levels(reduced_data$Exposure)[1], rownames(K1), sep = ":")
K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
rownames(K2) <- paste(levels(reduced_data$Exposure)[2], rownames(K2), sep = ":")
K3 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
rownames(K3) <- paste(levels(reduced_data$Exposure)[3], rownames(K3), sep = ":")
K4 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
rownames(K4) <- paste(levels(reduced_data$Exposure)[4], rownames(K3), sep = ":")
K <- rbind(K1, K2, K3,K4)
colnames(K) <- c(colnames(Tukey), colnames(Tukey), colnames(Tukey), colnames(Tukey))
summary(glht(oyster_lm, linfct = K %*% X))


#easier for all comparisons
require(lsmeans)
lsmeans(oyster_lm, pairwise ~ Exposure | Predator)


  


