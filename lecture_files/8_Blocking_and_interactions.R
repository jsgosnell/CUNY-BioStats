#from lecture on interactions and blocking

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

#compare to t-test####
t.test(cholesterol[cholesterol$day == "2", "cholest"], 
       cholesterol[cholesterol$day == "14", "cholest"], paired = T)
#note no differnce between days 2 and 14 in lm vs significant diffrence using 
#t-test - what's different?

#what about blocking (in addition to extra information on variance)
#to show this is the same as pairing
cholest_blocked <- lm(cholest ~ day + patient, cholesterol[cholesterol$day %in% 
                                                             c("2","14"),])
plot(cholest_blocked)
summary(cholest_blocked)
Anova(cholest_blocked, type = "III") #now has same p-value

#idea can extend to all days
cholest_blocked_all_days <- lm(cholest ~ day + patient, cholesterol)
plot(cholest_blocked_all_days)
summary(cholest_blocked_all_days)
Anova(cholest_blocked_all_days, type = "III")
comp_cholest_blocked_all_days <- glht(cholest_blocked_all_days, linfct = mcp(day = "Tukey"))
summary(comp_cholest_blocked_all_days) #now we see difference

#compare again to 
Anova(cholest, type = "III")
summary(comp_cholest)

#what if we care about other factor
#2-way ANOVA ####
#oyster exposure ####
oyster <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/oyster_exposure.csv?attredirects=0&d=1")

#graph multiple factors by includign extra factor in summarySE call 
function_output <- summarySE(oyster, measurevar="Mass", groupvars =
                               c("Predator", "Exposure"), na.rm = T)

#and then varying another aesthetic (here color and shape) to show that difference
require(ggplot2)
ggplot(function_output, aes_string(x="Exposure", y="mean",color="Predator", 
                                                      shape = "Predator")) +
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
                                   shape = "Predator")) +
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

#first, think about what the graph suggests
#
#now lets build model considering how predator identity impacts mass
oyster_lm_predator <- lm(Mass ~ Predator, oyster)
par(mfrow=c(2,2))
plot(oyster_lm_predator)
Anova(oyster_lm_predator, type = "III") #significant
predator_compare <- glht(oyster_lm_predator, linfct = mcp(Predator = "Tukey"))
summary(predator_compare)

#what about just exposure
oyster_lm_exposure <- lm(Mass ~ Exposure, oyster)
par(mfrow=c(2,2))
plot(oyster_lm_exposure)
Anova(oyster_lm_exposure, type = "III") #significant, but with some deviance...
exposure_compare <- glht(oyster_lm_exposure, linfct = mcp(Exposure = "Tukey"))
summary(exposure_compare)
#any issues here?

#we can consider both simultaneously by adding plus sign
#lm algorithm (least squares) partitions variance among the groups
oyster_lm_combined <- lm(Mass ~ Predator + Exposure, oyster)
plot(oyster_lm_combined)
require(car)
Anova(oyster_lm_combined, type = "III")
#why?
#having the control level included presents issues here
#you can skip
Anova(oyster_lm_combined, type = "III", singular.ok = T)
#or drop 
##define not in function
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0
oyster_lm_combined <- lm(Mass ~ Predator + Exposure, oyster[oyster$Predator %!in% c("None"),])
Anova(oyster_lm_combined, type = "III")

#compare groups using multcomp####
#this is a little ugly (from multcomp extra examples)
#https://cran.r-project.org/web/packages/multcomp/vignettes/multcomp-examples.pdf
K1 <- glht(oyster_lm_combined, mcp(Predator = "Tukey"))$linfct
K2 <- glht(oyster_lm_combined, mcp(Exposure = "Tukey"))$linfct
#simultaneously compare the levels of each factor using
combined_compare <- glht(oyster_lm_combined, linfct = rbind(K1, K2))
summary(combined_compare)

#interactions####
#add using colon (for just interaction) or asterisk to get MAIN EFFECTS and 
#INTERACTION
oyster_lm_interactions <- lm(Mass ~ Predator * Exposure, oyster[oyster$Predator %!in% c("None"),])
Anova(oyster_lm_interactions, type = "III")

#compare using glht (D
oyster$tw <- interaction(oyster$Predator, oyster$Exposure)
oyster_lm_2<-lm(Mass~-1+tw, oyster[oyster$Predator %!in% c("None"),])
summary(glht(oyster_lm_2,linfct=mcp(tw="Tukey")))
#but this does ALL pairs - what about within each group?

#using glht - DON'T! ####
#doing this in multcomp to demonstrate difficulties 
#SKIP
#lsmeasn will be used for upcoming interactions comparisons, but for an additive 
#experiment you have to hand-make the matrix 
#example here
reduced_data <- oyster[oyster$Predator %!in% c("None"),]
#dropped here as any empty (NULL) levels present problems
reduced_data$Predator <- factor(reduced_data$Predator)
reduced_data$Exposure <- factor(reduced_data$Exposure)
oyster_lm_lsmeans_example <- lm(Mass ~ Exposure * Predator, reduced_data)
#have to make grids by hand
tmp <- expand.grid(Predator = unique(reduced_data$Predator),
                   Exposure = unique(reduced_data$Exposure))
X <- model.matrix(~ Exposure * Predator, data = tmp)
glht(oyster_lm_lsmeans_example, linfct = X)

#now have to create comparison matrix by hand (or write function to do so)
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
summary(glht(oyster_lm_lsmeans_example, linfct = K %*% X))
#end multcomp note for comparisons by group####

#using lsmeans for all comparisons####
#easier for all comparisons
require(lsmeans)
lsmeans(oyster_lm, pairwise ~ Exposure | Predator)

#main value of multcomp:glht is doing targeted commands, eg looking at model without "none"
oyster_lm_predator_new
#is conch and crabs more than the combined effect of other two
conch_crabs_compare <- glht(oyster_lm_predator, linfct = mcp(Predator = c("Conchs_and_crabs - Conchs - Crabs = 0")))
summary(conch_crabs_compare)
#but note the estimate is too big
#the code automatically doubles it, ie if you look at the matrix to find the levels
model.matrix(oyster_lm_predator)
#for just order
model.matrix(oyster_lm_predator)[1,]
#note order of levels (intercept is the one you are "missing") and define the
#contrast (a messy but sometimes necessary option) it does
contr <- rbind("Conchs_and_crabs - Conchs - Crabs" = c(-1, 2, -1, 0))
summary(glht(oyster_lm_predator, linfct = mcp(Predator = contr)))
#compare to 
summary(conch_crabs_compare)

contr <- rbind("Conchs_and_crabs - Conchs - Crabs" = c(-.5, 1, -.5, 0))
summary(glht(oyster_lm_predator, linfct = mcp(Predator = contr)))

#is intercept the issue
oyster_lm_predator_no_intercept <- update(oyster_lm_predator, . ~ . -1)
model.matrix(oyster_lm_predator_no_intercept)[1,]
contr <- rbind("Conchs_and_crabs - Conchs - Crabs" = c(-1, 2, -1, 0))
summary(glht(oyster_lm_predator_no_intercept, linfct = mcp(Predator = contr)))