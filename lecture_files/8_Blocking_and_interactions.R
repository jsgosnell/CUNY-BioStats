#from lecture on interactions and blocking

#### mcfarland example ####
#doesn't match paper!


length <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/McFarland_PLOSOne_2020_IndLength.csv",
                   stringsAsFactors = )
#make it wide
library(reshape2)
length_long <- melt(length, id.vars = "Age..days.", value.name = "length", 
                    variable.name = "cohort")
str(length_long)
length_long$Age..days. <- factor(length_long$Age..days.)

fed_fit <- lm(length~Age..days.*cohort, 
              length_long[length_long$cohort == 
                            levels(length_long$cohort)[grep("Fed", 
                                                     levels(length_long$cohort))],])
summary(fed_fit)
library(car)
Anova(fed_fit, type="III")


#what if we care about other factor
#2-way ANOVA ####
memory <- read.table("http://www.statsci.org/data/general/eysenck.txt", header = T,
                     stringsAsFactors = T)
library(plyr)
memory$Age <- relevel(memory$Age, "Younger")

#graph data with line but no error bar
#add extra call to groupvars in summarySE
function_output <- summarySE(memory, measurevar="Words", groupvars =
                               c("Age", "Process"), na.rm = T)

ggplot(function_output, aes(x=Age, y=Words,color=Process, 
                                   shape = Process)) +
  geom_line(aes(group=Process, linetype = Process), size=2) +
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
library(lsmeans)
lsmeans(memory_interactions, pairwise ~ Process | Age)

#interpret coef (subtract intercept for ease)
coef(update(memory_interactions, .~.-1))

#example of no interaction####
#modified version of biomass project
biomass <- read.csv("http://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/biomass_experiment.csv", stringsAsFactors = T)
function_output <- summarySE(biomass, measurevar="Mass", groupvars =
                               c("PredSize", "PredNumber"), na.rm = T)
#make NA's control
function_output$PredSize <- as.character(function_output$PredSize)
function_output$PredNumber <- as.character(function_output$PredNumber)
function_output[5,1:2]  <- "Control"
#change back to factor to change order
function_output$PredSize <- factor(function_output$PredSize, 
                                   levels = c("Control", "Small", "Large"))
ggplot(function_output, aes(x=PredSize, y=Mass,color=PredNumber, 
                                   shape = PredNumber)) +
  geom_point(size = 5) +
  geom_line(aes(group=PredNumber, linetype =PredNumber), size=2) +
  geom_errorbar(aes(ymin=Mass-ci, ymax=Mass+ci), size=1.5) +
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
library(WRS2) 
#have to get rid of any empty levels!
t2way(Mass ~ PredSize + PredNumber, biomass[is.na(biomass$PredNumber) != T, ])
#example of multcomp option but not needed here
mcp2atm(Mass ~ PredSize + PredNumber, biomass[is.na(biomass$PredNumber) != T, ])


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
