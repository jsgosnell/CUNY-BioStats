oyster <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/oyster_exposure.csv?attredirects=0&d=1")
levels(oyster$Predator)
#then reorder (one way here, can also use revalue)
oyster$Predator <- factor(oyster$Predator, levels = c("None", "Crabs", "Conchs", "Conchs_and_crabs"))
#also fix spacing issue
oyster$Predator <- revalue (oyster$Predator, 
                            c("Conchs_and_crabs" = "Conchs and crabs"))


#graph multiple factors by includign extra factor in summarySE call 
function_output <- summarySE(oyster, measurevar="Shell_thickness_index", groupvars =
                               c("Predator", "Exposure"), na.rm = T)

#with line
ggplot(function_output, aes_string(x="Exposure", y="mean",color="Predator", 
                                   shape = "Predator")) +
  geom_point(size = 5) +
  geom_line(aes_string(group="Predator", linetype = "Predator"), size=2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  xlab("Days exposed per week") + 
  ggtitle("Impacts of predators on oyster growth")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

oyster_lm_interactions <- lm(Shell_thickness_index ~ Predator * Exposure, 
                             oyster[oyster$Predator %!in% c("None"),])
par(mfrow=c(2,2))
plot(oyster_lm_interactions)
require(car)
Anova(oyster_lm_interactions, type = "III")
#no significant interactino, so drop
oyster_lm_interactions <- lm(Shell_thickness_index ~ Predator + Exposure, 
                             oyster[oyster$Predator %!in% c("None"),])
Anova(oyster_lm_interactions, type = "III")
#only predator matters
require(multcomp)
predator_compare <- glht(oyster_lm_interactions, linfct = 
                           mcp(Predator = "Tukey"))
summary(predator_compare)




