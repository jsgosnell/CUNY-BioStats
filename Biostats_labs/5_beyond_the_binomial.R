#graphs and tables from Beyond the Binomial lecture
#
##days of weeks births####
##
##
##chisq test####
days_of_week_births <- data.frame(Days = c("Sunday", "Monday", "Tuesday", 
                                           "Wednesday", "Thursday", "Friday", 
                                           "Saturday"), Births = 
                                    c(33,41,63,63,47,56,47))

#can input p for each category, or it assumes equal
chisq.test(days_of_week_births$Births)
chisq.test(days_of_week_births$Births, p= rep(1/7, 7))

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

mu <- sum(as.numeric(as.character(soccer_goals$Number_of_goals)) * soccer_goals$Occurences) /
  sum(soccer_goals$Occurences)

soccer_goals$Expected_prob <- (exp(-mu) *mu^as.numeric(as.character(soccer_goals$Number_of_goals))) /
                            factorial(as.numeric(as.character(soccer_goals$Number_of_goals)))
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

#everest 
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


results <- chisq.test(x = matrix(c(1045, 88, 32, 8), 2, 2, byrow = T))
results$expected
results

#fisher's exact test####
fisher.test(x = matrix(c(1045, 88, 32, 8), 2, 2, byrow = T))

#wren example
chisq.test(x = matrix(c(12, 0, 0, 4), 2, 2, byrow = T))
fisher.test(x = matrix(c(12, 0, 0, 4), 2, 2, byrow = T))

#gtest####
require(DescTools)
GTest(x = matrix(c(12, 0, 0, 4), 2, 2, byrow = T))