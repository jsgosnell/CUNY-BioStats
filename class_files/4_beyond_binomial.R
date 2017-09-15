#graphs and tables from Beyond the Binomial lecture
#
days_of_week_births <- data.frame(Days = c("Sunday", "Monday", "Tuesday", 
                                           "Wednesday", "Thursday", "Friday", 
                                           "Saturday"), Births = 
                                    c(33,41,63,63,47,56,47))

require(ggplot2)
ggplot(days_of_week_births, aes_string(x= "Days", y = "Births")) +
  geom_col(fill = "orange") +
  ggtitle("Births each day of the week for sample in 1999") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#using the chisq distribution####
using_distribution = dchisq(0:50,6, 1/7)
using_distribution
sum(using_distribution)
Number_righthanded = c(0:50)
pdf = data.frame(Number_righthanded, using_distribution)
plot(0:50, using_distribution)
#for all plots, R takes a best guess at the best type of plot; you can also
#direct it with the type command.  ?plot for more info
barplot(using_distribution,
        xlab = "Chi-squared distribution", ylab = "Probability density" )





