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
x = c(0:50)
pdf = data.frame(Number_righthanded, using_distribution)

require(ggplot2)

#just histogram
ggplot(pdf, aes_string(x="x", y = "using_distribution")) +
  geom_col(fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#with curve
ggplot(pdf, aes_string(x="x", y = "using_distribution")) +
  geom_col(fill = "orange") +
  stat_function(fun = dchisq, args = list(df = 6),size = 3, color = "green") + 
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#filled in

dnorm_one_sd <- function(x){
  norm_one_sd <- dchisq(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x >= qchisq(.95, df = 6)] <- NA
  return(norm_one_sd)
}


ggplot(pdf, aes_string(x="x", y = "using_distribution")) +
  stat_function(fun = dchisq, args = list(df = 6),size = 3, fill = "green", geom = "area") + 
  stat_function(fun = dnorm_one_sd, args = list(df = 6), size = 3, fill = "blue",
                geom = "area") + 
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))






