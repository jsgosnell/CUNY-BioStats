#graphs and tables from normality lecture
#
#manipulations of binomial distribution used central_limit_theorem.R script from
#code_examples folder.  Note rbinom is odd. p is chance of success, n is hte number
#of observations (how many times you draw N balls from the urn), and size is N 
#(the number of balls you draw from the urn).  
#
require(ggplot2)

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, color = "orange",
                geom = "line") + 
  stat_function(fun = dt, n = 101, args = list( df= 3), size = 3, color = "purple",
                geom = "line") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))



dnorm_one_sd <- function(x, df){
  norm_one_sd <- dt(x, df)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -.56 | x >= .56] <- NA
  return(norm_one_sd)
}
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dt, n = 101, args = list( df= 24), size = 3, fill = "purple",
                geom = "area") +
  stat_function(fun = dnorm_one_sd, args = list( df= 24), geom = "area", fill = "orange") +
    ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dt, n = 101, args = list( df= 24), size = 3, fill = "purple",
                geom = "area") +
  stat_function(fun = dnorm_one_sd, args = list( df= 24), geom = "area", fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  geom_vline(xintercept = 2.06, color = "blue")
