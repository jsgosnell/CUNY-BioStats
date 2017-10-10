#graphs and tables from normality lecture
#
#manipulations of binomial distribution used central_limit_theorem.R script from
#code_examples folder.  Note rbinom is odd. p is chance of success, n is hte number
#of observations (how many times you draw N balls from the urn), and size is N 
#(the number of balls you draw from the urn).  
#
require(ggplot2)

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -1 | x >= 1] <- NA
  return(norm_one_sd)
}

dnorm_two_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -2 | x >= 2] <- NA
  return(norm_one_sd)
}
#standard normal with 1 and 2 se highlighted ####
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 3, fill = "black",
                geom = "area")+
  stat_function(fun = dnorm_two_sd, geom = "area", fill = "purple") +
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#compare t and normal distribution####
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

#show where .56 lies on t distribution #####
dt_one_sd <- function(x, df){
  norm_one_sd <- dt(x, df)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -2.06 | x >= 2.06] <- NA
  return(norm_one_sd)
}
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dt, n = 101, args = list( df= 24), size = 3, fill = "purple",
                geom = "area") +
  stat_function(fun = dt_one_sd, args = list( df= 24), geom = "area", fill = "orange") +
    ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#add vline to show sample value####
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dt, n = 101, args = list( df= 24), size = 3, fill = "purple",
                geom = "area") +
  stat_function(fun = dt_one_sd, args = list( df= 24), geom = "area", fill = "orange") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  geom_vline(xintercept = -.56, color = "blue")

#normal
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), ask=T, cex = 1.2, cex.main = 2)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rnorm(n=n[j]))
  }
  hist(means, main=paste("Normal, n = ",n[j], sep = ""), col = "orange", xlim = c(-5,5))
}

#binomial
n=c(1,5,10,20,40,80)
means=1:9000
par(mfrow=c(2,3), oma = c(3,2,2,2), ask=F, cex = 1.2, cex.main = 2)
for (j in 1:length(n)){
  for (i in 1:length(means)){
    means[i]=mean(rbinom(n=n[j], 1, .7))
  }
  hist(means, main=paste("Binomial, n = ",n[j], sep = ""), col = "orange")
}
mtext("Draws from a binomial distribution, p = .7", outer = T, side = 1, 
      cex = 3, line = 1)