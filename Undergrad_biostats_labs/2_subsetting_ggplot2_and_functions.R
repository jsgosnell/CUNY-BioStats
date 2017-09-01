

#intro to ggplot2
#back to airquality!
head(airquality)
require(ggplot2)
#first, set a base layer
plot_layers <- ggplot(airquality, aes_string(x="Temp", y = "Ozone"))
plot_layers
#nothing
#you have to add layers
plot_layers_2 <- plot_layers + geom_line()
plot_layers_2
plot_layers_2 <- plot_layers + geom_point()
plot_layers_2
plot_layers_2 <- plot_layers + geom_point(colour = "Month")
plot_layers_2
#no, you have to set it in aes
plot_layers_2 <- plot_layers + geom_point(aes_string(colour = "Month"))
plot_layers_2
#now you can add other things
plot_layers_3 <- plot_layers_2 + xlab("Temperature")
plot_layers_3
#facet
plot_layers_2 <- plot_layers + geom_point()
plot_layers_2
plot_layers_2 + facet_wrap(~Month)
ggsave("Fig1.jpg")

#for histograms, you only need one axis (frequency is calculated)
plot_layers <- ggplot(airquality, aes_string(x="temp"))
plot_layers + geom_histogram()
plot_layers + geom_histogram() + facet_wrap(~month)

#barchart
#
#load function
#
### Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
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

#now use
function_output <- summarySE(iris, measurevar="Sepal.Length", groupvars =
                               c("Species"))

ggplot(function_output, aes_string(x="Species", y="mean")) +
  geom_col(aes_string(fill="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))