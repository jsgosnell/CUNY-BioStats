#graph
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

#use summarySE to check for variance if wanted

function_output <- summarySE(iris, measurevar = "Sepal.Length", groupvars = 
                               c("Petal.Length", "Species")) 

require(ggplot2)
ggplot(iris, aes_string(x="Petal.Length", y="Sepal.Length",color="Species", 
                                   shape = "Species")) +
  geom_point(size = 5) +
  ylab("Sepal Length")+ 
  xlab("Petal Length") + 
  ggtitle("Sepal Length increases with Petal Length")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#or with lines

ggplot(iris, aes_string(x="Petal.Length", y="Sepal.Length",color="Species", 
                                   shape = "Species")) +
  geom_point(size = 5) +
  ylab("Sepal Length (mm)")+ 
  xlab("Petal Length (mm)") + 
  ggtitle("Sepal Length increases with Petal Length")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_smooth(method="lm")


petal_sepal_length_ANCOVA <- lm(Sepal.Length ~ Petal.Length * Species, iris)
par(mfrow=c(2,2))
plot(petal_sepal_length_ANCOVA)
require(car)
Anova(petal_sepal_length_ANCOVA, type = "III") 
summary(petal_sepal_length_ANCOVA)
#not a significicant interaction, so run without
petal_sepal_length_ANCOVA <- lm(Sepal.Length ~ Petal.Length + Species, iris)
par(mfrow=c(2,2))
plot(petal_sepal_length_ANCOVA)
Anova(petal_sepal_length_ANCOVA, type = "III") 
#check for differences among species
require(multcomp)
summary(glht(petal_sepal_length_ANCOVA, linfct =mcp(Species = "Tukey")))
#all species different

#note order is important
petal_sepal_length_ANCOVA <- lm( Petal.Length~  Sepal.Length * Species, iris)
Anova(petal_sepal_length_ANCOVA, type = "III") 
#interactino is significant, so
Anova(lm( Petal.Length~  Sepal.Length, iris[iris$Species == "setosa",]),
      type = "III") 
Anova(lm( Petal.Length~  Sepal.Length, iris[iris$Species == "virginica",]),
      type = "III") 
summary(lm( Petal.Length~  Sepal.Length, iris[iris$Species == "virginica",]))
Anova(lm( Petal.Length~  Sepal.Length, iris[iris$Species == "versicolor",]),
      type = "III") 
summary(lm( Petal.Length~  Sepal.Length, iris[iris$Species == "versicolor",]))