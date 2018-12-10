require(ggplot2)

#intial raw plot of data to check for outliers

#facet####
ggplot(iris, aes_string("Petal.Width")) + 
  geom_histogram(aes_string(fill="Species"), size=3) +
  xlab("Petal Width (cm)")+
  ylab("Frequency")+
  ggtitle("Petal Width of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  facet_wrap(~Species, ncol = 1)

#looks ok and big

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

summarySE(iris, measurevar = "Petal.Length", groupvars = c("Species")) #close enough

#fit model
width_lm <- lm(Petal.Width ~ Species, iris) 
#plot to check assumptions
par(mfrow = c(2,2))
plot(width_lm) #looks good
summary(width_lm)
require(car)
Anova(width_lm, type = "III")

#multiple comparisons
require(multcomp)
width_comparison <- glht(width_lm, linfct = mcp(Species = "Tukey"))
summary(width_comparison)

#plot
function_output <- summarySE(iris, measurevar = "Petal.Width", 
                             groupvars = c("Species")) #close enough

function_output$comparison <- "NA"
#enter by hand for small groups by comparing function_output means with multcomp 
#output (usually tukey)
function_output
summary(compare_cont_tukey)
#all different here, so
function_output$comparison <- letters[1:3]
ggplot(function_output, aes_string(x="Species", y="mean", color = "Species")) +
  geom_point(aes_string(fill="Species"), size = 3) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Petal Width (cm)")+ggtitle("Petal Width of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20, face = "italic"), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  #offset from mean by hand
  geom_text(aes(x = Species, y = mean + .35, label = comparison), size = 28, 
color = "black") +
  guides(fill = F, color = F)
