#from first class
iris

#note hist is odd with cex
label_size <- 2
title_size <- 2.5

par(mar = c(7,7,7,7))

hist(iris$Sepal.Length, main = "Sepal Lengths", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")

#add density overlay
hist(iris$Sepal.Length, main = "Sepal Lengths", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue", prob = T)
lines(density(iris$Sepal.Length), col = "red")             # add a density estimate with defaults

#specifics
head(USArrests)
names(USArrests)
USArrests$State <- as.factor(rownames(USArrests))
par(las=2)
barplot(USArrests$Murder, names.arg = USArrests$State, cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue", main = "Murder Arrests per 100,000 since 1973")
write.csv(USArrests, "USArrests.csv", row.names = F)

write.csv(iris, "iris.csv", row.names = F)

#skewed right
birds <- rbeta(10000,2,12)
hist(birds, main="Weight of Westchester Blue Jays", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "blue", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#skewed left
birds <- rbeta(10000,70,5)
hist(birds, main="Weight of Westchester cardinals", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "red", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)
 
#bimodal data
putnam <- c(rnorm(100,20,4),rnorm(100,40,4))
hist(putnam, main="Weight of Westchester woodpeckers", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "orange", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#normal data
putnam <- c(rnorm(1000,20,4))
hist(putnam, main="Weight of Westchester parrots", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "green", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)


#uniform data
putnam <- c(runif(1000,.1,8))
hist(putnam, main="Weight of Westchester Robins", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "Pink", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#normal data
putnam <- c(rnorm(1000,20,4))
hist(putnam, main="Weight of Westchester parrots", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "green", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size, probability = T)
lines(density(putnam), col = "black")   # add a density estimate with defaults
curve(dnorm(x, mean=20, sd=4), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#bimodal data
putnam <- c(rnorm(100,20,4),rnorm(100,40,4))
hist(putnam, main="Weight of Westchester woodpeckers", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "orange", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size, probability = T)
lines(density(putnam), col = "black", lwd = 4)   # add a density estimate with defaults
abline(v=mean(putnam), col="red", lwd = 4)
abline(v=median(putnam), col="green", lwd = 4)
#no built in mode function....
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

abline(v=(getmode(putnam)), col="blue", lwd = 4)
legend(x=32.5, y= .04, legend = c("mean", "median", "mode"), fill=c("red","green", 
                                                             "blue"), cex = 1.5,
       bty="n", x.intersp = .1, y.intersp = .5)

hist(iris$Sepal.Length, breaks=3, main = "Sepal length histogram, 3 breaks",
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")
hist(iris$Sepal.Length, breaks=10, main = "Sepal length histogram, 10 breaks",
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")
hist(iris$Sepal.Length, main = "Sepal Lengths, auto breaks with R", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")

#getting real data
#need to use ggplot2 for ease (will get to this later and typically use it)
require(ggplot2)
ggplot(iris, aes_string("Species","Sepal.Length")) + 
  geom_point(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

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

#boxplot
#
##just versicolor

ggplot(iris[iris$Species == "versicolor",], aes_string("Species","Sepal.Length")) + 
  geom_boxplot(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of Iris versicolor")+
  xlab("") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=0), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#all species
ggplot(iris, aes_string("Species","Sepal.Length")) + 
  geom_boxplot(aes_string(colour="Species"), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#cdf

par(cex = 1)
versi <- ecdf(iris[iris$Species == "versicolor","Sepal.Length"])
plot(versi, verticals = T, col = "red", main = "CDF of Sepal Length (cm)", 
     xlab = "Sepal length (cm)", ylab = "Cumulative relative frequencey")
seto <- ecdf(iris[iris$Species == "setosa","Sepal.Length"])
plot(seto, add = T, col = "blue", verticals = T)
virg <- ecdf(iris[iris$Species == "virginica","Sepal.Length"])
plot(virg, add = T, col = "orange", verticals = T)


#stacked histogram
ggplot(iris, aes_string("Sepal.Length")) + 
  geom_histogram(aes_string(fill="Species"), size=3) +
  xlab("Sepal Length (cm)")+
  ylab("Frequency")+
  ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#facet
ggplot(iris, aes_string("Sepal.Length")) + 
  geom_histogram(aes_string(fill="Species"), size=3) +
  xlab("Sepal Length (cm)")+
  ylab("Frequency")+
  ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  facet_wrap(~Species, ncol = 1)

#example of diverging, stacked, and mosaic graphs

# long-form vegetation survey data from
# http://luisdva.github.io/rstats/Diverging-bar-plots/
# these data should more or less reflect the vegetation patterns at "Quebrada de Cordoba", Chile

vegSurvey <- 
  data.frame(sampling_point=rep(c(1:5),4),
             slope=c(rep("North",10),rep("South",10)),
             veg_Type=rep(c(rep("native",5),rep("introduced",5)),2),
             spp=as.integer(abs(rnorm(20,5,2))))
vegSurvey$spp <-   ifelse(vegSurvey$veg_Type =="introduced",vegSurvey$spp+1,vegSurvey$spp)

#grouped bar plot
ggplot(vegSurvey, aes_string(x="sampling_point", y="spp")) +
  geom_bar(aes_string(fill="veg_Type"), size = 3, stat = "identity") +
  ylab("Frequency")+
  xlab("Sampling point")+
  ggtitle("Invasive and native species based on site")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D")) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) 

#facet
ggplot(vegSurvey, aes_string(x="sampling_point", y="spp")) +
  geom_bar(aes_string(fill="veg_Type"), size = 3, stat = "identity") +
  ylab("Frequency")+
  xlab("Sampling point")+
  ggtitle("Invasive and native species based on site")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D")) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  facet_wrap(~slope, nrow = 1)


#stacked bar plot
ggplot(vegSurvey, aes_string(x="sampling_point", y="spp")) +
  geom_bar(aes_string(fill="veg_Type"), size = 3, stat = "identity", 
           position = position_dodge(width=0.5)) +
  ylab("Frequency") + 
  xlab("Sampling point") +
  ggtitle("Invasive and native species based on site")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D")) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

library(dplyr)
library(ggplot2)
library(extrafont)
devtools::install_github('bart6114/artyfarty')
library(artyfarty)

vegSurvey <- vegSurvey %>%  mutate(sppInv= ifelse(veg_Type =="native",spp,spp*-1))

# divergent plot

ggplot(vegSurvey, aes(x=sampling_point, y=sppInv, fill=veg_Type))+
  geom_bar(stat="identity",position="identity")+
  xlab("sampling point")+ylab("number of species")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D"))+
  coord_flip()+
  geom_hline(yintercept=0)+
  xlab("Sampling Points")+
  ylab("Species number")+
  scale_y_continuous(breaks = pretty(vegSurvey$sppInv),labels = abs(pretty(vegSurvey$sppInv)))+
  theme_scientific()+
  theme(strip.text.x = element_text(face = "bold"))

# plot for both slopes using facetting

ggplot(vegSurvey, aes(x=sampling_point, y=sppInv, fill=veg_Type))+
  geom_bar(stat="identity",position="identity")+
  facet_wrap(~slope)+xlab("sampling point")+ylab("number of species")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D"))+
  coord_flip()+
  geom_hline(yintercept=0)+
  xlab("Sampling Points")+
  ylab("Species number")+
  scale_y_continuous(breaks = pretty(vegSurvey$sppInv),labels = abs(pretty(vegSurvey$sppInv)))+
  theme_scientific()+
  theme(strip.text.x = element_text(face = "bold"))

#mosaic plot
#for mosaic plots, need to use other package (ggmosaic) or add frequency column
#for your choice variable

require(reshape2)

#get total native/invasive per site
vegSurvey_veg_per_site <- dcast(vegSurvey, sampling_point+veg_Type~ "total_veg_per_site", sum, 
                                value.var = "spp")
vegSurvey_per_site <- dcast(vegSurvey, sampling_point ~ "total_per_site", sum, 
                            value.var = "spp")
vegSurvey_veg_per_site <- merge(vegSurvey_veg_per_site, vegSurvey_per_site)
vegSurvey_veg_per_site$Proportion <- vegSurvey_veg_per_site$total_veg_per_site/
  vegSurvey_veg_per_site$total_per_site

ggplot(vegSurvey_veg_per_site, aes_string(x="sampling_point", y="Proportion")) +
  geom_bar(aes_string(fill="veg_Type"), size = 3, stat = "identity") +
  ylab("Frequency") + 
  xlab("Sampling point") +
  ggtitle("Invasive and native species based on site")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D")) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) 

#per slope
vegSurvey_per_slope <- dcast(vegSurvey, sampling_point + slope~ "total_per_slope", sum, 
                             value.var = "spp")
vegSurvey <- merge(vegSurvey, vegSurvey_per_slope)
vegSurvey$Proportion <- vegSurvey$spp/vegSurvey$total_per_slope

ggplot(vegSurvey, aes_string(x="sampling_point", y="Proportion")) +
  geom_bar(aes_string(fill="veg_Type"), size = 3, stat = "identity") +
  ylab("Frequency") + 
  xlab("Sampling point") +
  ggtitle("Invasive and native species based on slope")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D")) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  facet_wrap(~slope, ncol=1)

#numerical, numerical relationships
#
#scatter
ggplot(iris, aes_string(y ="Petal.Length",x ="Sepal.Length")) + 
  geom_point(aes_string(colour="Species"), size = 3) +
  xlab("Sepal Length (cm)") + 
  ylab("Petal Length (cm)") +
  ggtitle("Relationship between sepal and petal lengths in irises")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#line
#use airquality dataset for time series
#
airquality$Date <- as.Date(paste(airquality$Month, airquality$Day, sep="/"), 
                           format ="%m/%d" )

#just points
ggplot(airquality, aes_string(x ="Date",y ="Temp")) + 
  geom_point(size = 3, col = "orange") +
  xlab("Date") + 
  ylab("Temperature (C)") +
  ggtitle("Temperature over time")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#points and line
ggplot(airquality, aes_string(x ="Date",y ="Temp")) + 
  geom_point(size = 3, col = "orange") +
  geom_line() +
  xlab("Date") + 
  ylab("Temperature (C)") +
  ggtitle("Temperature over time")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#more than one axis
#for 1:1 transformation data (anything you can use a function to make on a similar
#scale)
#can add easily but not get scale
ggplot(airquality, aes_string(x ="Date",y ="Temp")) + 
  geom_point(size = 3, col = "orange") +
  geom_line() +
  geom_point(aes_string(y="Wind")) +
  xlab("Date") + 
  ylab("Temperature (C)") +
  ggtitle("Temperature over time")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#manually scale data and get secondary axis
#have to aes to make tranformation (not aes_string)
#put colour in quotes in aes to force legend
ggplot(airquality, aes_string(x ="Date",y ="Temp")) + 
  geom_point(aes(col ="Temperature"), size = 3) +
  geom_line(col="orange") +
  geom_point(aes(y=Wind+50, col = "Wind speed")) +
  scale_y_continuous(sec.axis = sec_axis(~.-50, name = "Wind (mph)")) +
  xlab("Date") + 
  ylab("Temperature (C)") +
  ggtitle("Environmental measurements over time")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#outliers and bad fits

good_fit_x <- runif(100, 1, 50)
good_fit_y <- rnorm(100,25,2)
good_data <- data.frame(source = "good", x=good_fit_x, y=good_fit_y)
bad_fit_x <- runif(10, 20, 30)
bad_fit_y <- rnorm(10,95,1)
bad_data <- data.frame(source = "outlier", x=bad_fit_x, y=bad_fit_y)
all_data <- rbind (good_data, bad_data)

#just points
ggplot(all_data, aes_string(x ="x",y ="y")) + 
  geom_point(aes_string(color="source"), size = 3) +
  xlab("x") + 
  ylab("y") +
  ggtitle("Outliers can impact data")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#curve and scatter
ggplot(all_data, aes_string(x ="x",y ="y")) + 
  geom_point(aes_string(color="source"), size = 3) +
  geom_smooth(se = F) +
  xlab("x") + 
  ylab("y") +
  ggtitle("Outliers can impact data")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#just curve
ggplot(all_data, aes_string(x ="x",y ="y")) + 
  geom_smooth(se = F) +
  xlab("x") + 
  ylab("y") +
  ggtitle("Outliers can impact data")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))









