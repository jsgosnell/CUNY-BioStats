#plots from 1. Intro.ppt####
iris

#base histograms####
#note hist is odd with cex
label_size <- 2
title_size <- 2.5

par(mar = c(7,7,7,7))
#if you get an error about "figure margins too large", reset this to 
par(mar = c(rep(5,4)))
#this is just issue with trying to size figures for lecture slides
#may also have issues with some images...

hist(iris$Sepal.Length, main = "Sepal Lengths", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")

#skewed left####
birds <- rbeta(10000,70,5)
hist(birds, main="Weight of Westchester cardinals", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "red", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#normal data####
putnam <- c(rnorm(1000,20,4))
hist(putnam, main="Weight of Westchester parrots", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "green", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#uniform data####
putnam <- c(runif(1000,.1,8))
hist(putnam, main="Weight of Westchester Robins", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "Pink", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#skewed right####
birds <- rbeta(10000,2,12)
hist(birds, main="Weight of Westchester Blue Jays", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "blue", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#bimodal data####
putnam <- c(rnorm(100,20,4),rnorm(100,40,4))
hist(putnam, main="Weight of Westchester woodpeckers", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "orange", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#add density overlay to histograms####
hist(iris$Sepal.Length, main = "Sepal Lengths", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue", prob = T)
lines(density(iris$Sepal.Length), col = "red")             # add a density estimate with defaults


#normal data with density overly and true normal#####
putnam <- c(rnorm(1000,20,4))
hist(putnam, main="Weight of Westchester parrots", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "green", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size, probability = T)
lines(density(putnam), col = "black")   # add a density estimate with defaults
curve(dnorm(x, mean=20, sd=4), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#bar charts for categorical data####
head(USArrests)
names(USArrests)
USArrests$State <- as.factor(rownames(USArrests))
par(las=2)
barplot(USArrests$Murder, names.arg = USArrests$State, cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue", main = "Murder Arrests per 100,000 since 1973")
write.csv(USArrests, "USArrests.csv", row.names = F)

write.csv(iris, "iris.csv", row.names = F)

#bar chart issues####
#auto breaks####
hist(iris$Sepal.Length, main = "Sepal Lengths, auto breaks with R", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")
#counts of auto breaks####
x =hist(iris$Sepal.Length, main = "Sepal Lengths, auto breaks with R", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size)
plot(x$breaks[-9], x$counts, main = "Sepal Lengths, auto breaks with R", 
     xlab = "\n Sepal Length (cm)", ylab = "Frequency", 
     cex.lab=label_size, cex.axis=1.5, 
     cex.main=title_size, cex.sub=label_size, col="blue", pch = 16, cex = 3)
#setting own breaks####
hist(iris$Sepal.Length, breaks=3, main = "Sepal length histogram, 3 breaks",
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")
hist(iris$Sepal.Length, breaks=10, main = "Sepal length histogram, 10 breaks",
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")

#boxplot versicolor####
#need to use ggplot2 for ease (will get to this later and typically use it)
library(ggplot2)
ggplot(iris[iris$Species == "versicolor",], aes(Species,Sepal.Length)) + 
  geom_boxplot(aes(colour=Species), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of Iris versicolor")+
  xlab("") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=0), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#data from multiple groups####
#barchart ####
#
#load function from Rmisc
library(Rmisc)

#now use
function_output <- summarySE(iris, measurevar="Sepal.Length", groupvars =
                               c("Species"))

ggplot(function_output, aes(x=Species, y=Sepal.Length)) +
  geom_col(aes(fill=Species), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#scatterplot all species####
ggplot(iris, aes(Species,Sepal.Length)) + 
  geom_point(aes(colour=Species), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#stacked histogram####
ggplot(iris, aes(Sepal.Length)) + 
  geom_histogram(aes(fill=Species), size=3) +
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

#facetted stacked histogram####
ggplot(iris, aes(Sepal.Length)) + 
  geom_histogram(aes(fill=Species), size=3) +
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

#boxplot all species####
ggplot(iris, aes(Species,Sepal.Length)) + 
  geom_boxplot(aes(colour=Species), size = 3) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#cdf all species####
par(cex = 2)
versi <- ecdf(iris[iris$Species == "versicolor","Sepal.Length"])
plot(versi, verticals = T, col = "red", 
     main = "Cumulative frequency distribution  of Sepal Length (cm)", 
     xlab = "Sepal length (cm)", ylab = "Cumulative relative frequencey")
seto <- ecdf(iris[iris$Species == "setosa","Sepal.Length"])
plot(seto, add = T, col = "blue", verticals = T)
virg <- ecdf(iris[iris$Species == "virginica","Sepal.Length"])
plot(virg, add = T, col = "orange", verticals = T)

#example of diverging, stacked, and mosaic graphs####
library(dplyr)

# long-form vegetation survey data from
# http://luisdva.github.io/rstats/Diverging-bar-plots/
# these data should more or less reflect the vegetation patterns at "Quebrada de Cordoba", Chile

vegSurvey <- 
  data.frame(sampling_point=rep(c(1:5),4),
             slope=c(rep("North",10),rep("South",10)),
             veg_Type=rep(c(rep("native",5),rep("introduced",5)),2),
             spp=as.integer(abs(rnorm(20,5,2))))
vegSurvey$spp <-   ifelse(vegSurvey$veg_Type =="introduced",vegSurvey$spp+1,vegSurvey$spp)



vegSurvey <- vegSurvey %>%  mutate(sppInv= ifelse(veg_Type =="native",spp,spp*-1))

#grouped bar plot####
ggplot(vegSurvey, aes(x=sampling_point, y=spp)) +
  geom_bar(aes(fill=veg_Type), size = 3, stat = "identity", 
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

#stacked bar plot####
ggplot(vegSurvey, aes(x=sampling_point, y=spp)) +
  geom_bar(aes(fill=veg_Type), size = 3, stat = "identity") +
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


#facetted stacked####
ggplot(vegSurvey, aes(x=sampling_point, y=spp)) +
  geom_bar(aes(fill=veg_Type), size = 3, stat = "identity") +
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


# divergent plot####

ggplot(vegSurvey, aes(x=sampling_point, y=sppInv, fill=veg_Type))+
  geom_bar(stat="identity",position="identity")+
  xlab("sampling point")+ylab("number of species")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D"))+
  coord_flip()+
  geom_hline(yintercept=0)+
  xlab("Sampling Points")+
  ylab("Species number")+
  scale_y_continuous(breaks = pretty(vegSurvey$sppInv),labels = abs(pretty(vegSurvey$sppInv)))+
  theme(strip.text.x = element_text(face = "bold"))

# divergent plot with facetting by slope####

ggplot(vegSurvey, aes(x=sampling_point, y=sppInv, fill=veg_Type))+
  geom_bar(stat="identity",position="identity")+
  facet_wrap(~slope)+xlab("sampling point")+ylab("number of species")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D"))+
  coord_flip()+
  geom_hline(yintercept=0)+
  xlab("Sampling Points")+
  ylab("Species number")+
  scale_y_continuous(breaks = pretty(vegSurvey$sppInv),labels = abs(pretty(vegSurvey$sppInv)))+
  theme(strip.text.x = element_text(face = "bold"))

#mosaic plot####
#for mosaic plots, need to use other package (ggmosaic) or add frequency column
#for your choice variable

library(reshape2)

#get total native/invasive per site
vegSurvey_veg_per_site <- dcast(vegSurvey, sampling_point+veg_Type~ "total_veg_per_site", sum, 
                                value.var = "spp")
vegSurvey_per_site <- dcast(vegSurvey, sampling_point ~ "total_per_site", sum, 
                            value.var = "spp")
vegSurvey_veg_per_site <- merge(vegSurvey_veg_per_site, vegSurvey_per_site)
vegSurvey_veg_per_site$Proportion <- vegSurvey_veg_per_site$total_veg_per_site/
  vegSurvey_veg_per_site$total_per_site

ggplot(vegSurvey_veg_per_site, aes(x=sampling_point, y=Proportion)) +
  geom_bar(aes(fill=veg_Type), size = 3, stat = "identity") +
  ylab("Percent") + 
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

#turn this into pie chart####
vegSurvey_veg_per_site$sampling_point <- factor(vegSurvey_per_site$sampling_point)
#have to make weird empty factor else you get concentric circles
vegSurvey_veg_per_site$Share <- ""
ggplot(vegSurvey_veg_per_site, aes(x=Share, y=Proportion)) +
  geom_bar(aes(fill=veg_Type), size = 3, stat = "identity") +
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
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  coord_polar(theta="y") +
  facet_wrap(~sampling_point)

#or (not in slides) cast to get single plot/split####
vegSurvey_veg_per_site_cast <- dcast(vegSurvey_veg_per_site, veg_Type ~ "Proportion", value.var = "Proportion", mean)

vegSurvey_veg_per_site_cast$Site <- ""

ggplot(vegSurvey_veg_per_site_cast, aes(x=Site, y=Proportion)) +
  geom_bar(aes(fill=veg_Type), size = 3, stat = "identity") +
  ylab("Frequency") + 
  xlab("") +
  ggtitle("Overall composition of invasive and native species across sites")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D")) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  coord_polar(theta="y") 


#mosaic plot facetted by slope####
vegSurvey_per_slope <- dcast(vegSurvey, sampling_point + slope~ "total_per_slope", sum, 
                             value.var = "spp")
vegSurvey <- merge(vegSurvey, vegSurvey_per_slope)
vegSurvey$Proportion <- vegSurvey$spp/vegSurvey$total_per_slope

ggplot(vegSurvey, aes(x=sampling_point, y=Proportion)) +
  geom_bar(aes(fill=veg_Type), size = 3, stat = "identity") +
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

#numerical, numerical relationships ####
#
#scatter####
ggplot(iris, aes(y =Petal.Length,x =Sepal.Length)) + 
  geom_point(aes(colour=Species), size = 3) +
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

#data over time####
#use airquality dataset for time series
#
airquality$Date <- as.Date(paste(airquality$Month, airquality$Day, sep="/"), 
                           format ="%m/%d" )

#just points####
ggplot(airquality, aes(x =Date,y =Temp)) + 
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

#points and line####
ggplot(airquality, aes(x =Date,y =Temp)) + 
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

#more than one axis####
#for 1:1 transformation data (anything you can use a function to make on a similar
#scale)
#can add easily but not get scale
ggplot(airquality, aes(x =Date,y =Temp)) + 
  geom_point(size = 3, col = "orange") +
  geom_line() +
  geom_point(aes(y=Wind)) +
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
ggplot(airquality, aes(x =Date,y =Temp)) + 
  geom_point(aes(col ="Temp"), size = 3) +
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

#outliers and bad fits####

good_fit_x <- runif(100, 1, 50)
good_fit_y <- rnorm(100,25,2)
good_data <- data.frame(source = "good", x=good_fit_x, y=good_fit_y)
bad_fit_x <- runif(10, 20, 30)
bad_fit_y <- rnorm(10,95,1)
bad_data <- data.frame(source = "outlier", x=bad_fit_x, y=bad_fit_y)
all_data <- rbind (good_data, bad_data)

#just points####
ggplot(all_data, aes(x =x,y =y)) + 
  geom_point(aes(color=source), size = 3) +
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

#curve and scatter####
ggplot(all_data, aes(x =x,y =y)) + 
  geom_point(aes(color=source), size = 3) +
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

#just curve####
ggplot(all_data, aes(x =x,y =y)) + 
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

#skewed left with measures of central tendency####
##weird issues with margins and legends
par(mar = c(rep(4,4)))
birds <- rbeta(10000,70,5)
hist(birds, main="Weight of Westchester cardinals", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "red", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)
abline(v=mean(birds), col="yellow", lwd = 4)
abline(v=median(birds), col="green", lwd = 4)
#no built in mode function....
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

abline(v=(getmode(birds)), col="blue", lwd = 4)
legend("topleft", legend = c("mean", "median", "mode"), fill=c("yellow",
                                                                    "green","blue"), 
       cex = 1.5, bty = "n", x.intersp = .1, y.intersp = .5)

#bimodal data with measures of central tendency#####
par(mar = c(rep(2,4)))
putnam <- c(rnorm(100,20,4),rnorm(100,40,4))
hist(putnam, main="Weight of Westchester woodpeckers", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "orange", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size, probability = T)
lines(density(putnam), col = "black", lwd = 4)   # add a density estimate with defaults
abline(v=mean(putnam), col="red", lwd = 4)
abline(v=median(putnam), col="green", lwd = 4)
abline(v=(getmode(putnam)), col="blue", lwd = 4)
legend("bottomright", legend = c("mean", "median", "mode"), fill=c("red","green", 
                                                                    
                                                                    "blue"), cex = 1.5,
       bty="n", x.intersp = .1, y.intersp = .5)

#illustrate variance####
#add sample #
iris$sample <- 1:nrow(iris)

#just scatter plot

ggplot(iris[iris$Species == "setosa",], aes(sample,Sepal.Length)) + 
  geom_point(size = 3) +
  ylab("Sepal Length (cm)")+ggtitle(expression(paste("Sepal Length in ", italic("Iris setosa"))))+
    theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#add mean
ggplot(iris[iris$Species == "setosa",], aes(sample,Sepal.Length)) + 
  geom_point(size = 3) +
  ylab("Sepal Length (cm)")+ggtitle(expression(paste("Sepal Length in ", italic("Iris setosa"))))+
  geom_hline(yintercept = mean(iris[iris$Species == "setosa", "Sepal.Length"]), 
             color = "blue", size = 2) +
  annotate("text", label = "mean", x = 20, y = 4.9 , size = 8, color = "blue") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#add segment for each point
segment_data = data.frame(
  x = 1:50,
  xend = 1:50, 
  y = iris[iris$Species == "setosa", "Sepal.Length"],
  yend = mean(iris[iris$Species == "setosa", "Sepal.Length"])
)

ggplot(iris[iris$Species == "setosa",], aes(sample,Sepal.Length)) + 
  geom_point(size = 3) +
  ylab("Sepal Length (cm)")+ggtitle(expression(paste("Sepal Length in ", italic("Iris setosa"))))+
  geom_hline(yintercept = mean(iris[iris$Species == "setosa", "Sepal.Length"]), 
             color = "blue", size = 2) +
  annotate("text", label = "mean", x = 20, y = 4.9 , size = 8, color = "blue") +
  annotate("text", label = "square each red line  \n and find average", x = 25, 
           y = 5.5 , size = 8, color = "red") +
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend),
               color= "red", size = 1.1) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

  

#transformations####

sample_data <- data.frame(x = rnorm (1000, 1, 1))
sample_data$x_add <- sample_data$x+5

ggplot(sample_data) + 
  geom_histogram(aes(x =x, fill="x"), se = F) +
  geom_histogram(aes(x = x_add, fill="x+5"), se = F)+
  labs(fill="Data")+
  xlab("x") + 
  ylab("y") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=30),
        axis.text.x  = element_text(size=30), 
        legend.text =element_text(size=30),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

sample_data$x_multiply <- sample_data$x*5

ggplot(sample_data) + 
  geom_histogram(aes(x =x, fill="x"), se = F) +
  geom_histogram(aes(x = x_multiply, fill="x*5"), se = F)+
  labs(fill="Data")+
  xlab("x") + 
  ylab("y") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=30),
        axis.text.x  = element_text(size=30), 
        legend.text =element_text(size=30),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#shift them left
summary(birds)
length(birds[birds < .9])
birds[birds < .9] <- birds[birds < .9] - .2
hist(birds, main="Weight of Westchester cardinals", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "red", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)
abline(v=mean(birds), col="yellow", lwd = 4)
abline(v=median(birds), col="green", lwd = 4)
abline(v=(getmode(birds)), col="blue", lwd = 4)
legend(x=.75, y= 1000, legend = c("mean", "median", "mode"), fill=c("yellow","green", 
                                                                    
                                                                    "blue"), cex = 1.5,
       bty="n", x.intersp = .1, y.intersp = .5)

#categorical data####
head(iris)
iris$random <- runif(1:nrow(iris))
iris$LL <- 0
iris$LL[iris$random > .7] <- 1

ggplot(iris[iris$Species == "setosa", ], aes(LL)) + 
  geom_histogram(size=3) +
  xlab("Genotype score")+
  ylab("Frequency")+
  ggtitle("Genotype score in an iris species")+
  geom_vline(xintercept = mean(iris[iris$Species == "setosa", "LL"]), color = "blue") +
  annotate("text", label = "proportion", x = .25, y = 20 , size = 8, color = "blue") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

