#1 binomial####
binom.test(x = 11, n = 85, p = .07)
binom.test(x = 11, n = 85, p = .07, alternative = "greater")

#2 
13/85
binom.test(x = 13, n = 85, p = .07, alternative = "greater")


#3
tracks <- data.frame(origin = c(rep("bigfoot", 28), rep("bear", 100)), track_length = c(12 + 2* rchisq(28, df=1), rnorm(100, 13, 2)))
write.csv(tracks, "tracks.csv", row.names = F)
summary(tracks)

tracks <- NULL
tracks <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/tracks.csv")

library(ggplot2)
ggplot(tracks[tracks$origin == "bigfoot",], aes_string("track_length")) + 
  geom_histogram(size=3, fill = "blue") +
  xlab("Length (in)")+
  ylab("Frequency")+
  ggtitle("Length of Bigfoot Tracks from Wayward Pines, Oregon")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


ggplot(tracks, aes_string("track_length")) + 
  geom_histogram(aes_string(fill = "origin")) +
  xlab("Length (in)")+
  ylab("Frequency")+
  ggtitle("Length of Bigfoot Tracks from Wayward Pines, Oregon")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


track_summary <- summarySE(tracks, measurevar = "track_length", groupvars = "origin")
ggplot(track_summary, aes_string(x = "origin", y = "mean")) + 
  geom_col(aes_string(fill = "origin")) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  xlab("Supposed Source")+
  ylab("Length (in)")+
  ggtitle("Length of Bigfoot and Bear Tracks from Wayward Pines, Oregon")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#4####
t.test(tracks[tracks$origin == "bigfoot", "track_length"], mu = 13)

require(simpleboot)
bootstrapjsg=function(data1, data2=NULL, conf=.95, fun=mean, r=10000, null=0)
{
  if (is.null(data2)){
    a=one.boot(na.omit(data1), fun, r)
    confint=boot.ci(a, conf)
    p=length(subset(a$t, abs(a$t-mean(a$t))>=abs(null-mean(a$t))))/r
    output=c(conf, "% Confidence Interval", confint$percent[1,4:5], "p-value", p)
    return(output)}
  if (is.null(data2)==F){
    a=two.boot(na.omit(data1), na.omit(data2), fun, r)
    confint=boot.ci(a, conf)
    p=length(subset(a$t, abs(a$t-mean(a$t))>=abs(null-mean(a$t))))/r
    output=c(conf, "% Percentile Confidence Interval", confint$percent[1,4:5], "p-value", p)
    return(output)}
}

bootstrapjsg(tracks[tracks$origin == "bigfoot", "track_length"], null = 13)

require(BSDA)
SIGN.test(tracks[tracks$origin == "bigfoot", "track_length"], md = 13)

#5####
mean(tracks[tracks$origin == "bigfoot", "track_length"])

ggplot(track_summary, aes_string(x = "origin", y = "mean")) + 
  geom_col(aes_string(fill = "origin")) +
  xlab("Supposed Source")+
  ylab("Length (in)")+
  ggtitle("Length of Bigfoot and Bear Tracks from Wayward Pines, Oregon")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) + 
  coord_cartesian(ylim=c(12,14)) +
  guides(fill = F)

#6
require(binom)
binom.confint(x=17, n = 18)

