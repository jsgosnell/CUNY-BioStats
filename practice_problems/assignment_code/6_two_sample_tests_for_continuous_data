#load bootstrap function

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

#1
drug_b <- c( 8.8, 8.4, 7.9, 8.7, 9.1, 9.6)
drug_g <- c(9.9, 9.0, 11,1, 9.6, 8.7, 10.4, 9.5)

t.test(drug_b, drug_g)
wilcox.test(drug_b, drug_g)
bootstrapjsg(drug_b, drug_g)
require(coin) #requires data_frame
clotting <- data.frame(drug = c(rep("drug_b", length(drug_b)), rep("drug_g", 
                                                                   length(drug_g))),
                       clotting = c(drug_b, drug_g))
independence_test(clotting ~ drug, clotting)

#2
fertilizer <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/fertilizer.csv")

t.test(height ~ fertilizer, fertilizer)
wilcox.test(height ~ fertilizer, fertilizer)
bootstrapjsg(fertilizer[fertilizer$fertilizer =="old", "height"],
              fertilizer[fertilizer$fertilizer =="new", "height"])
independence_test(height ~ fertilizer, fertilizer)

#have to add extra identifier (but this results in a lot of NA)
fertilizer$ID <- 1:nrow(fertilizer)
fertilizer_wide <- dcast(fertilizer, ID~fertilizer, value.var = "height")
t.test(fertilizer_wide$new, fertilizer_wide$old)


#3
metabolic <- read.csv("https://raw.github.com/jsgosnell/CUNY-BioStats/blob/master/datasets/metabolic_rates.csv")
t.test(metabolic$before, metabolic$after, paired = T)
wilcox.test(metabolic$before, metabolic$after, paired = T)
require(BSDA)
SIGN.test(metabolic$before, metabolic$after)
bootstrapjsg(metabolic$before - metabolic$after)

#4
endurance <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/endurance.csv")
head(endurance)
t.test(endurance ~ time, endurance, paired = T)
#or
t.test(endurance[endurance$time == "before", "endurance"],
       endurance[endurance$time == "after", "endurance"],
       paired = T)
wilcox.test(endurance ~ time, endurance, paired = T)
SIGN.test(endurance[endurance$time == "before", "endurance"],
          endurance[endurance$time == "after", "endurance"])
bootstrapjsg(endurance[endurance$time == "before", "endurance"]-
             endurance[endurance$time == "after", "endurance"])

metabolic_long <- melt(metabolic, id.vars = "size", variable.name = "time",
                      value.name = "rate")


#5
sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)
require(ggplot2)
ggplot(sport, aes_string("Hg"))+
  geom_histogram() +
  facet_wrap(~Sex) +
  ggtitle("	Plasma ferritin concentration of Australian athletes") +
  xlab("Ferritin concentration")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        strip.text.x = element_text(size = 22))

t.test(Hg ~ Sex, sport)

#6
##from http://rcompanion.org/rcompanion/d_10.html
Input = ("
 Clone          August  November
 Balsam_Spire    8.1    11.2
 Beaupre        10.0    16.3
 Hazendans      16.5    15.3
 Hoogvorst      13.6    15.6
 Raspalje        9.5    10.5
 Unal            8.3    15.5
 Columbia_River  18.3   12.7
 Fritzi_Pauley   13.3   11.1
 Trichobel        7.9   19.9
 Gaver            8.1   20.4
 Gibecq           8.9   14.2
 Primo           12.6   12.7
 Wolterson       13.4   36.8
")

poplar <- read.table(textConnection(Input),header=TRUE)

poplar$difference <- poplar$November - poplar$August

ggplot(poplar, aes_string("difference"))+
  geom_histogram() +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        strip.text.x = element_text(size = 22))

#lots of holes, but fairly symmetric

wilcox.test(poplar$August, poplar$November, paired=TRUE)
#or
wilcox.test(poplar$difference)

#or

bootstrapjsg(poplar$difference)

#7

cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$day <- as.factor(cholesterol$day)
head(cholesterol)

summary(cholesterol)

ggplot(cholesterol[cholesterol$day %in% c("2", "14"),], aes_string("cholest")) +
  geom_histogram() +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  facet_wrap(~day)

t.test(cholesterol[cholesterol$day == "2", "cholest"], 
       cholesterol[cholesterol$day == "14", "cholest"],
       paired = T)

#compare to 

t.test(cholesterol[cholesterol$day == "2", "cholest"], 
       cholesterol[cholesterol$day == "14", "cholest"])
