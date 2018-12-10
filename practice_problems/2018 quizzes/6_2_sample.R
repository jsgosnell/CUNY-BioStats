cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$day <- as.factor(cholesterol$day)
head(cholesterol)

summary(cholesterol)

require(ggplot2)
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

#could also plot differences!

change <- data.frame (change = na.omit(cholesterol[cholesterol$day == "14", "cholest"] - cholesterol[cholesterol$day == "2", "cholest"]))

ggplot(change, aes_string("change")) +
  geom_histogram() +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


t.test(cholesterol[cholesterol$day == "2", "cholest"], 
       cholesterol[cholesterol$day == "14", "cholest"],
       paired = T)
#or
t.test(change$change)

#compare to 

t.test(cholesterol[cholesterol$day == "2", "cholest"], 
       cholesterol[cholesterol$day == "14", "cholest"])