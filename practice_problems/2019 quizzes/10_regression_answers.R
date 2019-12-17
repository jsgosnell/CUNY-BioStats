crickets <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/crickets.csv") 
str(crickets) 

crickets_relationship <- lm(Chirps ~ Temperature_F, crickets) 
par(mfrow=c(2,2)) 
plot(crickets_relationship) 
library(car) 
Anova(crickets_relationship, type = "III") 
summary(crickets_relationship) 

library(ggplot2) 
ggplot(crickets, aes(x=Temperature_F, y=Chirps)) + 
  geom_point(size = 5) + 
  ylab("Chirps")+  
  xlab(expression(paste("Temperature", ( degree~F)))) +  
  ggtitle("Crickets chirp more at higher temperatures")+ 
  theme(axis.title.x = element_text(face="bold", size=28),  
        axis.title.y = element_text(face="bold", size=28),  
        axis.text.y  = element_text(size=20), 
        axis.text.x  = element_text(size=20),  
        legend.text =element_text(size=20), 
        legend.title = element_text(size=20, face="bold"), 
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) 

#add line 
ggplot(crickets, aes(x=Temperature_F, y=Chirps)) + 
  geom_point(size = 5) + 
  ylab("Chirps")+  
  xlab(expression(paste("Temperature", ( degree~F)))) +  
  ggtitle("Crickets chirp more at higher temperatures")+ 
  theme(axis.title.x = element_text(face="bold", size=28),  
        axis.title.y = element_text(face="bold", size=28),  
        axis.text.y  = element_text(size=20), 
        axis.text.x  = element_text(size=20),  
        legend.text =element_text(size=20), 
        legend.title = element_text(size=20, face="bold"), 
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+ 
  stat_smooth(method = "lm", col = "red", se = F) 
