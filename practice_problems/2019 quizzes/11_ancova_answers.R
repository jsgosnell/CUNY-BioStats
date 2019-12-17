pollution <- read.table("http://www.statsci.org/data/general/wolfrive.txt", header = T)
str(pollution)

pollutant_model <- lm(Aldrin ~ HCB * Depth, pollution)
par(mfrow = c(2,2))
plot(pollutant_model)
library(car)
Anova(pollutant_model, type = "III")
#no impact of interaction, so remove
pollutant_model <- lm(Aldrin ~ HCB + Depth, pollution)
plot(pollutant_model)
Anova(pollutant_model, type = "III")
#significant, could remove Depth if you wanted...
pollutant_model <- lm(Aldrin ~ HCB, pollution)
plot(pollutant_model)
Anova(pollutant_model, type = "III")
summary(pollutant_model)

library(ggplot2)
ggplot(pollution, aes(x=HCB, y=Aldrin, color = Depth, shape = Depth))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  ggtitle("HCB and Aldrin levels are positively related but not depth-dependent")+ 
  theme(axis.title.x = element_text(face="bold", size=28),  
        axis.title.y = element_text(face="bold", size=28),  
        axis.text.y  = element_text(size=20), 
        axis.text.x  = element_text(size=20),  
        legend.text =element_text(size=20), 
        legend.title = element_text(size=20, face="bold"), 
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
  




