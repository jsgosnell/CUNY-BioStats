#quiz
#
require(ggplot2)
sleep <- read.csv("http://www.jstephengosnell.com/teaching-resources/datasets/sleep.csv?attredirects=0&d=1")
sleep$Predation <- as.factor(sleep$Predation)

#removing outliers in data call
ggplot(sleep[sleep$Predation == 2 & is.na(sleep$Predation) == F 
             & sleep$LifeSpan < 20 & is.na(sleep$LifeSpan)==F,], 
       aes_string(x="LifeSpan", y = "Gestation")) +
  geom_point(size = 4) +
  #below here is ylabel, xlabel, and main title
  xlab("Life span (years)") +
  ylab("Gestation (days)") +
  ggtitle("Gestation time increases with life span") +
  #scale commands help with legends
  scale_colour_manual(name="Type of mammal",values = c("#FFA373","#50486D")) +
  #theme sets sizes, text, etc
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32),
        # change plot background, grid lines, etc (just examples so you can see)
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(size=3),
        panel.grid.major = element_line(colour = "black"),
        plot.background = element_rect(fill="gray"),
        legend.background = element_rect(fill="gray"))


