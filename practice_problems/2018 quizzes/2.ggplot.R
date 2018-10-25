#quiz
#
require(ggplot2)
sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
sleep$Predation <- as.factor(sleep$Predation)

ggplot(sleep, 
       aes_string(x="LifeSpan", y = "BrainWt")) +
  geom_point(size = 4) +
  #below here is ylabel, xlabel, and main title
  xlab("Life span (years)") +
  ylab("Brain Weight (kg)") +
  ggtitle("Brain weight increases with lifespan but is highly variable") +
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

#if you did axis other way...
require(ggplot2)
sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")
sleep$Predation <- as.factor(sleep$Predation)

ggplot(sleep, 
       aes_string(x="BrainWt", y = "LifeSpan")) +
  geom_point(size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Life span (years)") +
  xlab("Brain Weight (kg)") +
  ggtitle("Brain weight increases with lifespan but is highly variable") +
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

#if you removed outliers
ggplot(sleep[sleep$BrainWt < 1500,], 
       aes_string(x="BrainWt", y = "LifeSpan")) +
  geom_point(size = 4) +
  #below here is ylabel, xlabel, and main title
  ylab("Life span (years)") +
  xlab("Brain Weight (kg)") +
  ggtitle("Brain weight increases with lifespan but is highly variable") +
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






