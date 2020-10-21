#1 vampires
# vampires <- data.frame ("blood_type" = c(rep("real", 50), rep ("fake", 50)),
# "score"  = c(runif(50, 5.5, 6.5),runif(50, 5.4,6.4)))
write.csv(vampires, "vampires.csv")

vampires <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSY7jlwNr6tN0mq_reYrqMbkVzhdiToGqboWmEWBu0ES3BAoRHmRX_fLrZ6SEyAuQ8oTngAd7vMPy1a/pub?gid=687055309&single=true&output=csv")

#2
library(ggplot2)
require(Rmisc)
vampire_summary <- summarySE(vampires, measurevar = "score", groupvars = "blood_type")
ggplot(vampire_summary, aes_string(x = "blood_type", y = "score")) + 
  geom_col(aes_string(fill = "blood_type")) +
  geom_errorbar(aes(ymin=score-ci, ymax=score+ci), size=1.5) +
  xlab("Blood Type")+
  ylab("Health Score")+
  ggtitle("Vampires are not healthier drinking real blood")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  guides(colour = F, fill = F)
#3
t.test(vampires[vampires$blood_type == "real", "score"], mu = 6.02)
#4
vampires$blood_type_graph <- revalue(vampires$blood_type, c("fake" = "fake blood",
                                                                          "real" = "real blood"))
ggplot(vampires, aes( x = score))+
  geom_histogram(aes(fill = blood_type_graph)) + 
  facet_wrap(~blood_type_graph, ncol = 1) +
  theme(axis.title.x = element_text(face="bold", size=28), 
                                         axis.title.y = element_text(face="bold", size=28), 
                                         axis.text.y  = element_text(size=20),
                                         axis.text.x  = element_text(size=20), 
                                         legend.text =element_text(size=20),
                                         legend.title = element_text(size=20, face="bold"),
                                         legend.position = "bottom",
                                         plot.title = element_text(hjust = 0.5, face="bold", size=32),
                                         strip.text.x = element_text(size = 22))+
  guides(colour = F, fill = F) +
  ylab("Frequency")+
  xlab("Health Score")

#4
binom.test(18,20, .5)
#5
binom.test(18,20, .78)
