function_output <- summarySE(additive, measurevar="mpg", groupvars =
                               c("Type", "Additive"), na.rm = T)
ggplot(function_output, aes_string(x="Additive", y="mean",color="Type", 
                                   shape = "Type")) +
  geom_line(aes_string(group="Type", linetype = "Type"), size=2) +
  geom_point(size = 5) +
  #geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Miles per gallon")+ 
  xlab("Gas Type") + 
  ggtitle("Impact of additive depends on car type")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
