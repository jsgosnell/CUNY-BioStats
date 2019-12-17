#create data
#idea from handbook of biological stats  
#http://www.biostathandbook.com/pairedttest.html
#and
#Wiebe, K.L., and G.R. Bortolotti. 2002. Variation in carotenoid-based color in northern flickers in a hybrid zone. Wilson Bulletin 114: 393-400.
# feather_color <- data.frame(normal = rnorm(n = 10, mean = 10, sd = 10))
# feather_color$new <- feather_color$normal + 4 + runif(10)
# write.csv(feather_color, "feather_color.csv", row.names = F)
#
#
feather_color <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/feather_color.csv")

#make long data to make graph
library(reshape2)
feather_color_long <- melt(feather_color, variable.name = "Feather_type",
                           value.name = "Brightness")

library(Rmisc)
feather_summary <- summarySE(feather_color_long, measurevar = "Brightness",
                             groupvars = "Feather_type")
library(ggplot2)
ggplot(feather_summary, aes(x=Feather_type, y = Brightness)) +
  geom_col() +
  xlab("Feather type") + ggtitle("Feather type impacts brightness") +
  geom_errorbar(aes(ymin = Brightness - ci, ymax = Brightness + ci))+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#could also plot differences!
feather_color$difference <- feather_color$normal - feather_color$new
feather_color_difference_summary <- summarySE(feather_color, measurevar = "difference")

#note you can make x a character by putting in it quotes
ggplot(feather_color_difference_summary, aes(x = "Paired data", y = difference)) +
  geom_col() +
  geom_errorbar(aes(ymin = difference - ci, ymax = difference + ci))+
  ggtitle("Feather type impacts brightness") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#paired t-test
t.test(feather_color$normal,feather_color$new, paired = T)
#or
t.test(feather_color$difference)
#or
t.test(Brightness ~ Feather_type, feather_color_long, paired = T)

#compare to 
t.test(feather_color$normal,feather_color$new)
#or
t.test(Brightness ~ Feather_type, feather_color_long)