petal_sepal_length_relationship <- lm(Sepal.Length ~ Petal.Length, iris)
par(mfrow=c(2,2))
plot(petal_sepal_length_relationship)
require(car)
Anova(petal_sepal_length_relationship, type = "III")
summary(petal_sepal_length_relationship)

require(ggplot2)
ggplot(iris, aes_string(x="Petal.Length", y="Sepal.Length")) +
  geom_point(size = 5) +
  ylab("Sepal Length (mm)")+ 
  xlab("Petal Length (mm)") + 
  ggtitle("Sepal Length increases with Petal Length")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#add line
ggplot(iris, aes_string(x="Petal.Length", y="Sepal.Length")) +
  geom_point(size = 5) +
  ylab("Sepal Length (mm)")+ 
  xlab("Petal Length (mm)") + 
  ggtitle("Sepal Length increases with Petal Length")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  stat_smooth(method = "lm", col = "red")
