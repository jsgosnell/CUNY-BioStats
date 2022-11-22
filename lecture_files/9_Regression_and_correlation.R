#from lecture on regression and correlation

# iris example

library(ggplot2)
ggplot(iris, aes(x=Petal.Length, y=Sepal.Length)) +
  geom_point(size = 3) +
  ylab("Sepal Length")+ggtitle("Sepal length increases with petal length")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  xlab("Petal length (cm)") +
  ylab("Sepal length (cm)")

iris_regression <- lm(Sepal.Length ~ Petal.Length, iris)
par(mfrow = c(2,2))
plot(iris_regression)
library(car)
Anova(iris_regression, type = "III")
summary(iris_regression)


#extension to linear model####
#heres the top row
model.matrix(iris_regression)[1,]
coef(iris_regression)

#correlation####
cor.test(~ Sepal.Length + Petal.Length, data = iris)
#note
(.871)^2 #is equal to 0.759, multiple r2 from lm output

#spearman rank####
monkey <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/monkey.csv")
cor.test(~ eggs_per_gram + Dominance_rank, monkey, method = "spearman")
cor.test(monkey$eggs_per_gram, monkey$Dominance_rank, method = "spearman")


#permutation####
library(coin)
independence_test(Sepal.Length ~ Petal.Length, iris)

#leverage and outliers
iris_new <- iris
iris_new$Source <- "original"
#make outlier
iris_outlier <- data.frame(Petal.Length = c(2.5,12, 12.1),
                           Sepal.Length = c(5.4,8.9, 3), 
                           Source = "new")
iris_merged <- merge(iris_new, iris_outlier, all = T)

ggplot(iris_merged, aes(x=Petal.Length, y=Sepal.Length, color=Source)) +
  geom_point(size = 3) +
  ylab("Sepal Length")+ggtitle("Sepal length increases with petal length")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  xlab("Petal length (cm)") +
  ylab("Sepal length (cm)")



iris_regression_merged <- lm(Sepal.Length ~ Petal.Length, iris_merged)
plot(iris_regression_merged)
iris_merged[1,]

iris_regression_merged <- lm(Sepal.Length ~ Petal.Length, iris_merged[-1,])
plot(iris_regression_merged)
