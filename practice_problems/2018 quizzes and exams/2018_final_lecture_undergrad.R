#1####

implant <- matrix(c(8,9,17,14,7,3,3,2), ncol = 4, byrow = T)
implant_table <- as.table(implant)
chisq.test(implant)
chisq.test(implant)$expected

fisher.test(implant)

#2####
binom.test(x=48, n=63, p=.6)

#4####
pollution <- read.table("http://www.statsci.org/data/general/wolfrive.txt", header = T)
depth_model <- lm(Aldrin ~ Depth, pollution)
plot(depth_model)
require(car)
Anova(depth_model)
require(multcomp)
summary(glht(depth_model, linfct = mcp(Depth = "Tukey")))

#5####
require(ggplot2)
function_output <- summarySE(pollution, measurevar = "Aldrin", groupvars = "Depth")
ggplot(function_output, aes_string(x = "Depth", y = "mean")) + 
  geom_col(aes_string()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  xlab("Toxin level")+
  ylab("Reading")+
  ggtitle("Your friend's results")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#6####
pollutant_model <- lm(Aldrin ~ HCB * Depth, pollution)
plot(pollutant_model)
Anova(pollutant_model, type = "III")
pollutant_model <- lm(Aldrin ~ HCB + Depth, pollution)
plot(pollutant_model)
Anova(pollutant_model, type = "III")
pollutant_model <- lm(Aldrin ~ HCB, pollution)
plot(pollutant_model)
Anova(pollutant_model, type = "III")
summary(pollutant_model)

cor.test(~Aldrin + HCB, pollution, method = "spearman")

#7####
#Aldrin5yl <- pollution$Aldrin - .5 - rnorm(length(pollution$Aldrin)) * .1
 # Aldrin5yl <-data.frame(Depth = pollution$Depth, Aldrin5yl = Aldrin5yl)
 # write.csv(Aldrin5yl, "aldrin_five_year_later.csv", row.names = F)
Aldrin5yl <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/aldrin_five_year_later.csv")
t.test(pollution$Aldrin, Aldrin5yl$Aldrin5yl)
t.test(pollution$Aldrin, Aldrin5yl$Aldrin5yl, paired = T)


