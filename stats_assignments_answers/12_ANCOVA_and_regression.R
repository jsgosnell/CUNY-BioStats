#1####
#
heat <- read.table("http://www.statsci.org/data/general/insulgas.txt", 
                   header = T)
head(heat)
pairs(heat)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
pairs(heat, lower.panel=panel.smooth, upper.panel=panel.cor)
heat_big <- lm(Gas ~ Insulate * Temp, heat)
plot(heat_big)
require(car)
Anova(heat_big, type = "III")

ggplot(heat, aes_string(x="Temp", y="Gas", color = "Insulate")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#2####
cherry <- read.table("http://www.statsci.org/data/general/cherry.txt",
                     header = T)
head(cherry)
cherry_full <- lm(Volume ~ Diam + Height, cherry)
plot(cherry_full)
Anova(cherry_full, type = "III")

#3####
horse <- read.table("http://www.statsci.org/data/oz/horses.txt",
                  header = T)
horse_full <- lm(Position ~ ., horse)
horse_final <- stepAIC(horse_full)
Anova(horse_final, type = "III")
summary(horse_final)

require(MuMIn)
options(na.action = "na.fail")
auto <- dredge(horse_full)
write.csv(auto, "dredge_output.csv", row.names = F)
top_model <- get.models(auto, subset = 1)[[1]]
Anova(top_model, type ="III")
summary(top_model)

model.avg(auto, subset = delta < 4) #notes uses AICc, best for small samples or 

#4
pulse_class_copy <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/pulse_class_copy.csv")
pulse_class_copy$Gender <- as.factor(pulse_class_copy$Gender)
pulse_class_copy$Smokes <- as.factor (pulse_class_copy$Smokes)
pulse_class_copy$Alcohol <- as.factor(pulse_class_copy$Alcohol)

require(MuMIn)
pulse_full <- lm(Change ~ ., pulse_class_copy )
pulse_final <- step(pulse_full)
#consider assumptions
plot(pulse_final)
Anova(pulse_final, type = "III")


#or
require(MuMIn)
options(na.action = "na.fail")
auto <- dredge(pulse_full)
write.csv(auto, "dredge_output.csv", row.names = F)
options(na.action = "na.omit")

