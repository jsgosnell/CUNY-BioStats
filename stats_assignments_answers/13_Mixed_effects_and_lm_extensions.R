#1

snail <- read.csv("http://jstephengosnell.com/teaching-resources/datasets/snail_modified_for_class.csv")
head(snail)
snail$Treatment <- as.factor(snail$Treatment)
require(plyr)
snail$Treatment_new <- revalue(snail$Treatment, c("1" = "Control", "2" = "Single predator",
                                                  "3" = "Two predators"))

snail[snail$Treatment_new == "Control", "FL"] <- snail[snail$Treatment_new == "Control", "FL"] + 2
require(lme4)
snail_mm <- lmer(FL ~ Treatment_new + (1|Container), snail)
summary(snail_mm)
plot(snail_mm)
require(car)
Anova(snail_mm, type = "III")

require(multcomp)
snail_comparison <- glht(snail_mm, linfct = mcp(Treatment_new = "Tukey"))
summary(snail_comparison)

#2
whelk <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/whelk.csv?attredirects=0&d=1")
head(whelk)
summary(whelk)
whelk_plot <- ggplot(whelk, aes_string(x="Shell.Length", y = "Mass")) +
  geom_point(aes_string(colour = "Location")) + 
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
whelk_plot
#power fit
whelk_lm <- lm(Mass ~ Shell.Length, whelk, na.action = na.omit)

whelk_power <- nls(Mass ~ b0 * Shell.Length^b1, whelk, 
                   start = list(b0 = 1, b1=3), na.action = na.omit)
whelk_exponential <- nls(Mass ~ exp(b0 + b1 * Shell.Length), whelk, 
                   start = list(b0 =1, b1=0), na.action = na.omit)
AICc(whelk_lm, whelk_power, whelk_exponential)

#plot
whelk_plot + geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "orange")+ 
  geom_smooth(method="nls", 
              # look at whelk_power$call
              formula = y ~ b0 * x^b1, 
              method.args = list(start = list(b0 = 1, 
                                              b1 = 3)), 
              se=FALSE, size = 1.5, color = "blue") +
  geom_smooth(method="nls", 
              # look at whelk_exponential$call
              formula = y ~ exp(b0 + b1 * x), 
              method.args = list(start = list(b0 = 1, 
                                              b1 = 0)), 
              se=FALSE, size = 1.5, color = "green")
  

#3

team <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/team_data_no_spaces.csv?attredirects=0&d=1")
elevation_linear <- gam(PlotCarbon.tonnes ~ Elevation, data = team)
elevation_gam <- gam(PlotCarbon.tonnes ~ s(Elevation), data = team)
elevation_gamm <- gamm(PlotCarbon.tonnes ~s(Elevation), random = list(Site.Name = ~ 1), data = team)
AICc(elevation_gam, elevation_gamm, elevation_linear)

#4
require(rpart)
head(kyphosis)
kyphosis_tree_information <- rpart(Kyphosis ~ ., data = kyphosis, 
                               parms = list(split = 'information'))
plot(kyphosis_tree_information)
text(kyphosis_tree_information)

kyphosis_tree_gini <- rpart(Kyphosis ~ ., data = kyphosis)
plot(kyphosis_tree_gini)
text(kyphosis_tree_gini)