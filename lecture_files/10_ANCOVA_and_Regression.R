#from lecture on ancova and multiple regression

#cholesterol example####

cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$patient <- as.factor(cholesterol$patient)
head(cholesterol)
summary(cholesterol)

require(ggplot2)
ggplot(cholesterol, aes_string(x="day", y="cholest")) +
  geom_point(size = 3) +
  ylab("Cholesterol level")+ggtitle("Cholesterol level following a heart attack")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#as regression####
cholest_regression <- lm(cholest ~ day, na.omit(cholesterol))
par(mfrow = c(2,2))
plot(cholest_regression)
require(car)
Anova(cholest_regression, type = "III")

#as ANOVA####
cholesterol$day <- as.factor(cholesterol$day)
cholest_blocked_all_days <- lm(cholest ~ day + patient, na.omit(cholesterol))
Anova(cholest_blocked_all_days, type = "III")

#as multiple regression####
cholesterol$day <- as.numeric(as.character(cholesterol$day))
cholest_multiple <- lm(cholest ~ day + patient, na.omit(cholesterol))
plot(cholest_multiple)
Anova(cholest_multiple, type = "III")
summary(cholest_multiple)

#graph####

ggplot(cholesterol, aes_string(x="day", y="cholest", color = "patient")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F) +
  ylab("Cholesterol level")+ggtitle("Cholesterol level following a heart attack")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#linear model####
model.matrix(cholest_multiple)[1,]
coef(cholest_multiple)

#ancova with interactions example - simulated
iris_example_species <-data.frame(
  Species = c(rep("x",25), rep("y", 25), rep("z", 25)),
  Sepal_Length = runif(75,2,4 ),
  #no difference based on species or sepal length
  Petal_no_impacts = runif (75, 4, 6))
#no difference based on species
iris_example_species$Petal_no_impact_species <- 
  iris_example_species$Sepal_Length * 2 + rnorm(75)
#no impact of petal length
iris_example_species$Petal_no_relationship <-rnorm(75) +
  c(rep(2,25), rep(3,25), rep(4,25))
#impact of species and petal length but no interaction
iris_example_species$Petal_no_interaction <- 
  iris_example_species$Sepal_Length * 2 + rnorm(75) +
  c(rep(2,25), rep(3,25), rep(4,25))
#impact of species and petal length with interaction
iris_example_species$Petal_interaction <- 
  iris_example_species$Sepal_Length * c(rep(-2, 25),rep(2,25), rep(5,25)) + 
  c(rep(2,25), rep(3,25), rep(4,25)) + rnorm(75)

#no impact species or relationship
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_no_impacts, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
Anova(lm(Petal_no_impacts ~ Sepal_Length * Species, iris_example_species), 
      type = "III")
Anova(lm(Petal_no_impacts ~ Sepal_Length + Species, iris_example_species), 
      type = "III")
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_no_impacts, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_smooth(method = "lm", se = F)

#no impact species but relationship
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_no_impact_species, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
Anova(lm(Petal_no_impact_species ~ Sepal_Length * Species, iris_example_species), 
      type = "III")
Anova(lm(Petal_no_impact_species ~ Sepal_Length + Species, iris_example_species), 
      type = "III")
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_no_impact_species, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_smooth(method = "lm", se = F)

# impact species but no relationship
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_no_relationship, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
Anova(lm(Petal_no_relationship ~ Sepal_Length * Species, iris_example_species), 
      type = "III")
Anova(lm(Petal_no_relationship ~ Sepal_Length + Species, iris_example_species), 
      type = "III")
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_no_relationship, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_smooth(method = "lm", se = F)
require(multcomp)
summary(glht(lm(Petal_no_relationship ~ Sepal_Length + Species, iris_example_species), 
             linfct =mcp(Species = "Tukey")))


# impacts species and relationship but no interaction
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_no_interaction, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
Anova(lm(Petal_no_interaction ~ Sepal_Length * Species, iris_example_species), 
      type = "III")
Anova(lm(Petal_no_interaction ~ Sepal_Length + Species, iris_example_species), 
      type = "III")
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_no_interaction, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_smooth(method = "lm", se = F)
summary(glht(lm(Petal_no_interaction ~ Sepal_Length + Species, iris_example_species), 
             linfct =mcp(Species = "Tukey")))

# impacts species and relationship with interaction####
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_interaction, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
Anova(lm(Petal_interaction ~ Sepal_Length * Species, iris_example_species), 
      type = "III")
ggplot(iris_example_species, aes(x= Sepal_Length, y = Petal_interaction, color = Species)) +
  geom_point(size = 3)+
  xlab("Sepal Length") +
  ylab("Petal Length") +
  ggtitle("Impact of Sepal Length and Species on Petal Length") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_smooth(method = "lm", se = F)

#interpret interactions (remove intercept for ease)
coef(lm(Petal_interaction ~ Sepal_Length * Species - 1, iris_example_species))

summary(lm(Petal_interaction ~ Sepal_Length, 
         iris_example_species[iris_example_species$Species == "x",]))
summary(lm(Petal_interaction ~ Sepal_Length, 
           iris_example_species[iris_example_species$Species == "y",]))
summary(lm(Petal_interaction ~ Sepal_Length, 
           iris_example_species[iris_example_species$Species == "z",]))

#fev data####
fev <- read.table("http://www.statsci.org/data/general/fev.txt", header = T)
head(fev)
fev_age <- lm(FEV ~ Age, fev)
plot(fev_age)
Anova(fev_age, type = "III")
summary(fev_age)

#age plot####
ggplot(fev, aes_string(x="Age", y="FEV")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  ylab("FEV")+ggtitle("FEV increases with age")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

fev_height <- lm(FEV ~ Height, fev)
plot(fev_height)
Anova(fev_height, type = "III")
summary(fev_height)

#height plot####
ggplot(fev, aes_string(x="Height", y="FEV")) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  ylab("FEV")+ggtitle("FEV increases with height")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

fev_gender <- lm(FEV ~ Sex, fev)
plot(fev_gender) #anova is fine
Anova(fev_gender, type = "III")
summary(fev_gender)

#gender plot ####

#bar chart with error bars ####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  #  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

function_output <- summarySE(fev, measurevar="FEV", groupvars =
                               c("Sex"))

ggplot(function_output, aes_string(x="Sex", y="mean")) +
  geom_col(size = 3) +
  ylab("FEV") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#larger models####
#check for correlation
pairs(fev)
#modification gives you r2 to compare, size of text shows how correlated they are,
#and asterisks indicate level of significance using bivariate relationships
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
pairs(fev, lower.panel=panel.smooth, upper.panel=panel.cor)

#prove what this is showing
cor.test(~ Age + Height, fev)

#build large model
fev_full <- lm(FEV ~ Age * Height * Sex, fev)
plot(fev_full)
Anova(fev_full, type = "III")
summary(fev_full)

#all significant, for other option
drop1(fev_full, test="F")

#report on model
Anova(fev_full, type = "III")
summary(fev_full)

#forward selection
fev_under <- lm(FEV ~ 1, fev)
add1(fev_under, ~ Age + Height + Sex, test = "F")

fev_under_a <- update(fev_under, .~. + Age)
add1(fev_under_a, ~ . + Height + Sex, test = "F")

#with AIC ####
require(MASS)
final_chosen_via_AIC <- stepAIC(fev_full)


#build large model wiht interactions
fev_full <- lm(FEV ~ Age * Height * Sex, fev)
plot(fev_full)
Anova(fev_full, type = "III")
summary(fev_full)

#remember likelihood####
#example from http://www.johnmyleswhite.com/notebook/2010/04/21/doing-maximum-likelihood-estimation-by-hand-in-r/
#
#determine most likely value of p for a bernoulli variable
p.parameter <- runif(1)
sequence <- rbinom(10, 1, p.parameter)
#
#write a likelihood function to solve for likelihood
likelihood <- function(sequence, p.parameter)
{
  likelihood <- 1
  
  for (i in 1:length(sequence))
  {
    if (sequence[i] == 1)
    {
      likelihood <- likelihood * p.parameter
    }
    else
    {
      likelihood <- likelihood * (1 - p.parameter)
    }
  }
  
  return(likelihood)
}

#then solve and plot
possible.p <- seq(0, 1, by = 0.001)
library('ggplot2')
likelihood_plot <- qplot(possible.p,
                         sapply(possible.p, function (p) {likelihood(sequence, p)}),
                         geom = 'line',
                         main = paste('Likelihood as a Function of P when p = ', p.parameter),
                         xlab = 'P',
                         ylab = 'Likelihood')
likelihood_plot +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


#TEAM example####
team <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/team_data_no_spaces.csv")
names(team)
head(team)

#lets build a model looking at variation among continents####
#this is an ANOVA
team_model_1 <-lm(PlotCarbon.tonnes ~ Continent, team)
#plot to see if assumptions are met. remember what these are (from Lisa Manne)
# The Residuals vs. Fitted values plot should show no structure; it should not show a trend of residuals against fitted values. The variance should not increase or decrease as you move along the x-axis.  The residuals should be centered around 0.
# The Quantile-quantile plot tests whether your residuals are normally distributed.  It plots each data point vs. its position in a theoretical normal distribution.  If the residuals are normally distributed, the plot will look like a straight line.
# The scale-location plot is another plot of residuals vs. fitted values:  it shows the square root of the standardized residuals against the fitted values.  This plot may more clearly show if there is an issue with the variance increasing with the mean (in that case the scatter would increase as the fitted values increased).
# The residuals vs. leverage plot also includes Cook's distance (pale blue dashed line).  Cook's Distance compares the fitted response of the regression which uses every data point, against the fitted response of the regression where a particular data point has been dropped from the analysis (and then sums this difference across all data points). Very influential data points (on the parameter estimates) are identified, and are labeled in this plot.  If there are heavily influential data points, you might consider re-doing the regression model after removing them.
par(mfrow =c(2,2))
plot(team_model_1)
Anova(team_model_1, type = "III")
summary(team_model_1)

#where's the "almost" difference? use Tukey's HSD for all pairs
require(multcomp)
compare_cont_tukey <- glht(team_model_1, linfct = mcp(Continent = "Tukey"))
summary(compare_cont_tukey)

#we could do one by one,but we have a lot of variables
#instead, we can use top-down or bottom-up approaches to compare

#top-down approach####
#for top-down, start with a model containing all variables you may wish to include
#and interactions.  You should ensure these are not highly correlated before including
#pairs let you visualize this.let's try continent, shannon diversity, phylogenetic 
#diveristy, a few rao q measures, rainfall, and elevation
#
#for ease just pull those out (not required, just for viewing here)
team_potential <- team[,colnames(team) %in% c(#site specific
  "Continent",
  #diversity
  "shannon", "wd.RaoQ", "maxdbh.RaoQ", "CWM.wd", "CWM.maxdbh", "PD",
  #environmental
  "Precip_mean.mm", "Elevation",
  #outcome
  "PlotCarbon.tonnes")]

pairs(team_potential, lower.panel=panel.smooth, upper.panel=panel.cor)
#some significant relationships, and shannon and pd are highly correlated...may 
#not be the best idea, but let's continue on

#. means all other variables
team_model_full <- lm(PlotCarbon.tonnes ~ ., 
                      team_potential)
drop1(team_model_full)
#you can drop one at a time. remember goal is lowest AIC. this is still a step-down
#method

team_model_full_a <- update(team_model_full, .~. - PD)
drop1(team_model_full_a)

team_model_full_b <- update(team_model_full_a, .~. - CWM.wd)
drop1(team_model_full_b)
#... and so on until we see <none> as the lowest AIC
#can specify test as well. F is ok for linear model. then drop highest p-value and work
#down until all significant
drop1(team_model_full, test = "F")

#add1 uses same idea but for smallest model. build small model. 1 means intercept
team_model_empty <- lm(PlotCarbon.tonnes ~ 1, team_potential)
#then add whatever you may want to consider in the argument (just doing smaller
#set here for ease)
add1(team_model_empty, ~ shannon + wd.RaoQ )
# 
add1(team_model_empty, ~ shannon + wd.RaoQ, test = "F" )

#or R can do this for us. default is backwards
stepAIC(team_model_full)
#save as object to get output
stepAIC_final <- stepAIC(team_model_full)

#or we can go forward
stepAIC(team_model_empty, ~ shannon + wd.RaoQ, direction = "forward")

#remember to check assumptions for final model
plot(stepAIC_final)
#you can also check variance inflation factors (vif) for final model to see 
#if they are too big (>5 indicates high correlation among factors)
vif(stepAIC_final) #all good
Anova(stepAIC_final, type ="III")
summary(stepAIC_final)

#these are all step-wise methods. we can also use AIC to do a full model search
require(MuMIn)
?dredge
options(na.action = "na.fail")
auto <- dredge(team_model_full)
write.csv(auto, "dredge_output.csv", row.names = F)
#model.avg for output. can decide how far delta can go. look at dredge output
head(auto)
#can average top models
model.avg(auto, subset = delta < 4) #notes uses AICc, best for small samples or 
#where you have to estimate lots of parameters
#to get the top 1
top_model <- get.models(auto, subset = 1)[[1]]
#check
plot(top_model)
vif(top_model)

