#lecture on mixed models and other lm extensions

#generalized linear models####
#what if the outcome isn't continuous but is either 0/1 (presence/absence) or 
#a proportion?
#We use a generalized linear model, aka logistic regression
#use glm command (instead or arcsin transform!)
#data from Needles et al 2014
otters <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/needles_january.csv")
head(otters)
#star, otters, and mussels are treatments (1 is present)
#WASU and notWASU are count data
otter_fit <- glm(cbind(WASU, notWASU)~ Star + Otters + Mussels, otters, family=binomial)
Anova(otter_fit, type = "III")
summary(otter_fit)
#same basic assumption
plot(otter_fit)
#can use drop1, stepAIC, etc


deaths <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/deaths.csv")
str(deaths)
library(Rmisc)
summarySE(deaths, measurevar = "death", groupvars = "treatment")
#graph it####
library(ggplot2)
ggplot(summarySE(deaths, measurevar = "death", groupvars = "treatment"), 
       aes ( x= treatment, y = death)) +
  geom_col() +
  geom_errorbar(aes(ymin = death - ci, ymax = death + ci))

#wrong order! so I go back to beginning and do all again
deaths$treatment <- factor(deaths$treatment, c("control", "low", "high"))
summarySE(deaths, measurevar = "death", groupvars = "treatment")
#graph it####
ggplot(summarySE(deaths, measurevar = "death", groupvars = "treatment"), 
       aes ( x= treatment, y = death)) +
  geom_col() +
  geom_errorbar(aes(ymin = death - ci, ymax = death + ci))+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#lm approach#####
fit_lm <- lm(death~treatment, deaths)
par(mfrow=c(2,2))
plot(fit_lm)
library(car)
Anova(fit_lm, type = "III")
#significant omnibus so test
library(multcomp)
summary(glht(fit_lm, linfct = mcp(treatment = "Tukey")))

#gamma####
gamma_data <- data.frame(x=rgamma(100000, shape = 3, scale = 2))

ggplot(data = gamma_data, aes(x = x)) + 
  geom_histogram()+
  ylab("Frequency") + xlab("Time until 3 deaths") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

ggplot(data = gamma_data, aes(x = x)) + 
  geom_density() +  
  ylab("Proportion") + xlab("Time until 3 deaths") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))


ggplot(data = gamma_data, aes(x = x)) + 
  geom_density() +     
  stat_function(fun = dgamma, args = list(shape =3, scale = 2),size = 1, color = "green")+
  ylab("Proportion") + xlab("Time until 3 deaths") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#glm approach#####
fit_gamma <- glm(death~treatment, deaths, family = Gamma)
par(mfrow=c(2,2))
plot(fit_gamma)
library(car)
Anova(fit_gamma, type = "III")
#significant omnibus so test
library(multcomp)
summary(glht(fit_gamma, linfct = mcp(treatment = "Tukey")))

#compare
AIC(fit_lm, fit_gamma)

summary(fit_gamma)

#translate to impact
#for high treatment
1/(.289017 - .143669)

#censored data####
sheep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sheep.deaths.csv")
head(sheep)
str(sheep)

library(survminer)

survival_fit <- survfit(Surv(death, status) ~ group, data = sheep)
ggsurvplot(survival_fit, data = sheep)
survival_model <- survreg(Surv(death, status) ~ group, data = sheep)
summary(survival_model)
tapply(predict(survival_model,type="response"),sheep$group,mean)

#compare to lm
survival_model_lm <- lm(death~group, data = sheep)
summary(survival_model_lm)
tapply(predict(survival_model_lm,type="response"),sheep$group,mean)

#mixed models

team <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/team_data_no_spaces.csv")
names(team)
head(team)

#for ease just pull those out (not required, just for viewing here)
team_potential <- team[,colnames(team) %in% c(#site specific
  "Continent",
  #diversity
  "shannon", "wd.RaoQ", "maxdbh.RaoQ", "CWM.wd", "CWM.maxdbh", "PD",
  #environmental
  "Precip_mean.mm", "Elevation",
  #outcome
  "PlotCarbon.tonnes")]

#from lecture 10 code
##. means all other variables
team_model_full <- lm(PlotCarbon.tonnes ~ ., 
                      team_potential)
stepAIC(team_model_full)

#mixed models####
#but we need to consider mixed model here, since each plot has 6+ sites at it
require(lme4) #nlme is another package thats good if you need covariance structures
#start with same full model
team_model_full_mm <- lmer(PlotCarbon.tonnes ~ 
                             #site specific
                             Continent +
                             #diversity
                             shannon + wd.RaoQ + maxdbh.RaoQ + CWM.wd + CWM.maxdbh +
                             PD +
                             #environmental
                             Precip_mean.mm + Elevation +
                             #random portion, new notation
                             (1|Site.Name), team)
summary(team_model_full_mm)

#now to do top-down test we have to use Chi-squared tests (not F); not fully shown here
library(car)
Anova(team_model_full_mm, type = "III")
stepAIC(team_model_full_mm) # won't work with mixed models, so have to do manually
drop1(team_model_full_mm)
drop1(team_model_full_mm, test = "Chi")

#dredge will work, but may be slow
library(MuMIn)
options(na.action = "na.fail")
auto <- dredge(team_model_full_mm)
#write to csv to observe if needed, its sorted by AICc values so top line is bst model
#still should check assumptions
write.csv(auto, "dredge_output.csv", row.names = F)
team_final_mm <- get.models(auto, subset = delta < 4, REML = T)  
#easy error, just take top
team_final_mm <- get.models(auto, subset = 1, Re)[[1]]
#use function check_mixed_model to evaluate mixed model
check_mixed_model <- function (model, model_name = NULL) {
  #collection of things you might check for mixed model
  par(mfrow = c(2,3))
  #not sure what this does with mutliple random effects, so stop with 1 for now
  if(length(names(ranef(model))<2)){
    qqnorm(ranef(model, drop = T)[[1]], pch = 19, las = 1, cex = 1.4, main= paste(model_name, 
                                                                                  "\n Random effects Q-Q plot"))
  }
  plot(fitted(model),residuals(model), main = paste(model_name, 
                                                    "\n residuals vs fitted"))
  qqnorm(residuals(model), main =paste(model_name, 
                                       "\nresiduals q-q plot"))
  qqline(residuals(model))
  hist(residuals(model), main = paste(model_name, 
                                      "\nresidual histogram"))
}

check_mixed_model(team_final_mm)
#r2 equivalent using r.squaredGLMM
r.squaredGLMM(team_final_mm) #m is for fixed effects (transferable) and c is for
#full model with random effects


#glmm
#but this is a mixed-model again! multiple measures per piling
otter_fit_mm <- glmer(cbind(WASU, notWASU)~ Star + Otters + Mussels + (1|Piling), otters, family=binomial)
Anova(otter_fit_mm, type = "III") #uses chisq test
#check assumptions
#function belows passed from R-sig-ME discussion on checking for overdispersion  
dispersion_glmer <- function(modelglmer){
  ## computing  estimated scale  ( binomial model)
  #following  D. Bates :
  #That quantity is the square root of the penalized residual sum of
  #squares divided by n, the number of observations, evaluated as:
  n <- length(residuals(modelglmer))
  return(  sqrt( sum(c(residuals(modelglmer), modelglmer@u) ^2) / n ) )
}
dispersion_glmer(otter_fit_mm)
#not overdispered

drop1(otter_fit_mm, test = "Chi") 
otter_fit_mm_a <- update(otter_fit_mm, .~. - Star)
Anova(otter_fit_mm_a, type = "III") #uses chisq test
otter_fit_mm_b <- update(otter_fit_mm_a, .~. - Mussels)
Anova(otter_fit_mm_b, type = "III") #uses chisq test
r.squaredGLMM(otter_fit_mm_b)

#nls is used to fit specified functions in R####
whelk <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/whelk.csv")
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
#linear fit
whelk_lm <- lm(Mass ~ Shell.Length, whelk, na.action = na.omit)
#power fit
whelk_power <- nls(Mass ~ b0 * Shell.Length^b1, whelk, 
                   start = list(b0 = 1, b1=3), na.action = na.omit)
AICc(whelk_lm, whelk_power)
whelk_plot + geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "orange")+ 
  geom_smooth(method="nls", 
              # look at whelk_power$call
              formula = y ~ b0 * x^b1, 
              method.args = list(start = list(b0 = 1, 
                                              b1 = 3)), 
              se=FALSE, size = 1.5, color = "blue") 

#generalized additive model (gam)####
#non-linear model

#compare fit to whelk mass-length to that produced by gam
require(mgcv)
require(MASS)
#this just produces an lm
whelk_gam_lm <- gam(Mass ~ Shell.Length, data = whelk)
summary(whelk_gam_lm)
plot(whelk_gam_lm)
#or we can specify using spline
whelk_gam_spline <- gam(Mass ~ s(Shell.Length), data = whelk)
summary(whelk_gam_spline)
#gam.check(whelk_gam_spline)
plot(whelk_gam_spline)
#or using ggplot2
whelk_plot + geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "orange")+ 
  geom_smooth(method="nls", 
              # look at whelk_power$call
              formula = y ~ b0 * x^b1, 
              method.args = list(start = list(b0 = 1, 
                                              b1 = 3)), 
              se=FALSE, size = 1.5, color = "blue") + 
  geom_smooth(stat= "smooth", method = "gam", formula = y ~ s(x), 
                         color = "yellow")

#can compare fits with AIC
AICc(whelk_gam_spline, whelk_lm, whelk_power)









