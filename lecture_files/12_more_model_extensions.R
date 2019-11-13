#code for gamma distribution####
#death data from crawley
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



