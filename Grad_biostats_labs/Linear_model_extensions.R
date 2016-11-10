#extensions to the linear model
#first, selecting among multiple models

#lets go back to the sleep dataset from last week
#sleep
sleep <- read.csv("http://sites.google.com/site/stephengosnell/teaching-resources/datasets/sleep.csv")
head(sleep)
sleep_lm <- lm(TotalSleep ~ BrainWt + BodyWt + LifeSpan, sleep)
summary(sleep_lm)

#add in primate
sleep_lm_2 <- lm(TotalSleep ~ BrainWt + BodyWt + LifeSpan+Primate, sleep)
summary(sleep_lm)

#we can compare any two models using

anova(sleep_lm_2, sleep_lm)

#compare to
require(car)
Anova(sleep_lm_2)
Anova(sleep_lm_2, type="III")

#but remember
sleep_lm <- lm(TotalSleep ~ BrainWt*BodyWt+ LifeSpan, sleep)
sleep_lm_2 <-update(sleep_lm, .~. + Primate)
Anova(sleep_lm_2)
Anova(sleep_lm_2, type="III")
#always using Type III is safer!

#but there are so many models we could build and multiple ways
#top-down
#bottom_up

#other comparison methods that don't rely on changing given model
#which is heavily impacted by correlation
#AIC, BIC

#what if linear model doesn't seem correct
#generalized additive model (gam)
#non-linear model (nls)
