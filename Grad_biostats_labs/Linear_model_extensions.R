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
#suggets primate isn't important

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
sleep_lm_full <- lm(TotalSleep~.-Species, sleep)
summary(sleep_lm_full)
drop1(sleep_lm_full)
#bottom_up uses add1

#compare relationships among variables
pairs(sleep)
#hard to read
require(car)
vif(sleep_lm_full) #<5 is good

#other comparison methods that don't rely on changing given model
#which is heavily impacted by correlation
#AIC, BIC
require(MASS)
stepAIC(sleep_lm_full)
#issue wiht missing values
sleep_lm_full <- lm(TotalSleep~.-Species, na.omit(sleep))
stepAIC(sleep_lm_full)

#automated
require(MuMIn)
?dredge
options(na.action = "na.fail")
auto <- dredge(sleep_lm_full)
write.csv(auto, "dredge_output.csv", row.names = F)
#model.avg for output

#mixed models
#lme4 and nlme are main packages to do this
#nlme sometimes better for fitting odd correlation structures, but otherwise I
#to use lme4

#to see how these work, lets revisit our lizard data

f <-"http://csivc.csi.cuny.edu/Lisa.Manne/files/classes/biol78002/Clark1972.csv"
clark<-read.csv(f, header=T)
str(clark)
head(clark)

#remember issue with class...
clark$class_new <- as.factor(clark$class)
levels(clark$class_new)
require(plyr)
clark$class_new <- revalue(clark$class_new, c("1" = "eight_toes_clipped", "2" = "one_toe_clipped"))

require(reshape)
cast(clark, class_new ~ ., value="total", sum)

#analysis with paired t-test
clark1 <- clark[clark$class_new == "eight_toes_clipped" & clark$toes <8, ]
clark2 <- clark[clark$class_new == "one_toe_clipped" & clark$toes > 1, ]
clark1
clark2
#big idea here is you are pairing based on number of toes clipped
t.test(clark1$precaptures, clark2$precaptures)
t.test(clark1$precaptures, clark2$precaptures, paired=T)

#how does this compare to
require(lme4)
fit <- lmer(precaptures ~ class_new + (1|toes), clark[clark$toes >1 & clark$toes < 8,])
summary(fit)
require(car)
Anova(fit, type = "III", test.statistic = "F")
#for glmer this will have to be Chi-square test, use F for normal tests
#LIkelihood ratio test is equivalent to Chisq
#or
fit_under <- lmer(precaptures ~  (1|toes), clark[clark$toes >1 & clark$toes < 8,])
anova(fit,fit_under)

#note
Anova(fit, type = "III", test="F")
#is different than
Anova(fit, type="III")


#what if linear model doesn't seem correct
#glm or glmer for mixed effects
#put these together
#data on cervical lenth of deer
DeerEcervi <- read.table("http://sites.google.com/site/stephengosnell/teaching-resources/datasets/DeerEcervi.txt", header = T)
DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
#change to presence/absence of Ecervi
DeerEcervi$Ecervi.01[DeerEcervi$Ecervi>0] <-1
DeerEcervi$fSex <- factor(DeerEcervi$Sex)
DeerEcervi$CLength <- DeerEcervi$Length - mean(DeerEcervi$Length)
DeerEcervi$fFarm <- factor(DeerEcervi$Farm)
DE.glm<-glm(Ecervi.01 ??? CLength * fSex+fFarm,
            data = DeerEcervi, family = binomial)
#outcome here is just presence/absence, so don't worry about dispersion
#unless you want ot include a observation level random effect
summary(DE.glm)
drop1(DE.glm, test = "Chi")
step(DE.glm)

#what abbout as mixed model
DE.lme4 <- glmer(Ecervi.01 ??? CLength * fSex +
                   (1 | fFarm), family = binomial,
                 data = DeerEcervi)
summary(DE.lme4)
AIC(DE.lme4, DE.glm)
#good for small sample sizes, eg n/K <40
AICc(DE.lme4, DE.glm)

#generalized additive model (gam)
#non-linear model
TN <- read.table("http://sites.google.com/site/stephengosnell/teaching-resources/datasets/TeethNitrogen.txt",
                 header=T)
Moby <- subset(TN, TN$Tooth == "Moby")
Moby_lm <- lm(X15N ??? Age, data = Moby)
op <- par(mfrow = c(2, 2))
plot(M2, add.smooth = FALSE)
par(op)
#what issue do you see?
require(mgcv)
require(MASS)
Moby_s <- gam(X15N ~ s(Age),data=Moby)
summary(Moby_s)
AIC(Moby_s, Moby_lm)
