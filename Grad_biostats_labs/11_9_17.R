#11/9/17 class script

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
#

#what if the outcome isn't numerical
#generalized linear model

#aka,logistic regression
#use glm command (instead or arcsin transform!)
anthrax <- read.csv("http://sites.google.com/site/stephengosnell/teaching-resources/datasets/anthrax.csv")
head(anthrax)
#we have to put data in success/failure set
logistic_fit <- glm(cbind(survived, died)~ anthraxConcentration, anthrax, family=binomial)
summary(logistic_fit)
Anova(logistic_fit, type="III") # notice we switched to a deviance table
#data can also be put in for each individual (just as 1/0)
#you should really check dispersion here to make sure data isn't over-dispered

#see ?glm for other parameters, but most common other form is poisson for count data
#poisson_fit=glm(y~x1+x2, data, family=possion)

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


#nls is used to fit specified functions in R
f <- "http://csivc.csi.cuny.edu/Lisa.Manne/files/classes/biol78002/2930_Callipepla_squamata_all_factors.txt"

calli <- read.table(f, header=T)  # data file is .txt, so need read.table
head(calli)

## Key to variables:
## long4:  longitude      lat4:  latitude
## log_avg_abun:  average abundance of Callipepla squamata in the years 1995-2005, log transformed
## log_bm_comp / log_guild_comp_abun / log_family_abun:  abundance of spatially coincident species within 75% and 125% of Callipepla squamata's (scaled quail) biomass; abundance of spatially coincident species in the same guild; abundance of spatially coincident family members
##  pred_all_abun/ pred_main_abun :  abundances of all predatory birds, or predatory birds that have birds as the main part of their diet
## log_NPP / ndvi:  Net Primary Productivity (log) or Normalized Differential Vegetation Index
## AvgOfatr:  Annual temperature range
## AvgOfhtwm:  hottest temperature in the warmest month
## AvgOfmpdm:  mean precipitation in the driest month
## AvgOfmph2oq :  mean precipitation in the wettest quarter
## AvgOfmtwq :  mean temperature in the warmest quarter
## AvgOfsdmp/ AvgOfsdmt:  std deviation of mean precipitation / mean temperature
## hab_suit/ avg_of_dis:  habitat suitability (1,0) / distance to nearest optimal habitat


##log_avg_abun is the response, and this file contains a number of possible useful predictors for this abundance.

#lets fit a model.  you must specify the variable (c) and an initial vector.
#the curve is fit using the Newton-Raphson procedure by default

m2<-nls(log_avg_abun~c/ndvi, data=calli, start=list(c=1))
summary(m2)
AIC(m2)

#trees
corn <- read.csv("http://csivc.csi.cuny.edu/Lisa.Manne/files/classes/biol78002/corn_yield.csv")
head(corn)
require(tree)
corn_tree_model <- tree(corn)#uses first term as response if not specified
corn_tree_model <- tree(log_yield~., corn)#better to specify
plot(corn_tree_model)
text(corn_tree_model)
prune.tree(corn_tree_model)
plot(prune.tree(corn_tree_model))
corn_tree_model_2 <- prune.tree(corn_tree_model, best = 4)
plot(corn_tree_model_2)
text(corn_tree_model_2)

#validation techniques

dove_or_waxwing <- read.csv("http://csivc.csi.cuny.edu/Lisa.Manne/files/classes/biol78002/gams_data.csv", header=T)

#column data
# lat	latitude
# long	longitude
# sdmp	standard deviation of mean (annual) precipitation
# sdmt	standard deviation of mean annual temperature
# ndvi 	normalized differential vegetation index - a measure of greenness, and thus of productivity
# mph2om 	mean precipitation in the wettest month
# htwm 	highest temperature in the warmest month (e.g., to test for extreme temperatures)
# mpdm 	mean precipitation in the driest month (e.g., to test for drought conditions)
# s3160 	species 3160 is the mourning dove, presence or absence indicated by 1 or 0
# s6190	species 6190 is the cedar waxwing, presence or absence indicated by 1 or 0

head(dove_or_waxwing)
#get rid of odd rows
dove_or_waxwing <- dove_or_waxwing[,names(dove_or_waxwing) %in% c("lat", "long",
                                                                  "sdmp", "sdmt", "ndvi", "mph2om", "htwm", "mpdm", "s3160", "s6190")]
head(dove_or_waxwing)
tail(dove_or_waxwing)

#can we predict dove presence?
#fit with glm, all variables except waxing presence
dove_glm <-glm(s3160~.-s6190, family="binomial", dove_or_waxwing)
summary(dove_glm)
#let's pare this down using AIC
#what else could/should we have done?
#plot data, check for correlation among explanatory variables...
dove_glm_reduced <- step(dove_glm)

#..check model assumptions
#lets see how this does
plot(fitted.values(dove_glm_reduced), dove_or_waxwing$s3160)
#not overly helpful
#calculate AUROC (AUC)
require(ROCR)
dove_glm_reduced_pred<-prediction(fitted.values(dove_glm_reduced), dove_or_waxwing$s3160)
dove_glm_reduced_performance<-performance(dove_glm_reduced_pred,"tpr","fpr")
plot(dove_glm_reduced_performance)
(dove_glm_reduced_AUC <- performance(dove_glm_reduced_pred, "auc"))
#AUC is the y.values
#to call this
str(dove_glm_reduced_AUC) #see whats going on
#compare to
str(dove_or_waxwing)
dove_glm_reduced_AUC@y.values

#compare to a gam

require(mgcv)
dove_gam <- gam(s3160~s(lat) + s(long) + s(sdmp) + s(sdmt) + s(ndvi) + s(mph2om) +
                  s(htwm) + s(mpdm), data = dove_or_waxwing)
summary(dove_gam)
dove_gam_reduced <- update(dove_gam, . ~ . - s(sdmp) - s(ndvi))
summary(dove_gam_reduced)
#validation
dove_gam_reduced_pred<-prediction(fitted.values(dove_gam_reduced), dove_or_waxwing$s3160)
dove_gam_reduced_performance<-performance(dove_gam_reduced_pred,"tpr","fpr")
plot(dove_gam_reduced_performance)
(dove_gam_reduced_AUC <- performance(dove_gam_reduced_pred, "auc"))
#AUC is the y.values
#to call this
str(dove_gam_reduced_AUC) #see whats going on
#compare to
str(dove_or_waxwing)
dove_gam_reduced_AUC@y.values
AIC(dove_gam_reduced, dove_glm_reduced) #gam is better, AIC is lower
#can try other smoothers for gam as well if wanted (lo is loess, but you need
#gam from gam package)

#cross validation
require(boot)
dove_glm_reduced_cv<-cv.glm(dove_or_waxwing,  dove_glm_reduced,  K=3)
str(dove_glm_reduced_cv)
#delta is the prediction error and the adjusted rate - use adjusted to minimize
#impact of sampling or outliers

#for gam
require(gamclass)
dove_gam_reduced_cv <-CVgam(s3160 ~ s(lat) + s(long) + s(sdmt) + s(mph2om) + s(htwm) + s(mpdm), data = dove_or_waxwing, nfold = 3)
str(dove_gam_reduced_cv)
#CV_mse_GAM is your prediction accuracy [not totally sure you can compare this to
#glm score, but can use to compare models]

#extra credit
#develop a coin flip experiment where you use Bayesian analysis
#consider how changing the prior (hint, use a Beta(1,1) for uniform and increase
#to Beta(30,30)) for a prior centered at .5) and hte experiment size (you can use
#dbinom(p,N) again, and let your data be the number of samples and number of heads)
#impacts your results
