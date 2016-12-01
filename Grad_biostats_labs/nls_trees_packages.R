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
