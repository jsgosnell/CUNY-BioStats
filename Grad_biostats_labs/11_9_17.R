#11/9/17 class script
#example of anova, regression, multiple regression/anova as lm, with extension to 
#TEAM DATA
team <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/team_data.csv?attredirects=0&d=1")
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
summary(team_model_1)

#analyze multiple factors using ANOVA to get traditional p-value for category
require(car)
Anova(team_model_1, type = "III")

#where's the almost difference? use Tukey's HSD for all pairs
require(multcomp)
compare_cont_tukey <- glht(team_model_1, linfct = mcp(Continent = "Tukey"))
summary(compare_cont_tukey)

#compare to another common method for post hoc comparisons
team_model_aov <- aov(PlotCarbon.tonnes ~ Continent, team)
TukeyHSD(team_model_aov)

#or we can specify any we wanted a priori
#only interested in differences from Latin America? just test those
compare_latin_america_only <- glht(team_model_1, linfct = mcp(Continent = 
                                                         c("Asia - Latin America = 0", 
                                              "Latin America - Africa = 0")))
#error because "Latin America" has space, and multcomp doesn't like that
#can fix here or redo in excel, etc
require(plyr)
team$Continnent = revalue(team$Continent, c(Latin America = "Latin_America" ))
#neither does plyr...do it directly
levels(team$Continent)[levels(team$Continent) == "Latin America"] <- "Latin_America"
#fixed, but big lesson is avoid spaces

#now, rerun model since you changed the data
team_model_1 <-lm(PlotCarbon.tonnes ~ Continent, team)
#check assumptions
plot(team_model_1)
compare_latin_america_only <- glht(team_model_1, linfct = mcp(Continent = 
                                                         c("Asia - Latin_America = 0", 
                                                           "Latin_America - Africa = 0")))
#remember to control for error rates!
summary(compare_latin_america_only, test=adjusted("holm")) #africa is not driving the difference
#other options
summary(compare_latin_america_only, test=adjusted("fdr")) #africa is not driving the difference
#we can "find" significance more easily, but you need to justify why you did this

#model comparison####
#does rainfall add anything
#use update command if you want, . means everything on that side in initial model
#this is an ANCOVA (categorical + numerical predictor)
team_model_2 <- update(team_model_1, . ~ . + Precip_mean.mm)
summary(team_model_2) #good for r2 value
plot(team_model_2)
#check each parameter for significance
Anova(team_model_2, type = "III")
#or compare models
anova(team_model_2, team_model_1) # does not appear to be different

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

pairs(team_potential)
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

#now to do top-down test we have to use Chi-squared tests (not F)
Anova(team_model_full_mm, type = "III")
stepAIC(team_model_full_mm) # won't work with mixed models, so have to do manually
drop1(team_model_full_mm)
drop1(team_model_full_mm, test = "Chi")

#dredge will work, but may be slow
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

#generalized linear models
#what if the outcome isn't continuous but is either 0/1 (presence/absence) or 
#a proportion?
#We use a generalized linear model, aka logistic regression
#use glm command (instead or arcsin transform!)
#data from Needles et al 2014
otters <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/needles_january.csv?attredirects=0&d=1")
head(otters)
#star, otters, and mussels are treatments (1 is present)
#WASU and notWASU are count data
otter_fit <- glm(cbind(WASU, notWASU)~ Star + Otters + Mussels, otters, family=binomial)
summary(otter_fit)
#same basic assumption
plot(otter_fit)
#can use drop1, stepAIC, etc

#glmm
#but this is a mixed-model again! multiple measures per piling
otter_fit_mm <- glmer(cbind(WASU, notWASU)~ Star + Otters + Mussels + (1|Piling), otters, family=binomial)
summary(otter_fit_mm)
Anova(otter_fit_mm, type = "III") #uses chisq test
drop1(otter_fit_mm, test = "Chi") 
otter_fit_mm_a <- update(otter_fit_mm, .~. - Star)
Anova(otter_fit_mm_a, type = "III") #uses chisq test
otter_fit_mm_b <- update(otter_fit_mm_a, .~. - Mussels)
Anova(otter_fit_mm_b, type = "III") #uses chisq test

#nls is used to fit specified functions in R
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

#generalized additive model (gam)
#non-linear model
TN <- read.table("http://sites.google.com/site/stephengosnell/teaching-resources/datasets/TeethNitrogen.txt",
                 header=T)
Moby <- subset(TN, TN$Tooth == "Moby")
Moby_lm <- lm(X15N ~ Age, data = Moby)
op <- par(mfrow = c(2, 2))
plot(Moby_lm, add.smooth = FALSE)
par(op)
#what issue do you see?
require(mgcv)
require(MASS)
Moby_gam <- gam(X15N ~ s(Age),data=Moby)
summary(Moby_gam)
plot(Moby_gam)
#can compare fits with AIC
AIC(Moby_gam, Moby_lm)

#trees are useful way of handling data visually and allow first look
#building the classification tree
#install if necessary
#example with famous iris dataset (built-in)
#good for species classification!
library(rpart)
iris_tree_initial <- rpart(Species ~ ., data = iris, method = "class", 
                           minsplit = 2, minbucket = 1)
plot(iris_tree_initial)
text(iris_tree_initial)
#or for a prettier graph
require(rattle)
fancyRpartPlot(iris_tree_initial, main="Iris")

#what if you want fewer splits (less complex model)
#can use defaults for buckets 
iris_tree_initial_auto <- rpart(Species ~ ., data = iris)
fancyRpartPlot(iris_tree_initial_auto, main="Iris")

#or minimize complexity parameter (good for larger models)
iris_tree_model_2<- prune(iris_tree_initial, 
                          cp =   iris_tree_initial$cptable[which.min(iris_tree_initial$cptable[,"xerror"]),"CP"])
#is using this to make decisions
iris_tree_initial$cptable
fancyRpartPlot(iris_tree_model_2)

# #validation techniques
# #UNDER CONSTRUCTION
# #need 0/1 column for for prediction
# iris$setosa <- iris$Species
# levels(iris$setosa)[levels(iris$setosa) == "setosa"]  <- "1"
# levels(iris$setosa)[levels(iris$setosa) %in% c("versicolor", "virginica")] <- "0"
# iris$setosa <- as.numeric(as.character(iris$setosa))
# 

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

# #cross validation
# UNDER CONSTRUCTION
# require(boot)
# dove_glm_reduced_cv<-cv.glm(dove_or_waxwing,  dove_glm_reduced,  K=3)
# str(dove_glm_reduced_cv)
# #delta is the prediction error and the adjusted rate - use adjusted to minimize
# #impact of sampling or outliers
# 
# #for gam
# require(gamclass)
# dove_gam_reduced_cv <-CVgam(s3160 ~ s(lat) + s(long) + s(sdmt) + s(mph2om) + s(htwm) + s(mpdm), data = dove_or_waxwing, nfold = 3)
# str(dove_gam_reduced_cv)
# #CV_mse_GAM is your prediction accuracy [not totally sure you can compare this to
# #glm score, but can use to compare models]
# 
# 







