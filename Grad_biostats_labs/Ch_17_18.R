#linear models to unite anovas and regression
#correlation too!and tools to make them work
#lmer to handle mixed effects
#glm to handle non-numerica
#glmer to handle these together
#plus a refresher on ggplot (focused on finally getting error bars)

#lm is the base function for running linear models (can use instead of aov, for regression,
#etc)

#using lisa's elephant dataset from last week
elephant<-read.csv("http://csivc.csi.cuny.edu/Lisa.Manne/files/classes/biol78002/elephant.csv", header=T)
head(elephant)

#lets start with regression
#does age impact hearth girth

fit_heart_age <- lm(girth_heart_cm ~ age, elephant)
summary(fit_heart_age)
#what does this tell us?
#did we meet assumptions?
par(mfrow=c(2,2))
plot(fit_heart_age)
#normal, but some evidence of structure in residuals
#may need to transfor or add other factor

#what these are (from Lisa Manne)
# The Residuals vs. Fitted values plot should show no structure; it should not show a trend of residuals against fitted values. The variance should not increase or decrease as you move along the x-axis.  The residuals should be centered around 0.
# The Quantile-quantile plot tests whether your residuals are normally distributed.  It plots each data point vs. its position in a theoretical normal distribution.  If the residuals are normally distributed, the plot will look like a straight line.
# The scale-location plot is another plot of residuals vs. fitted values:  it shows the square root of the standardized residuals against the fitted values.  This plot may more clearly show if there is an issue with the variance increasing with the mean (in that case the scatter would increase as the fitted values increased).
# The residuals vs. leverage plot also includes Cook's distance (pale blue dashed line).  Cook's Distance compares the fitted response of the regression which uses every data point, against the fitted response of the regression where a particular data point has been dropped from the analysis (and then sums this difference across all data points). Very influential data points (on the parameter estimates) are identified, and are labeled in this plot.  If there are heavily influential data points, you might consider re-doing the regression model after removing them.

#ancova
fit_heart_age_sex <- lm(girth_heart_cm ~ age * sex, elephant)
summary(fit_heart_age_sex)
plot(fit_heart_age_sex)
#helps a little

#aside
#proving why we need adjsuted r2
extra_variable <- rnorm(dim(elephant)[1])
fit_heart_age_sex_extra <- lm(girth_heart_cm ~ age * sex + extra_variable, elephant)
summary(fit_heart_age_sex_extra)
#focus on r2
summary(fit_heart_age_sex_extra)$r.squared
#compare r2 to first model
summary(fit_heart_age_sex)$r.squared
#now compare adjusted
summary(fit_heart_age_sex_extra)$adj.r.squared
#compare r2 to first model
summary(fit_heart_age_sex)$adj.r.squared
#closer, effect may be positive but won't show net better fit


#another view of interactions
par(mfrow=c(1,1))
interaction.plot(elephant$age, elephant$sex, elephant$girth_heart_cm)

#this looks odd
#lets get some data summaries
#reshape package is great tool for this

require(reshape)
cast(elephant, sex~., value="age", mean)
#males are much younger, probalby shouldn't be included in same model
cast(elephant, sex~., value="age", length)
#and there almost none of them

elephants_female <- elephant[elephant$sex != "M", ]

#now lets look at how multiple factors impact female elephants
fit_heart_age_population_females <- lm(girth_heart_cm ~ age * pop, elephants_female)
summary(fit_heart_age_population_females)
#no interactin, so drop it
fit_heart_age_population_females_no_int <- lm(girth_heart_cm ~ age + pop, elephants_female)
summary(fit_heart_age_population_females_no_int)
#populatoin also appears to not matter
#what does this look like
cast(elephants_female, pop~., value="girth_heart_cm", mean)


#we find another elephant herd
elephants_all <- read.csv("http://sites.google.com/site/stephengosnell/teaching-resources/datasets/elephant.csv")
#grab the females
elephants_female_all <- elephants_all[elephants_all$sex != "M", ]
fit_heart_age_population_female_all <- lm(girth_heart_cm ~ age * pop, elephants_female_all)
summary(fit_heart_age_population_female_all)

#woah, now what do we do? thse doent' make sense with 3+levels for a factor
require(car)
Anova(fit_heart_age_population_female_all, type = "III")
#now we can see some results
#still no interactioin

fit_heart_age_population_female_all_no_int <- lm(girth_heart_cm ~ age + pop, elephants_female_all)
summary(fit_heart_age_population_female_all_no_int)
#same issue
Anova(fit_heart_age_population_female_all_no_int, type = "III")

#major difference, but where
#multcomp is another way to handle multiple comparisons
require(multcomp)
elephant_comparison <- glht(fit_heart_age_population_female_all_no_int, linfct = mcp(pop = "Tukey"))
summary(elephant_comparison)

#what if we had only cared about differences between C and the other populations
elephant_comparison_select <- glht(fit_heart_age_population_female_all_no_int,
                                   linfct = mcp(pop = c("A - C = 0","B - C = 0")))
summary(elephant_comparison_select)
#uh, oh, single-step method has no corrections
summary(elephant_comparison_select, test=adjusted("holm"))
#or maximize the risk
summary(elephant_comparison_select, test=adjusted("fdr"))
#lots of other options exist


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
