#binomial example####
#binomial test####
binom.test(x=14, n=18, p=.5)

#ANOVA example####
#build model####
iris_anova <- lm(Sepal.Length~Species, iris)
#it creates an object you can manipulate

#check assumptions####
par(mfrow = c(2,2))
plot(iris_anova)

#look at outputs using p-values
summary(iris_anova)

#Anova command from car package is needed to give overall group p-value
require(car)
Anova(iris_anova, type = "III")


#you can get specific group means by builidng model without intercept
iris_anova_no_intercept <- lm(Sepal.Length~Species - 1, iris)
summary(iris_anova_no_intercept)

#since our overall anova was significant, we need to carry out multiple comparisons
#to see what groups are driving the difference
#multcomp
require(multcomp)

#where's the almost difference? use Tukey's HSD for all pairs
require(multcomp)
compare_cont_tukey <- glht(iris_anova, linfct = mcp(Species = "Tukey"))
summary(compare_cont_tukey)

#with stan
require(rstanarm)

bayesian_iris_anova <- stan_aov(Sepal.Length~Species, data = iris, 
                  prior = R2(what = "median", location = 0.5), adapt_delta = 0.9999)
                  
bayesian_iris_anova

#have to call certainty interval by dummy variable!
ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "Speciesversicolor")
round(ci95, 2)
ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "Speciesvirginica")
round(ci95, 2)
ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "(Intercept)")
round(ci95, 2)

#check sampling outcomes####
launch_shinystan(bayesian_iris_anova)

#leave one out cross validation
loo_bayesian_iris_anova <- loo(bayesian_iris_anova)
plot(loo_bayesian_iris_anova)

#check model#### 
#check residuals for patterns
resid = resid(bayesian_iris_anova)
fit = fitted(bayesian_iris_anova)
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))

#does model predict observed data?
#http://www.flutterbys.com.au/stats/tut/tut7.4b.html

y_pred = posterior_predict(bayesian_iris_anova)
require(magrittr)
newdata = data %>% cbind(t(y_pred)) %>% gather(key = "Rep", value = "Value",
                                               -y:-x)
require(ggplot2)
ggplot(newdata) + geom_violin(aes(y = Value, x = x, fill = "Model"),
                              alpha = 0.5) + 
  geom_violin(data = data, aes(y = y, x = x,fill = "Obs"), alpha = 0.5) + 
  geom_point(data = data, aes(y = y,= x), 
             position = position_jitter(width = 0.1, height = 0),
             color = "black")





bayesian_iris_anova <- stan_lmer(Sepal.Length ~ 1 + (1|Species),
                                 data = iris, prior_intercept = cauchy(),
                                 prior_covariance = decov(shape = 2, scale = 2))

#increase adapt_delta and iter due to error
bayesian_iris_anova <- stan_lmer(Sepal.Length ~ 1 + (1|Species),
                                 data = iris, prior_intercept = cauchy(),
                                 prior_covariance = decov(shape = 2, scale = 2), 
                                 adapt_delta = 0.99, iter = 4000)
bayesian_iris_anova
launch_shinystan(bayesian_iris_anova)

#regression example####
cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholest_regression <- lm(cholest ~ day, na.omit(cholesterol))
par(mfrow = c(2,2))
plot(cholest_regression)
require(car)
Anova(cholest_regression, type = "III")

