#ANOVA example####
#with stan for all species
library(rstanarm)
bayesian_iris_anova <- stan_aov(Sepal.Width~Species, data = iris, 
                                prior = R2(what = "median", location = 0.5), adapt_delta = 0.9999)

#check sampling outcomes####
launch_shinystan(bayesian_iris_anova)

#check model#### 
#check residuals for patterns
require(ggplot2)
resid = resid(bayesian_iris_anova)
fit = fitted(bayesian_iris_anova)
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))

#can also look at  posterior predictive checks in shinystan

#does model predict observed data?
#http://www.flutterbys.com.au/stats/tut/tut7.4b.html
y_pred = posterior_predict(bayesian_iris_anova)
#just getting all simulated outcomes into a column
require(tidyr)
newdata = iris[,c("Sepal.Width", "Species")] %>% cbind(t(y_pred)) %>% gather(key = "Rep", value = "Sepal.Width",
                                                                              -"Species":-"Sepal.Width")
require(ggplot2)
ggplot(newdata) + 
  geom_violin(aes(y = Sepal.Width, x = Species, fill = "Model"),
              alpha = 0.5) + 
  geom_violin(data = iris, aes(y = Sepal.Width, x = Species,fill = "Obs"), alpha = 0.5) + 
  geom_point(data = iris, aes(y = Sepal.Width, x= Species), 
             position = position_jitter(width = 0.1, height = 0),
             color = "black")


#analyze posterior####
#have to call certainty interval by dummy variable!
summary(bayesian_iris_anova)

ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "Speciesversicolor")
round(ci95, 2)
ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "Speciesvirginica")
round(ci95, 2)
ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "(Intercept)")
round(ci95, 2)

#make model with virginican and versicolor as same species####

iris$combined.species <- iris$Species
levels(iris$combined.species) <- c("setosa", "combined", "combined")
bayesian_iris_anova_combined <- stan_aov(Sepal.Width~combined.species, data = iris, 
                                prior = R2(what = "median", location = 0.5), adapt_delta = 0.9999)

#check sampling outcomes####
launch_shinystan(bayesian_iris_anova_combined)

#check model#### 
#check residuals for patterns
resid = resid(bayesian_iris_anova_combined)
fit = fitted(bayesian_iris_anova_combined)
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))

#can also look at  posterior predictive checks in shinystan

#does model predict observed data?
#http://www.flutterbys.com.au/stats/tut/tut7.4b.html
y_pred = posterior_predict(bayesian_iris_anova_combined)
#just getting all simulated outcomes into a column
newdata = iris[,c("Sepal.Width", "combined.species")] %>% cbind(t(y_pred)) %>% gather(key = "Rep", value = "Sepal.Width",
                                                                             -"combined.species":-"Sepal.Width")
ggplot(newdata) + 
  geom_violin(aes(y = Sepal.Width, x = combined.species, fill = "Model"),
              alpha = 0.5) + 
  geom_violin(data = iris, aes(y = Sepal.Width, x = combined.species,fill = "Obs"), alpha = 0.5) + 
  geom_point(data = iris, aes(y = Sepal.Width, x= combined.species), 
             position = position_jitter(width = 0.1, height = 0),
             color = "black")


#analyze posterior####
#have to call certainty interval by dummy variable!
summary(bayesian_iris_anova_combined)

ci95 <- posterior_interval(bayesian_iris_anova_combined, prob = 0.95, pars = "combined.speciescombined")
round(ci95, 2)

ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "(Intercept)")
round(ci95, 2)


#compare models using loo
#leave one out cross validation #not in lecture
loo_bayesian_iris_anova <- loo(bayesian_iris_anova)
print(loo_bayesian_iris_anova)
loo_bayesian_iris_anova_combined <- loo(bayesian_iris_anova_combined)
print(loo_bayesian_iris_anova_combined)
compare_models(loo_bayesian_iris_anova,loo_bayesian_iris_anova_combined)

#or using an information criterion
waic_bayesian_iris_anova <- waic(bayesian_iris_anova)
print(waic_bayesian_iris_anova)
waic_bayesian_iris_anova_combined <- waic(bayesian_iris_anova_combined)
print(waic_bayesian_iris_anova_combined)
compare_models(waic_bayesian_iris_anova,waic_bayesian_iris_anova_combined) #model wtih 3 species is better!

