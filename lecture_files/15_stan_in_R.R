#metropolis hastings example####
#https://web.as.uky.edu/statistics/users/pbreheny/701/S13/notes/2-28.pdf
p <- function(mu) {
  dt(mu, 5) * prod(dt(y, 5, mu))
}
N <- 10000
mu <- numeric(N)

#if we observe y=c(-1,1,5)
y=c(-1,1,5)
for (i in 1:(N-1)) {
  proposal <- mu[i] + rnorm(1)
  r <- p(proposal)/p(mu[i])
  accept <- rbinom(1, 1, min(1,r))
  mu[i+1] <- if (accept) proposal else mu[i]
}
#easy traceplot
plot(mu)

#if we observe y=c(39,41,45)
y=c(39,41,45)
for (i in 1:(N-1)) {
  proposal <- mu[i] + rnorm(1)
  r <- p(proposal)/p(mu[i])
  accept <- rbinom(1, 1, min(1,r))
  mu[i+1] <- if (accept) proposal else mu[i]
}
#easy traceplot
plot(mu)

#doesn't matter where we start (within reason)
par(mfrow =c(2,2))
for(j in 1:4){
mu <- numeric(N)
mu[1] <- runif(1,5,100)
#if we observe y=c(39,41,45)
y=c(39,41,45)
for (i in 1:(N-1)) {
  proposal <- mu[i] + rnorm(1)
  r <- p(proposal)/p(mu[i])
  accept <- rbinom(1, 1, min(1,r))
  mu[i+1] <- if (accept) proposal else mu[i]
}
#easy traceplot
plot(mu, main = paste("initial estimate = ", mu[1]))
}
remove(p)
#binomial example####
#binomial test####
binom.test(x=14, n=18, p=.5)

#compare intercept in stan_glm
#make dataframe
frog_data <- data.frame(hand = c(rep(1,14), rep(0,4)))
bayesian_frog <-stan_glm(hand ~ 1, frog)

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

#check sampling outcomes####
launch_shinystan(bayesian_iris_anova)

#check model#### 
#check residuals for patterns
require(ggplot2)
resid = resid(bayesian_iris_anova)
fit = fitted(bayesian_iris_anova)
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))

#does model predict observed data?
#http://www.flutterbys.com.au/stats/tut/tut7.4b.html
y_pred = posterior_predict(bayesian_iris_anova)
#just getting all simulated outcomes into a column
require(tidyr)
newdata = iris[,c("Sepal.Length", "Species")] %>% cbind(t(y_pred)) %>% gather(key = "Rep", value = "Sepal.Length",
                                               -"Species":-"Sepal.Length")
require(ggplot2)
ggplot(newdata) + 
   geom_violin(aes(y = Sepal.Length, x = Species, fill = "Model"),
                               alpha = 0.5) + 
  geom_violin(data = iris, aes(y = Sepal.Length, x = Species,fill = "Obs"), alpha = 0.5) + 
  geom_point(data = iris, aes(y = Sepal.Length, x= Species), 
             position = position_jitter(width = 0.1, height = 0),
             color = "black")

#leave one out cross validation
loo_bayesian_iris_anova <- loo(bayesian_iris_anova)
plot(loo_bayesian_iris_anova)


#analyze posterior####
#have to call certainty interval by dummy variable!

summary(bayesian_iris_anova)

ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "Speciesversicolor")
round(ci95, 2)
ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "Speciesvirginica")
round(ci95, 2)
ci95 <- posterior_interval(bayesian_iris_anova, prob = 0.95, pars = "(Intercept)")
round(ci95, 2)

#compare to p-values####
#maybe not a good idea...
#http://www.flutterbys.com.au/stats/downloads/slides/mcmcpvalue.R
mcmcpvalue <- function(samp)
{
  ## elementary version that creates an empirical p-value for the
  ## hypothesis that the columns of samp have mean zero versus a
  ## general multivariate distribution with elliptical contours.
  
  ## differences from the mean standardized by the observed
  ## variance-covariance factor
  
  ## Note, I put in the bit for single terms
  if (length(dim(samp))==0) {
    std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - mean(samp),transpose = TRUE)
    sqdist <- colSums(std * std)
    sum(sqdist[-1] > sqdist[1])/length(samp)
  }
  else {
    std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - colMeans(samp),transpose = TRUE)
    sqdist <- colSums(std * std)
    sum(sqdist[-1] > sqdist[1])/nrow(samp)
  }
  
}

mcmcpvalue(as.matrix(bayesian_iris_anova)[, "Speciesversicolor"])  # effect of (versicolor - setosa (intercept))
mcmcpvalue(as.matrix(bayesian_iris_anova)[, "Speciesvirginica"])  # effect of (virginica - setosa (intercept))
mcmcpvalue(as.matrix(bayesian_iris_anova)[, 2:4]) #effect of all groups




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

