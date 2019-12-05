#bayesian code####

#probability trees
##below code is unfortunately cumbersome
#from https://daranzolin.github.io/2018-01-07-probability-trees/
#
library(DiagrammeR)

bayes_probability_tree <- function(prior, true_positive, true_negative, label1 = "Prior", 
                                   label2 = "Complimentary Prior", label3 = "True Positive",
                                   label4 = "False Negative", label5 = "False Positive",
                                   label6 = "True Negative") {
  
  if (!all(c(prior, true_positive, true_negative) > 0) && !all(c(prior, true_positive, true_negative) < 1)) {
    stop("probabilities must be greater than 0 and less than 1.",
         call. = FALSE)
  }
  c_prior <- 1 - prior
  c_tp <- 1 - true_positive
  c_tn <- 1 - true_negative
  
  round4 <- purrr::partial(round, digits = 5)
  
  b1 <- round4(prior * true_positive)
  b2 <- round4(prior * c_tp)
  b3 <- round4(c_prior * c_tn)
  b4 <- round4(c_prior * true_negative)
  
  bp <-  round4(b1/(b1 + b3))
  
  labs <- c("X", prior, c_prior, true_positive, c_tp, true_negative, c_tn, b1, b2, b4, b3)
  
  tree <-
    create_graph() %>%
    add_n_nodes(
      n = 11,
      type = "path",
      label = labs,
      node_aes = node_aes(
        shape = "circle",
        height = 1,
        width = 1,
        x = c(0, 3, 3, 6, 6, 6, 6, 8, 8, 8, 8),
        y = c(0, 2, -2, 3, 1, -3, -1, 3, 1, -3, -1))) %>% 
    add_edge(
      from = 1,
      to = 2,
      edge_aes = edge_aes(
        label = label1
      )
    ) %>% 
    add_edge(
      from = 1, 
      to = 3,
      edge_aes = edge_aes(
        label = label2
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 4,
      edge_aes = edge_aes(
        label = label3
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 5,
      edge_aes = edge_aes(
        label = label4
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 7,
      edge_aes = edge_aes(
        label = label5
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 6,
      edge_aes = edge_aes(
        label = label6
      )
    ) %>% 
    add_edge(
      from = 4,
      to = 8,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 5,
      to = 9,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 7,
      to = 11,
      edge_aes = edge_aes(
        label = "="
      )
    ) %>% 
    add_edge(
      from = 6,
      to = 10,
      edge_aes = edge_aes(
        label = "="
      )
    ) 
  message(glue::glue("The probability of having {label1} after testing {label3} is {bp}"))
  print(render_graph(tree))
  invisible(tree)
}


# example####
bayes_probability_tree(prior = 0.0001, true_positive = 0.9, true_negative = 0.999, label1 = "cancer", 
                       label2 = "not cancer",
                       label3 = "positive", 
                       label4 = "negative",
                       label5 = "positive", 
                       label6 = "negative")

#add nodes
.00009 + .00001 + .001 + .9989

(.00009)/(.001+ .00009)

#bayesian frog analysis####
#frog analysis
#binomial test####
binom.test(x=14, n=18, p=.5)

require(LearnBayes)
#for proportions, we often use beta
#beta curve peaked at .5####
ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = 20, shape2 = 20),
                aes(color = "20, 20"),
                size = 3,geom = "line") + 
  stat_function(fun = dbeta, n = 101, args = list(shape1 = 2, shape2 = 2),
                aes(color = "2, 2"),
                size = 3,geom = "line") + 
  stat_function(fun = dbeta, n = 101, args = list(shape1 = 5, shape2 = 20),
                aes(color = "5, 20"),
                size = 3,geom = "line") +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = 1, shape2 = 1), 
                aes(color = "1, 1"),
                size = 3,geom = "line") + 
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  labs(colour = "Parameters (a, b)")

#triplot####
#peaked at .5 (more certain)
triplot(prior = c(20,20), data = c(14,4), where = "topleft")
#less peaked at .5
triplot(prior = c(2,2), data = c(14,4), where = "topleft")

#get a credible interval####
#for posterior we can simply add shape 1 + success, shape2 + failures
qbeta(c(0.025, 0.975),shape1 =  20+14, shape2 = 20+4)
qbeta(c(0.025, 0.975),shape1 =  2+14, shape2 = 2+4)

#bayesian one-sample t-test####
#bayes factor using athlete data
#remember the normal t-test
t.test(sport[sport$Sex == "male", "Ht"], mu = 175.6)
library(BayesFactor)
#uses JZS, which relies on cauchy
ggplot(data = data.frame(x = c(-10, 10)), aes(x)) +
  stat_function(fun = dcauchy, n = 101, args = list(location = 0, scale = 1),
                aes(color = "0, 1"),
                size = 3,geom = "line") + 
  stat_function(fun = dcauchy, n = 101, args = list(location = 0, scale = 2),
                aes(color = "0,2"),
                size = 3,geom = "line") + 
  stat_function(fun = dcauchy, n = 101, args = list(location = 2, scale = 1),
                aes(color = "2,1"),
                size = 3,geom = "line") +
  ylab("Probablity") +
  xlab("")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  labs(colour = "Parameters (location, scale)")

sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)

bf = ttestBF(x = sport[sport$Sex == "male", "Ht"], mu = 175.6)
bf #this is bf10 (alternative over null).can reverse with
#1/bf10 #to get bf01, but hard to see difference here!

#what if we support null more?
bf = ttestBF(x = sport[sport$Sex == "male", "Ht"], mu = 175.6, 
             rscale  = "ultrawide")
bf

#or sample posterior
chains <- posterior(bf, iterations = 1000)
summary(chains)

#can recomputer more chains
chains2 <- recompute(chains, iterations = 10000)
plot(chains2[,1:2])

#BEST method
#://www.sumsar.net/best_online/
#
##two sample####
require(BayesFactor)
t.test(Ferr ~ Sport, sport[sport$Sport %in% c("BBall", "Row"),])
ttestBF(formula = Ferr ~ Sport, data = sport[sport$Sport %in% c("BBall", "Row"),])

#get data for online BF test  http://pcl.missouri.edu/bf-two-sample####
t.test(Ferr ~ Sport, sport[sport$Sport %in% c("BBall", "Row"),])
summary(sport[sport$Sport %in% c("BBall", "Row"),])

#get data for online BEST test http://www.sumsar.net/best_online/ ####
sport[sport$Sport == "BBall", "Ferr"]
sport[sport$Sport == "Row", "Ferr"]

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
library(rstanarm)
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

#leave one out cross validation #not in lecture
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


#or write as lmer
#with defaults
bayesian_iris_anova_lmer <- stan_lmer(Sepal.Length ~ 1 + (1|Species),
                                      data = iris, adapt_delta = 0.99, iter = 4000)

#with other priors
#increased adapt_delta and iter due to error
bayesian_iris_anova_lmer_prior_choices <- stan_lmer(Sepal.Length ~ 1 + (1|Species),
                                                    data = iris, prior_intercept = cauchy(),
                                                    prior_covariance = decov(shape = 2, scale = 2), 
                                                    adapt_delta = 0.99, iter = 4000)

#can check and validate
bayesian_iris_anova_lmer
launch_shinystan(bayesian_iris_anova_lmer)

#compare models using loo
#leave one out cross validation #not in lecture
loo_bayesian_iris_anova <- loo(bayesian_iris_anova)
print(loo_bayesian_iris_anova)
loo_bayesian_iris_anova_lmer <- loo(bayesian_iris_anova_lmer)
print(loo_bayesian_iris_anova_lmer)
compare_models(loo_bayesian_iris_anova,loo_bayesian_iris_anova_lmer)

#or using an information criterion
waic_bayesian_iris_anova <- waic(bayesian_iris_anova)
print(waic_bayesian_iris_anova)
waic_bayesian_iris_anova_lmer <- waic(bayesian_iris_anova_lmer)
print(waic_bayesian_iris_anova_lmer)
compare_models(waic_bayesian_iris_anova,waic_bayesian_iris_anova_lmer)




#regression example####
cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholest_regression <- lm(cholest ~ day, na.omit(cholesterol))
par(mfrow = c(2,2))
plot(cholest_regression)
require(car)
Anova(cholest_regression, type = "III")
summary(cholest_regression)

#with rstanarm#

#for uniform prior (bad idea) use NULL
bayesian_cholest_regression <- stan_lm(cholest ~ day, na.omit(cholesterol),
                                       prior = NULL)

#otherwise can specify as predicted R2 again
bayesian_cholest_regression <- stan_lm(cholest ~ day, na.omit(cholesterol),
                                       prior = R2(what = "median", location = 0.5))
#analyze draws####
launch_shinystan(bayesian_cholest_regression)

#model validation####
#check model#### 
#check residuals for patterns
resid = resid(bayesian_cholest_regression)
fit = fitted(bayesian_cholest_regression)
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))

#analyze posterior####
#have to call certainty interval by dummy variable!

summary(bayesian_iris_anova)



#analyze posterior####
summary(bayesian_cholest_regression)

#binomial example####
#binomial test####
binom.test(x=14, n=18, p=.5)

#compare intercept in stan_glm
#make dataframe
frog_data <- data.frame(hand = c(rep(1,14), rep(0,4)))
bayesian_frog <-stan_glm(hand ~ 1, frog_data, family = "binomial")

bayesian_frog
#note intercept is equal to 1.3. we would expect 0 under normal binomial link.
#why?
#link is
#log(p/1-p) = (linear outcome)
log(.5/(1-.5)) # equal 0
#so if we get a value for the intercept, we can say it equals to
#exp(predicted intercept)/(1-exp(predicted intercept))
exp(1.3)/(1 + (exp(1.3))) #close to binomial test prediction (probably no reason 
#to do this)

#check sampling outcomes####
launch_shinystan(bayesian_frog)

#analyze posterior####
summary(bayesian_frog)





