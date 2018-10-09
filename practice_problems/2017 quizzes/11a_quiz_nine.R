#regression quiz
#
nuclear <- read.table("http://www.statsci.org/data/general/hanford.txt",
                      header = T)
head(nuclear)
fit <- lm(Mortality ~ Exposure, nuclear)
plot(fit)
require(car)
Anova(fit, type = "III")