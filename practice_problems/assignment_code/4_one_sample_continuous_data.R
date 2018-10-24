#3#####
#a-e below
data <- c(183.2, 149.0, 154.0, 167.2, 187.2, 158.0, 143.0)
require(BSDA)
#a
z.test(x=data,mu = 161, sigma.x=sqrt(275))
#b
t.test(x=data,mu = 161)
#c
wilcox.test(data, mu=161)
#d
SIGN.test(data, md=161)
#e
require(BayesFactor)
ttestBF(data, mu = 161) #anecdotal support for H0
#compare to 
ttest.tstat(.32656,7, simple = T)

#4####
#a-e below
data <- c(239, 176, 235, 217, 234, 216, 318, 190, 181, 225)
#a
z.test(x=data,mu = 205, sigma.x=sqrt(1500))
#b
t.test(x=data,mu = 205)
#c
wilcox.test(data, mu=205)
#d
SIGN.test(a, md=205)
#e
ttestBF(data, mu = 205) #anecdotal support for H0


#5####
#a
binom.test(25,30, p=.5)
#b
binom.test(25,30, .8)

#6####
data <- c(0.9, 1.4, 1.2, 1.2, 1.3, 2.0, 1.4, 1.6)
t.test(data, mu=1.4)

#7
butterfly <-read.csv("http://csivc.csi.cuny.edu/Lisa.Manne/files/classes/biol78002/Parmesan_by_family.csv")
head(butterfly)
dim(butterfly)
butterfly$percert_north <-butterfly$N.boundary.Extended.northwards/butterfly$Num.species.considered.here
t.test(butterfly$percert_north ,mu=.5)
#could also include southern range moving north
#compare to binomial
sum(butterfly$N.boundary.Extended.northwards)
sum(butterfly$Num.species.considered.here)
binom.test(34,92, .5)
#but this is pseudoreplication in some ways
#
#8####
#
sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)
require(simpleboot)
bootstrapjsg=function(data1, data2=NULL, conf=.95, fun=mean, r=10000, null=0)
{
  if (is.null(data2)){
    a=one.boot(na.omit(data1), fun, r)
    confint=boot.ci(a, conf)
    p=length(subset(a$t, abs(a$t-mean(a$t))>=abs(null-mean(a$t))))/r
    output=c(conf, "% Confidence Interval", confint$percent[1,4:5], "p-value", p)
    return(output)}
  if (is.null(data2)==F){
    a=two.boot(na.omit(data1), na.omit(data2), fun, r)
    confint=boot.ci(a, conf)
    p=length(subset(a$t, abs(a$t-mean(a$t))>=abs(null-mean(a$t))))/r
    output=c(conf, "% Percentile Confidence Interval", confint$percent[1,4:5], "p-value", p)
    return(output)}
}

bootstrapjsg(data1=sport[sport$Sex == "male", "Wt"], null=85.9)
summary(sport[sport$Sex == "male",])
hist(sport[sport$Sex == "male", "Ht"])

#9
ttestBF(sport[sport$Sex == "male", "Wt"], mu=85.9)
ttestBF(sport[sport$Sex == "male", "Wt"], mu=85.9, rscale rscale = "ultrawide")

