#voter preference####
#HO: proportion of democratic voters is equal to .46
#HA: proportion of democratic voters is not equal to .46
binom.test(11,20, .46)
#pvalue is .5032
#given this data, we fail to reject HO.  there is not enough data to suggest the 
#proportion of democratic voters differs from 46%
require(binom)
binom.confint(11,20,.46)
#the 95% confidence interval for voter preferences is [.482, .616]
#note in this instance estimation methods lead to different conclusions than
#hypothesis testing
#
#trilling and distance####
##HO: proportion of chipmunks trilling does not depend on distance
#HA: proportion of chipmunks trilling does depend on distance
chisq.test(matrix(c(16,8,3,18), nrow = 2, byrow = T))
chisq.test(matrix(c(16,8,3,18), nrow=2, byrow = T))$expected
fisher.test(matrix(c(16,8,3,18), nrow=2, byrow = T))
#pvalue is .004541
##given this data, we reject HO.  there is enough data to suggest the 
#proportion of chipmunks trilling does depend on distance.
#
#Australian male athlete weight####
sport <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/sport.csv?attredirects=0&d=1")
##HO: average weight of males training at the facility is no different than average weight of australian male
#HA: average weight of males training at the facility is different than average weight of australian male
##normal based method
t.test(sport[sport$Sex == "male", "Wt"], mu = 85.9)
#p value is .007089
#given this data, we reject HO.there is enough data to suggest that 
#average weight of males training at the facility is different than average weight of australian male
##bootstrap method (no assumptions)
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
#p value is .0054
#given this data, we reject HO.there is enough data to suggest that 
#average weight of males training at the facility is different than average weight of australian male
