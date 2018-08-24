sport <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/sport.csv?attredirects=0&d=1")
##HO: average weight of males training at the facility is no different than average weight of australian male
#HA: average weight of males training at the facility is different than average weight of australian male
##normal based method
t.test(sport[sport$Sex == "female", "Wt"], mu = 71.1)

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

bootstrapjsg(sport[sport$Sex == "female", "Wt"], null = 71.1)