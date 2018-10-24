sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)
##HO: average weight of males training at the facility is no different than average weight of australian male
#HA: average weight of males training at the facility is different than average weight of australian male

#looks spread/normal and sample size is large
plot(sport[sport$Sex == "female" & sport$Sport == "Netball", "RCC"])
hist(sport[sport$Sex == "female" & sport$Sport == "Netball", "RCC"])

##normal based method
t.test(sport[sport$Sex == "female" & sport$Sport == "Netball", "RCC"], mu = 4.8)

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

bootstrapjsg(sport[sport$Sex == "female" & sport$Sport == "Netball", "RCC"], null = 4.8)
