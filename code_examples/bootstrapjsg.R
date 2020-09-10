#bootstrapjsg function####
#just open and source the file to have access to the function
bootstrapjsg <- function(data1, data2=NULL, conf=.95, fun=mean, r=10000, null=0)
{
  library(boot)
  library(simpleboot)
  one.boot <- function (data, FUN, R, student = FALSE, M, weights = NULL, ...) 
  {
    func.name <- ifelse(is.character(FUN), FUN, deparse(substitute(FUN)))
    extra <- list(...)
    if (func.name == "quantile") {
      if (is.na(match("probs", names(extra)))) 
        stop("'probs' argument must be specified")
      if (length(extra$probs) > 1) 
        stop("can only bootstrap a single quantile")
    }
    func <- match.fun(FUN)
    boot.func <- function(x, idx) {
      fval <- func(x[idx], ...)
      if (student) {
        rs.x <- x[idx]
        b <- one.boot(rs.x, FUN, R = M, student = FALSE, 
                      M = NULL, weights = NULL, ...)
        fval <- c(fval, var(b$t))
      }
      fval
    }
    b <- boot(data, statistic = boot.func, R = R, weights = weights)
    b$student <- student
    structure(b, class = "simpleboot")
  }
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
