### multiple comparison procedures
#modified from ?glht
require(multcomp)
require(reshape)

### set up a one-way ANOVA
amod <- aov(breaks ~ tension, data = warpbreaks)

### set up all-pair comparisons for factor `tension'
### using a symbolic description (`type' argument
### to `contrMat()')
summary(glht(amod, linfct = mcp(tension = "Tukey")))

### alternatively, describe differences symbolically
summary(glht(amod, linfct = mcp(tension = c("M - L = 0",
                                    "H - L = 0",
                                    "H - M = 0"))))

### alternatively, define contrast matrix directly
contr <- rbind("M - L" = c(-1, 1, 0),
               "H - L" = c(-1, 0, 1),
               "H - M" = c(0, -1, 1))
summary(glht(amod, linfct = mcp(tension = contr)))

#need to understand how matrix/effect size works for group comparison

#get true means here
cast(warpbreaks, tension ~ ., value = "breaks", mean)

#use automated procedure
summary(glht(amod, linfct = mcp(tension = c("L - M = 0",
                                    "L - H - M = 0",
                                    "L - H = 0"))))

#estimate appears to be making the contrast integers even but leads to estimates
#larger than actual group differences

#so it does this
contr <- rbind("L - M" = c(1, -1, 0),
               "L - H -M" = c(2, -1, -1),
               "L - H" = c(1, 0, -1))
summary(glht(amod, linfct = mcp(tension = contr)))

#instead of this
contr <- rbind("L - M" = c(1, -1, 0),
               "L - H -M" = c(1, -.5, -.5),
               "L - H" = c(1, 0, -1))
summary(glht(amod, linfct = mcp(tension = contr)))

#all p-values are slightly differnet but only at some far off decimal place
#but estimates are wrong and should be taken from other sources




