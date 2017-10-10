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

