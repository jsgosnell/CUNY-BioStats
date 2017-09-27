#demonstrate chi-squared - z relationship

#first, look at one draw
means=1:10000
for (i in 1:length(means)){
means[i]=mean(rchisq(n=1, df=1))
}
par(mfrow=c(2,1))
hist(means)
for (i in 1:length(means)){
means[i]=rnorm(1,0,1)^2
}
hist(means)


#now, two draws
means=1:10000
for (i in 1:length(means)){
means[i]=mean(rchisq(n=1, df=2))
}
par(mfrow=c(2,1))
hist(means)
for (i in 1:length(means)){
means[i]=rnorm(1,0,1)^2+rnorm(1,0,1)^2
}
hist(means)


#now, four draws
means=1:10000
for (i in 1:length(means)){
means[i]=mean(rchisq(n=1, df=4))
}
par(mfrow=c(2,1))
hist(means)
for (i in 1:length(means)){
means[i]=rnorm(1,0,1)^2+rnorm(1,0,1)^2+rnorm(1,0,1)^2+rnorm(1,0,1)^2
}
hist(means)

#now, ten draws
means=1:10000
for (i in 1:length(means)){
means[i]=mean(rchisq(n=1, df=10))
}
par(mfrow=c(2,1))
hist(means)
for (i in 1:length(means)){
means[i]=rnorm(1,0,1)^2+rnorm(1,0,1)^2+rnorm(1,0,1)^2+rnorm(1,0,1)^2
+norm(1,0,1)^2+rnorm(1,0,1)^2+rnorm(1,0,1)^2+rnorm(1,0,1)^2
+norm(1,0,1)^2+rnorm(1,0,1)^2
}
hist(means)
