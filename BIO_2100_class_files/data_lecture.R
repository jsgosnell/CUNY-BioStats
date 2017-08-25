#from first class
iris

#note hist is odd with cex
label_size <- 2
title_size <- 2.5

par(mar = c(7,7,7,7))

hist(iris$Sepal.Length, main = "Sepal Lengths", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")

#add density overlay
hist(iris$Sepal.Length, main = "Sepal Lengths", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue", prob = T)
lines(density(iris$Sepal.Length), col = "red")             # add a density estimate with defaults

#specifics
head(USArrests)
names(USArrests)
USArrests$State <- as.factor(rownames(USArrests))
par(las=2)
barplot(USArrests$Murder, names.arg = USArrests$State, cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue", main = "Murder Arrests per 100,000 since 1973")
write.csv(USArrests, "USArrests.csv", row.names = F)

write.csv(iris, "iris.csv", row.names = F)

#skewed right
birds <- rbeta(10000,2,12)
hist(birds, main="Weight of Westchester Blue Jays", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "blue", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#skewed left
birds <- rbeta(10000,70,5)
hist(birds, main="Weight of Westchester cardinals", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "red", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)
 
#bimodal data
putnam <- c(rnorm(100,20,4),rnorm(100,40,4))
hist(putnam, main="Weight of Westchester woodpeckers", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "orange", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#normal data
putnam <- c(rnorm(1000,20,4))
hist(putnam, main="Weight of Westchester parrots", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "green", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)


#uniform data
putnam <- c(runif(1000,.1,8))
hist(putnam, main="Weight of Westchester Robins", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "Pink", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)

#normal data
putnam <- c(rnorm(1000,20,4))
hist(putnam, main="Weight of Westchester parrots", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "green", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size, probability = T)
lines(density(putnam), col = "black")   # add a density estimate with defaults
curve(dnorm(x, mean=20, sd=4), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#bimodal data
putnam <- c(rnorm(100,20,4),rnorm(100,40,4))
hist(putnam, main="Weight of Westchester woodpeckers", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "orange", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size, probability = T)
lines(density(putnam), col = "black", lwd = 4)   # add a density estimate with defaults
abline(v=mean(putnam), col="red", lwd = 4)
abline(v=median(putnam), col="green", lwd = 4)
#no built in mode function....
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

abline(v=(getmode(putnam)), col="blue", lwd = 4)
legend(x=32.5, y= .04, legend = c("mean", "median", "mode"), fill=c("red","green", 
                                                             "blue"), cex = 1.5,
       bty="n", x.intersp = .1, y.intersp = .5)

hist(iris$Sepal.Length, breaks=3, main = "Sepal length histogram, 3 breaks",
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")
hist(iris$Sepal.Length, breaks=10, main = "Sepal length histogram, 10 breaks",
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")
hist(iris$Sepal.Length, main = "Sepal Lengths, auto breaks with R", 
     xlab = "Sepal Length (cm)", cex.lab=label_size, cex.axis=label_size, 
     cex.main=title_size, cex.sub=label_size, col = "blue")



