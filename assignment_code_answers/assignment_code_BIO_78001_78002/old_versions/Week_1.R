#1####
x <- 1:10
length(x)
max(x)
x[x < 5]
x^2
x[ x < 8 & x > 2]

#2####
f <- factor(2:4)
mean(f)
mean(as.numeric(f))
mean(as.numeric(as.character(f)))

#3####
-1:2
(-1):2
-(1:2)

sleep <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/sleep.csv")

#4####
dim(sleep)
str(sleep)
sapply(sleep, class)

#5####
names(sleep)[names(sleep) %in% "BodyWt"] = "Body_weight"

#6####
plot(TotalSleep ~ Primate, sleep)

#7####
plot(TotalSleep ~ BrainWt, sleep)
plot(TotalSleep ~ BrainWt, sleep[sleep$BrainWt<1000,])
sleep_fit <- lm(TotalSleep ~ BrainWt, sleep[sleep$BrainWt<1000,])
summary(sleep_fit)

#8####
abline(sleep_fit)

#9####
alcids <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/alcids55.csv",header = F)
names(alcids) <- c("year", "a1_abund", "NAO", "a2_abund", "a3_abund", "a4_abund", "a5_abund", "a6_abund")
head(alcids)

#10 ####
#input using the read.csv function

