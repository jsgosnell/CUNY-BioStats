#multiple category binomial tests/goodness of fit tests

#Let's consider heart attack incidences in the US.  Read in the data from 
#http://statland.org/R/R/heartatk4R.txt", header=T.  
#
heart<-read.table("http://statland.org/R/R/heartatk4R.txt", header=T)
head(heart)
str(heart)


#lets test if heart attacks occur equally across genders, assuming 50% split in population
#how to get tabular data?
table(heart$SEX)
chisq.test(table(heart$SEX))
#same as
chisq.test(c(5065,7779))
#same as
chisq.test(c(5065,7779), p=c(.5,.5))
chisq.test(table(heart$SEX), p=c(.5,.5))#same as
#but we prefer 
binom.test(5065, 5065+7779, .5) #why? chi-square is normal approximation
#
#we need chi-squared test when we have more than 2 categories####
#example: what actually led to the heart attack
#diagnosis tells which part of hear was affected, so lets use that as a proxy
#change diagnosis to a factor
heart$DIAGNOSIS <- as.factor(heart$DIAGNOSIS)
summary(heart$DIAGNOSIS)
#without knowing what these represent, are they distributed equally among the groups?
chisq.test(summary(heart$DIAGNOSIS))
# same as
chisq.test(summary(heart$DIAGNOSIS), p = c(rep(1/nlevels(heart$DIAGNOSIS), nlevels(heart$DIAGNOSIS))))
#not equally spread out
#
#also need to test independence among groups####
#is there a relationship between gender and diagnosis?
table(heart$SEX, heart$DIAGNOSIS)
chisq.test(table(heart$SEX, heart$DIAGNOSIS)) 
#p = .0022
#what does this mean?


#but chisquared test isn't appropriate from small sample sizes
#whats the split in people under 30?
table(heart[heart$AGE<30,]$SEX, heart[heart$AGE<30,]$DIAGNOSIS)
#same as
table(heart[heart$AGE<30, "SEX"], heart[heart$AGE<30, "DIAGNOSIS"])

chisq.test(table(heart[heart$AGE<30, "SEX"], heart[heart$AGE<30, "DIAGNOSIS"]))

#instead, use fisher.test
fisher.test(table(heart[heart$AGE<30, "SEX"], heart[heart$AGE<30, "DIAGNOSIS"]))
#what are your results? what do they mean?

#putting data in manually
#requires matrix command
#each row is group, so put in by row, specify number of rows, and put byrow = T
table(heart[heart$AGE<30, "SEX"], heart[heart$AGE<30, "DIAGNOSIS"])
#becomes
results <- fisher.test(x = matrix(c(1,3,0,0,0,0,0, 0, 4, 
                                    2,5,0,1,7,0,4, 0, 0), nrow = 2, byrow = T))

#if you save object you can call p-value
results
#and expected values
results$expected

#gtest####
require(DescTools)
GTest(x = matrix(c(1,3,0,0,0,0,0, 0, 4, 
                       2,5,0,1,7,0,4, 0, 0), nrow = 2, byrow = T))

#what about more categories
#is smoking independent of exercise
#http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
smoke <- chisq.test(matrix(c(7, 1, 3, #spacing just for visual use
                    87,18,84,
                    12,3,4,
                    9,1,7), nrow = 4, byrow = T))
smoke$expected #too small!
fisher.test(matrix(c(7, 1, 3, #spacing just for visuals
                    87,18,84,
                    12,3,4,
                    9,1,7), nrow = 4, byrow = T))

#what if we do find a significant difference
#lets use some dolphin data
#Data on dolphin behavior was collected off the coast of Iceland. Data is @
#http://www.statsci.org/data/general/dolpacti.txt
#Since this is a .txt file, not a .csv, you'll need to use something like
dolphin <- read.table("http://www.statsci.org/data/general/dolpacti.txt", sep="", header = T)
#More info on data @ 
#http://www.statsci.org/data/general/dolpacti.html
#difference between traveling and not traveling by hand  
travel <- chisq.test(matrix(c(6, 28+ 38, 6, 9, 14, 9, 13, 66), nrow = 4, byrow = T))
travel
#wheres the difference
#easier if you make a table
travel_table <- as.table(matrix(c(6, 28+ 38, 6, 9, 14, 9, 13, 66), nrow = 4, byrow = T))
colnames(travel_table) = c("travel", "not_travel")
rownames(travel_table) = c("morning", "noon", "afternoon", "night")
#now look at it
travel_table
require(fifer)
chisq.post.hoc(travel_table,
               control = "holm")

#using table to begin with
#difference using table
dolphin_table <- table(dolphin$Period, dolphin$Period)
chisq.test(dolphin_table)
chisq.post.hoc(dolphin_table, control = "holm")