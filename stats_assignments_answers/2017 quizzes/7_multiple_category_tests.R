heart<-read.table("http://statland.org/R/R/heartatk4R.txt", header=T)
head(heart)
str(heart)

#1####
table(heart$SEX)
binom.test(7779, 77779+5065)

#2
binom.test(7779, 77779+5065, .508)
#better than
chisq.test(table(heart$SEX), p=c(.508, .492))

#3####
chisq.test(table(heart$SEX, heart$DIED))

#4
chisq.test(table(heart[heart$AGE < 30, "SEX"], heart[heart$AGE <30, "DIED"]))
fisher.test(table(heart[heart$AGE < 30, "SEX"], heart[heart$AGE <30, "DIED"]))
#so if you are young, no difference


#5
#easier if you make a table
travel_table <- as.table(matrix(c(6, 28+ 38, 6, 9, 14, 9, 13, 66), nrow = 4, byrow = T))
colnames(travel_table) = c("travel", "not_travel")
rownames(travel_table) = c("morning", "noon", "afternoon", "night")
#now look at it
travel_table
chisq.test(travel_table)
require(fifer)
chisq.test(travel_table)
chisq.post.hoc(travel_table,
               control = "holm")


#6 
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
