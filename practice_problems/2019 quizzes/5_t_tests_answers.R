sport <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T)
##HO: average red blood cell count  of males training for tennis at the facility is 5.4
##HA: average red blood cell count  of males training for tennis  at the facility is not 5.4

#looks spread/normal and sample size is small
plot(sport[sport$Sex == "male" & sport$Sport == "Tennis", "RCC"])
hist(sport[sport$Sex == "male" & sport$Sport == "Tennis", "RCC"])

##normal based method since  sample is small
t.test(sport[sport$Sex == "male" & sport$Sport == "Tennis", "RCC"], mu = 5.4)

####too small for bootstrap!
