####exam#### 
setwd("C:/Dropbox/Stephen/Teaching and Mentoring/Statistics/Undergraduate Biostatistics/fall 2017")

#1####
require(binom)
binom.test(6,35, p=.08)
binom.confint(6,35)

#2####
tv_watching <- matrix(c(2,7,38,212,10,150), ncol = 2, byrow = T)
rownames(tv_watching) = c("none", "some", "alot")
chisq.test(tv_watching)
chisq.test(tv_watching)$expected
fisher.test(tv_watching)
#significant, so post-hoc
require(fifer)
(bonf_correct <- chisq.post.hoc(tv_watching,
                               control = "bonf"))
2/7
38/212
10/150

#3 ####
# elf_data <- data.frame(
#   hours_tv_day = rchisq(500,2),
#   number_of_siblings = sample (c(rep(0,4), rep (1, 8), rep(2, 7), rep(3,4), rep (4, 2),
#                                 rep (5, 1), rep (6,1)), 500, replace = T),
#   state = sample(state.name, 500, replace = T),
#   urban = sample (c("urban", "urban", "rural"), 500, replace = T),
#   school_year = sample(c("K", "1", "2", "3", "4", "5", "6"), 500, replace = T),
#   school_grade = sample(letters[c(1:4,6)], 500, replace = T), 
#   gender = sample(c("boy", "girl"), 500, replace = T))
# elf_data$goody_score <- 100 - 2 * (elf_data$hours_tv_day+ rnorm(500, 2))- 
#   (3 +  rnorm(500, 3)) * (elf_data$number_of_siblings)  - 
#   (2 + rnorm(500, 1)) * ((elf_data$urban == "urban")) - 
#   (4 + rnorm(500, 1)) * ((elf_data$urban == "rural"))  
# 
# summary(elf_data)
#write.csv(elf_data, "elf_data.csv")
elf_data <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/elf_data.csv?attredirects=0&d=1")
goody_full <- lm(goody_score~.,elf_data)
goody_final <- stepAIC(goody_full)
par(mfrow=c(2,2))
plot(goody_final)

#4####
require(ggplot2)
ggplot(elf_data, aes_string(x = "hours_tv_day", y = "goody_score")) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Goody score")+ 
  xlab("Daily tv consumption (hours)") + 
  ggtitle("Watching tv puts you on the naughty list")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#5####

#make data once
# toy <- data.frame (program = c(rep("CrossGift", 30), rep("PX25", 30), 
#                                rep("Bow", 30)),
#                    toys_produced_hour = c(rnorm(30, 25, 3), rnorm(30, 20, 3.5), 
#                                           rnorm(30,20.2, 3.2))
#                   )

#write.csv(toy, "toy_production.csv")
toy <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/toy_production.csv?attredirects=0&d=1")

#model
toy_model <- lm(toys_produced_hour ~ program, toy)
par(mfrow=c(2,2))
plot(toy_model)
require(car)
Anova(toy_model, type = "III")
require(multcomp)
summary(glht(toy_model, linfct = mcp(program = "Tukey")))

#plot
function_output <- summarySE(toy, measurevar = "toys_produced_hour", 
                             groupvars = "program")
require(ggplot2)
ggplot(function_output
       , aes_string(x="program", y="mean")) +
  geom_col(size = 3, fill = "green") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5, color ="red") +
  xlab ("Toy production program") +
  ylab ("Toy production after completing \n program (toys/hours)") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#look at glht output and summary to match
function_output
summary(glht(toy_model, linfct = mcp(program = "Tukey")))
function_output$character <- c("A", "B", "A")

ggplot(function_output
       , aes_string(x="program", y="mean")) +
  geom_col(size = 3, fill = "green") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5, color ="red") +
  xlab ("Toy production program") +
  ylab ("Toy production after completing \n program (toys/hours)") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) +
  geom_text(aes(x=program, y=mean + 2, label=character))

#toy$prior_speed <-toy$toys_produced_hour - rnorm(90,0,3)
#write.csv(toy, "toy_with_prior_speed.csv")
toy_with_prior_speed <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/toy_with_prior_speed.csv?attredirects=0&d=1")
function_output <-summarySE(toy_with_prior_speed, measurevar = "prior_speed", 
                                      groupvars = "program")
ggplot(function_output
       , aes_string(x="program", y="mean")) +
  geom_col(size = 3, fill = "green") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5, color ="red") +
  xlab ("Toy production program") +
  ylab ("Toy production before completing \n program (toys/hours)") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) 

#6####
#make data
# reindeer_speed <- data.frame(feed = c(rep("old", 50),rep("new",50)),
#                              sleep = c(rep(c(rep("little",25), rep("lot",25)),2)))
# reindeer_speed$speed <- 20 + 
#   (5 + rnorm(100, 0, 1)) * (reindeer_speed$sleep == "lot" & reindeer_speed$feed == "new") +
#   rnorm(100, 0, 1)

#write.csv(reindeer_speed, "reindeer_speed.csv")
reindeer_speed <-read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/reindeer_speed.csv?attredirects=0&d=1")
#check
speed_lm <- lm(speed ~ sleep * feed, reindeer_speed)
require(car)
Anova(speed_lm, type = "III")

#split
speed_lm <- lm(speed ~ sleep, reindeer_speed[reindeer_speed$feed == "new",])
Anova(speed_lm, type = "III")

speed_lm <- lm(speed ~ sleep, reindeer_speed[reindeer_speed$feed == "old",])
Anova(speed_lm, type = "III")


#plot
function_output <- summarySE(reindeer_speed, measurevar="speed", groupvars =
c("feed", "sleep"), na.rm = T)
ggplot(function_output, aes_string(x="feed", y="mean",color="sleep", 
                                   shape = "sleep")) +
  geom_point(size = 5) +
  geom_line(aes_string(group="sleep", linetype = "sleep"), size=2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), size=1.5) +
  ylab("Speed (km/hour)")+ 
  xlab("Feed") + 
  ggtitle("Impacts of sleep and feeding on reindeer speed")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
  
  