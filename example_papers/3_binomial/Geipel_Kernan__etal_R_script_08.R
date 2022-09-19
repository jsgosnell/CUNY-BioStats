# R script for "Predation risks of signaling and searching: bats prefer katydids in motion"
# Inga Geipel*, Ciara E. Kernan*, Amber S. Litterer, Gerald G. Carter, Rachel A. Page, & Hannah M. ter Hofstede

# choose working directory----
# navigate to the file "Micronycteris_prey_movement_data.csv"
first_run <- T
if (first_run){
  setwd(dirname(file.choose())) 
  wd <- getwd()
}

# set working directory
setwd(wd)

# get data-----
raw <- read.csv("Micronycteris_prey_movement_data.csv", stringsAsFactors = F) 

# install and/or load necessary packages----
load_install <- function(x) {
  if (require(x, character.only = T)) return(require(x, character.only = T))
  else{
    install.packages(x, type="source")
    library(x)}}

load_install('tidyverse')
load_install('boot')
load_install('cowplot')

# functions----
# create some functions for estimating 95% confidence interval using bootstrapping
# bootstrap the 95% CI of the mean
boot_ci <- function(x, perms=5000) {
  mean.w=function(x,w) sum(x*w) # get weighted mean
  numNA <- sum(is.na(x))
  x <- as.vector(na.omit(x))
  mean <- mean(x)
  boot <- boot.ci(boot(data=x, statistic=mean.w, R=perms, stype="w"), type="bca")
  low <- boot$bca[1,4]
  high <- boot$bca[1,5]
  c(low=low,mean=mean,high=high, N=round(length(x)))
}

# bootstrap the 95% CI of the mean within groups 
boot_ci2 <- function(df=df, y=df$y, x=df$x, perms=5000){
  df <- data.frame(effect=unique(x))
  df$low <- NA
  df$mean <- NA
  df$high <- NA
  df$n.obs <- NA
  for (i in 1:nrow(df)) {
    b <- boot_ci(y[which(x==df$effect[i])], perms=perms)
    df$low[i] <- b[1]
    df$mean[i] <- b[2]
    df$high[i] <- b[3]
    df$n.obs[i] <- b[4]
  }
  df
}

# function for permuted paired t-test
ppt.test <- function(x){
  perms=5000
  n <- length(x)
  obs <- t.test(x)$statistic
  exp <- rep(NA, perms)
  for (i in 1:perms) {
    exp[i] <- t.test(x * sample(c(-1, 1), n, replace = TRUE))$statistic
  }
  p <- mean(abs(obs)<abs(exp))
  out=ifelse(p==0,"p<0.0002",p)
  out
}

# colors----
# choose colors that print well and work for color-blind people
colors <- c("#a6bddb", "#2b8cbe", "dark blue")

# clean data----
head(raw)
unique(raw$Bat)
unique(raw$Stimulus)
unique(raw$Treatment.choice)
unique(raw$Duration.of.choice.hover..frames.)

# rename variables
colnames(raw)
colnames(raw) <- c("order","date", "bat", "test", "position","side.choice","choice", "duration", "second.choice", "second.duration") 


# First choices----
# Test 1: Do bats choose prey that are moving more? Response is first choice.
# let's first label all cases where the bat chose higher movement

# choice of more movement for tremulating vs walking insects is unknown "NA"
data <- 
  raw %>% 
  mutate(test2= substr(test,1,1)) %>% 
  mutate(chose_more_movement = case_when(
    test2 == "A" & choice== "T"  ~ T,
    test2 == "A" & choice== "S"  ~ F,
    test2 == "B" & choice== "W"  ~ T,
    test2 == "B" & choice== "S"  ~ F,
    test2 == "C" & choice== "W"  ~ NA,
    test2 == "C" & choice== "T"  ~ NA)) 

# How many cases did bats choose more movement?
data$chose_more_movement
sum <- sum(data$chose_more_movement, na.rm=T)
total <- length(data$chose_more_movement[which(!is.na(data$chose_more_movement))])
sum 
total # There were 17 cases out of 20
mean(data$chose_more_movement, na.rm=T) # There was a 85% probability of bats choosing the higher moving prey


# permutation test

# get observed probability of choosing higher movement
observed <- mean(data$chose_more_movement, na.rm=T)

# now get expected probability if every bat chooses randomly within each test trial (5000 permutations)
perms <-  5000
exp <- rep(NA, perms)

# simulate random choices in 5000 experiments (this step might take a minute or two)
set.seed(1234)
for (i in 1:perms){
  data2 <- 
    data %>% 
    group_by(bat, test) %>% 
    mutate(chose_more_movement_rand = sample(c(T,F), 1, replace=T))
  exp[i] <- mean(data2$chose_more_movement_rand, na.rm=T)
}

# How many cases did bats choose more movement?
ggplot()+
  geom_histogram(aes(x=exp), fill= "light blue", color="black")+
  geom_vline(xintercept= observed, color="red", size=1.5)+
  xlab("average probability")+
  ylab("count of visits")+
  ggtitle("Prey choice: expected by random choice (blue) versus observed (red)")

# get p-value
p <- mean(exp>=observed)
# two-sided p-value
p*2


# We can also do a binomial test for each test type to get probability expected by chance
# This is a weak test because n=10

# test A: motionless vs tremulating, 7 of 10, p=0.34
binom.test(7,10)

# test B: motionless vs walking, 10 of 10, p=0.002
binom.test(10,10)

# test c: tremulating vs walking, 10 of 10, p=0.002
binom.test(6,10)


# proportion of first choices
plot1 <- 
  data %>%
  mutate(label= case_when(
    test2 == "A" ~ "tremulating\n over motionless",
    test2 == "B" ~ "walking\n over motionless",
    test2 == "C" ~ "walking\n over tremulating")) %>% 
  mutate(chose_expected = case_when(
    test2 == "A" & choice== "T"  ~ T,
    test2 == "A" & choice== "S"  ~ F,
    test2 == "B" & choice== "W"  ~ T,
    test2 == "B" & choice== "S"  ~ F,
    test2 == "C" & choice== "W"  ~ T,
    test2 == "C" & choice== "T"  ~ F)) %>% 
  group_by(label) %>% 
  summarize(prob= sum(chose_expected)) %>%
  mutate(label=factor(label)) %>% 
  mutate(label = fct_reorder(label, prob)) %>% 
  mutate(ps= c("NS","p=0.002","NS")) %>% 
  ggplot(aes(x=label, y=prob))+
    geom_bar(stat="identity", color="black", fill="grey")+
    geom_text(aes(label = ps), position = position_dodge(0.5), vjust = +2, size=4)+
    geom_hline(yintercept =5)+
    ylab("number of bats (out of 10 bats)")+
    xlab("first choice")+
    scale_y_continuous(breaks=1:10)+
    theme_classic()
plot1
ggsave("Plot_1.pdf", width = 4, height = 6, units = "in", dpi = 1300)

# Test 2: Do bats hover more over prey that are moving more? Response is difference in hovering time.

# get duration bias as duration at more movement minus duration with less movement
data <- 
  data %>%
  mutate(label= case_when(
    test2 == "A" ~ "tremulating\n over motionless",
    test2 == "B" ~ "walking\n over motionless",
    test2 == "C" ~ "walking\n over tremulating")) %>% 
  mutate(chose_expected = case_when(
    test2 == "A" & choice== "T"  ~ T,
    test2 == "A" & choice== "S"  ~ F,
    test2 == "B" & choice== "W"  ~ T,
    test2 == "B" & choice== "S"  ~ F,
    test2 == "C" & choice== "W"  ~ T,
    test2 == "C" & choice== "T"  ~ F)) %>% 
  mutate(duration.bias= if_else(chose_more_movement, duration- second.duration, second.duration- duration)) %>% 
  mutate(duration.bias2= if_else(chose_expected, duration- second.duration, second.duration- duration)) %>% 
  as.data.frame()

# permuted paired t-test p-value (testing bias towards more movement)
set.seed(1234)
ppt.test(data$duration.bias)

# get t-values for each test type
at <- t.test(data$duration.bias2[which(data$test2=="A")])$statistic
bt <- t.test(data$duration.bias2[which(data$test2=="B")])$statistic
ct <- t.test(data$duration.bias2[which(data$test2=="C")])$statistic

# compile mean effects and add confidence intervals
means <- 
  boot_ci2(data, data$duration.bias2, data$test2, perms=5000) %>% 
  mutate(t= c(at, bt, ct)) %>% 
  rename(test2= effect) %>% 
  mutate(label= case_when(
    test2 == "A" ~ "tremulating\n over motionless",
    test2 == "B" ~ "walking\n over motionless",
    test2 == "C" ~ "walking\n over tremulating")) %>% 
  mutate(label=factor(label)) %>% 
  mutate(label = fct_reorder(label, mean)) 

# get data points
data2 <- 
  data %>% 
  mutate(label= case_when(
    test2 == "A" ~ "tremulating\n over motionless",
    test2 == "B" ~ "walking\n over motionless",
    test2 == "C" ~ "walking\n over tremulating")) %>% 
  mutate(label=factor(label)) 

# plot side bias----
plot2 <- 
  means %>% 
  ggplot(aes(x=label, y=mean, color= label))+
  geom_hline(aes(yintercept=0), color="grey")+
  geom_point(size=4, position= position_nudge(x = 0.2))+
  geom_errorbar(aes(ymin=low, ymax=high, width=.1), size=1, position= position_nudge(x = 0.2))+
  geom_jitter(data=data2, aes(x=label, y=duration.bias2, color=label), size=2, width=0.05, alpha=0.9)+
  ylab("bias in hovering time (video frames)")+
  xlab("choice")+
  ylim(-25,25)+
  scale_color_manual(values= colors)+
  theme_classic()+
  theme(legend.position = "none")
plot2
ggsave("Plot_2.pdf", width = 4, height = 6, units = "in", dpi = 1300)




# combine plots
plot1b <- 
  plot1 + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

fig2 <- 
  plot_grid(plot1b,plot2,nrow = 2, labels= c("a", "b"))
fig2
ggsave("Fig_2.pdf", width = 4, height = 8, units = "in", dpi = 1300)
