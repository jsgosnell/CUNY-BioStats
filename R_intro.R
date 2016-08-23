##Introduction to R

########################## Things to know about R (General information):

# R is a programming language, and as such it can be annoying to use.  However,
# it's free, extremely powerful, and replicable (you can send code to a colleague
# or student; compare this to walking someone through JMP steps)

# You can enter stuff directly into R (command line), or you can use a script (like this). 
# Using a script allows you to save what you did (generally a good idea). To 
# run what you have in a script, simply select it, right click, and select "Run
# line or selection"

# R is an object-based language.  All that means is you can define objects (x=2, 
# or a list, or a matrix) and then use x for your calculations.  Sometimes it 
# helpful to understand what's happening (is the object being changed, evaluated,
# or modified?)

#Every object in R has a class (list, matrix, dataframe, timeseries).  You can
#manipulate different classes in different ways using different commands (functions
#, see next)

# As a language, R runs commands (normally function) from different libraries.  When you install R 
# a number of base packages are installed automatically.  Howevever, you may find
# you eventually need to download a new library.  We'll walk through this during 
# tutorial.

# Every function (including ones you can make yourself) have to be fed certain inputs.
# This normally looks somethign like functionname ( part1,part2,part3).  R also
# recognizes names of the different parts, so you can technically do 
# functionname(part3 = , part2 = , part1 = ).  It will also guess what you mean if you 
# didn't enter something. Sometimes this is bad.  If you forget what a function
# needs, you can type ?functionname or ??functionname for help.  For example, lm is the linear model
# function.
?lm
# under usage, the help file shows you what the function needs to be told.  Any
# time it says part = something, thats the default that you don't have to fill in


# Graphical user interfaces (GUI's) also exists for R.  Revolution R (which we'll
# discuss later also helps by adding more buttons

############################# Learning by doing: some r examples:

# a # sign in front of a line means its not read by the program; its a note
#think of this as your lab notebook; write down what you are doing

#first, lets play with some simple stuff
2

# see, it pops up in your console

2 + 2

# a rather complicated calculator, but it works

(2 + 2) 2

# but that didn't. Realize that you have to put in operators, like

(2 + 2) * 2

# that should be better.  Now let's make some objects

x <- 2
#note here:  <- is equivalent in most cases to =, but style guides recommend using <- (and = can supposedly
#cause trouble in some instances, though I've never seen them

x
class(x)
#class (above) is the first recognizable function you've used.  A function is a set of code
#that has been given a name (saved as an  object).  The code states what type of input
#(parameters or values, x in the case above) the code needs to run and produce some type of 
#output. You can recognize functions by their phrase() setup.

y <- 2
y
x + y

#simple enough, but larger lists or objects work the same way

x <- c(2, 2, 2, 2)
class(x)
x
# now you've made a list.  The c stands for concatenate and 
#you'll see it used often


x <- matrix(c(2, 2, 2, 2), 2, 2)
class(x)
x

# and a matrix.  Just a note, if you are doing matrix calculations directly, you 
# need to use different operators:  Example)


y <- matrix(data=c(1, 1, 1, 1), nrow=2, ncol=2)
#note here:  I am usually bad about not specifying parameter inside a function call (compare x and y assignmnets above).
# R will guess what you want, but if you don't specify things can go wrong

y

x  +y
 
x * y 
# this is wrong 

x %*% y
# this is right

# You can also call out certain parts of a list

#making a random number set for illustration
x <- rnorm(10, 1, 1)
x
x[2]

# or drop one

x [-2]

#note here: this isn't replacing x (just using or showing it) unless you assign it, like
#x <- x[-2]

# Comparing objects or elements

x <- matrix(c(2, 2, 2, 2), 2, 2)
x
y
x == y
x != y
x < y
x >= y

# all of the above is relatively useless, but it will help you understand later parts

# some useful functions 

#making a random number set for illustration
x <- rnorm( 1000, 100, 1)
#above notating is lazy (not specifying what every parameter is actually being
#used for in the function).  you can also write
x <- rnorm (n = 1000, mean = 1, sd = 1)

mean(x)
var(x)
sd(x)
x>100
subset(x, x > 100)
#or
x[x>100]
subset(x, x > 102)


# now, lets play with real data and show how you might actually use this. One of the trickiest
# parts of R is getting the data in.  Here are a few ways.

file <- file.choose()
#this should open a popup window. select the provided file

mydataset <- read.csv(file)

# and thats it.  however, this is not a great way to grab data (or automate code).  you can
#also do

mydataset <- read.csv("C:/Users/SGosnell/Desktop/Example data set.csv")

#try this using the provided file
#for your own datasets, note you want to use short column titles and avoid spaces
#(uses _ or SnakeCase).  You should also use "long format" (one observation per row)
# and use .csv or .txt formats.
#Later in class we'll discuss using plyr and reshape packages to reformat data

#if your data sets don't have headers, you'll need to adjust the code, but you normally
# do. to check what function requires, defaults to, or does, type

?read.csv

# its always good to make sure the data are in correctly. for today we'll be using the built in 
#airquality dataset

airquality
# if this is too big, try

head(airquality)
# this shows you the first 10 lines

summary(airquality)
#gives you basic summary statistics

sapply(mydataset, class)
sapply(airquality, class)
# a little more complicated. there are a set of commands that let you apply a command across
# a vector. class is checkign for what type of variable R thinks you have put in (factor,integer,
# or numeric).  lets assume you need to change a column (a common example is Trial is a 
# factor, not an integer). You can call up a column using the dollar sign

airquality$Month <- as.factor(airquality$Month)

#similar commands exist (as.integer, as.numeric) for other classes


# once your classes are set, let's do some basic plotting.  there are multiple plotting commands in r.  the
# simplest is plot.  You can plot by specifying x and y coordinates or by using a the formula setup.  In r, 
# formulas are marked as something ~ (explained by) something else)


plot(airquality$Ozone, airquality$Temp)
plot(Ozone~Solar.R, data <- airquality)
plot(Ozone~Solar.R, airquality, type = "l")
plot(Ozone~Month, airquality)
pairs(airquality)

#if you check out plot, you'll also notice you can change type and thousand other things,


# Now lets do some basic analyses.  the easiest way to do this is using linear models (a more general description
# for regression, t-tests, ANOVAs, ANCOVAs.  If you prefer there are commands that specifically do these only

fitair <- lm(Ozone~Month+Temp+Solar.R+Wind, airquality)
#the above code creates a model object

#now lets look at what we found
summary(fitair)

#or we can get traditional p values. we need to install a library to do this.  

#Illustrating help function:  Let's say you need to use a function but when you run it  you get an error like this

Anova(fitair, type = "III") #could not find function

#then use can use ??Anova to figure out which library its in

??Anova

# and install the library.  To install the library car, in the top row, click packages, install packages.
# then find a closer mirror.  Then find the car  package. once the library is installed it will stay on your computer forever. However, you'll have to 
# load it when you need to use it (unless you mess with start up files).  just type 

require(car)

Anova(fitair, type = "III") #always specify type 3, else order matters for the model

#we can also consider interactions
fitairinteractions <- lm(Ozone ~ Month * Temp, airquality)
summary(fitairinteractions)
Anova(fitairinteractions, type  = "III")

# Renaming and subsetting data
# What if we wanted to rename months to their normal names
levels(airquality$Month)
levels(airquality$Month) <- c("May","June", "July", "August", "September")
levels(airquality$Month)
#this is also useful if you want to combine months/treatments

#now look at the plot again
plot(Temp ~ Month, airquality)
#remember, check out ?plot to see more advanced commands
par(mfrow  = c(2,2), oma  = c(2,0,0,0), mar  = c(5,4,2,2))
plot(Ozone ~ Temp, subset(airquality, Month == "June"),ylab  =  "Temperature",
     xlab  = "Ozone level (ppm))", main  = "June", xlim = c(60,100), ylim  = c(0, 175))
plot(Ozone ~ Temp, subset(airquality, Month == "July"), xlab  = "", ylab  = "",
     main  = "July", xlim  = c(60,100), ylim  = c(0, 175))
plot(Ozone ~ Temp, subset(airquality, Month == "August"), xlab  = "", ylab  = "",
     main  = "August", xlim  = c(60,100), ylim  = c(0, 175))
plot(Ozone ~ Temp, subset(airquality, Month == "September"), xlab  = "",
     ylab  = "", main  = "September", xlim  = c(60,100), ylim  = c(0, 175))
mtext("Figure 1:  Ozone Levels for the Summer Months of 2001", 1, outer=T, cex=1.5, line=1)
#later in the class we'll transition to using the ggplot2 package for plotting

# what if we only cared about the impact of temperature on Ozone in July

fitjuly <- lm(Ozone~Temp, subset(airquality, Month == "July"))
summary(fitjuly)
Anova(fitjuly, type = "III")


#More advanced code


## PROGRAMMING AND FLOW CONTROL
#	1. FOR loops

for(x in 1:10){
	print(x+1)
}

#	2. WHILE loops
x<-1
while(x<10){
	print(x)
	x<-x+x
}

#	3. Defining functions

timestwo <- function (x) {
	x+x
}

timestwo(12)

timestwo(c(1,2,3,4))

