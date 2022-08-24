##INTRODUCTION TO R####
##JSG, 7/2/22

#this is a broad introduction to how R works and what it can do
#we'll return to individual topics in later sessions (both theory and practice)

#Things to know about R (General information):####

# R is a programming language, and as such it can be annoying to use at first (steep
#learning curve). However,it's free, extremely powerful, and replicable (you can 
#send code (scripts, like this file, see below) to a colleague or student or use it again later. 
#Compare this to walking someone through JMP steps). or updating analysis.
#It's also very useful to be able to return to code months (or years) later
#and run it again (as the program will load data, analyze, and graph it in one step)

# You can enter stuff directly into R (command line), or you can use a script. A 
# script is just a text file (like this), usually a .R file but any editor can open it.
# Using a script allows you to save what you did (generally a good idea). To
# run what you have in a script, simply select it, right click, and select "Run
# line or selection".  You can also add comments to a script to guide others (and you)
# on what you were trying to do.  A # sign in front of a line (like all these)
# means its not read by the program; its a note.  #### will automate a "table of contents"
# in Rstudio.  Think of this as your lab notebook; write down what you are doing and why!
# 
# if you are writing scripts, its good practice (and will make your life easier) to develop 
#and implement good habits (not always practiced in these scripts... a classic do as I 
#say, not as I do comment).  As Hadley William's states, "Good coding style is like 
#using correct punctuation. You can manage without it, but it sure makes things easier
# to read.". His guide is @ 
# http://adv-r.had.co.nz/Style.html
# and I recommend using it

# We'll only use scripts this once. I'll introduce you to Rmd files soon, which
# are a much better way of combining text (prose) and code in literate programming.

# R is an object-based language.  All that means is you can define objects (x=2,
# or a list, or a matrix) and then use x for your calculations.  Sometimes it
# helpful to understand what's happening (is the object being changed, evaluated,
# or modified?)

#Every object in R has a class (list, matrix, dataframe, timeseries).  You can
#manipulate different classes in different ways using different commands (functions
#, see next).  We'll work on manipulating on these throughout the semester

# As a language, R runs commands (normally function) from different libraries.  When you install R
# a number of base packages are installed automatically.  Howevever, you may find
# you eventually need to download a new package.  We'll walk through this during
# tutorial.

# Every function (including ones you can make yourself) have to be fed certain inputs.
# This normally looks something like functionname ( part1,part2,part3).  R also
# recognizes names of the different parts, so you can technically do
# functionname(part3 = , part2 = , part1 = ).  It will also guess what you mean if you
# didn't enter something. Sometimes this is bad.  If you forget what a function
# needs, you can type ?functionname or ??functionname for help.  For example, lm
#is the linear model
# function.
?lm
#for odd symbols, put them in single quotes
?'?'
# under usage, the help file shows you what the function needs to be told.  Any
# time it says part = something, thats the default that you don't have to fill in


# Graphical user interfaces (GUI's) also exists for R.  We'll use Rstudio in class, 
# which presents data, objects, plots, and other things you create in various windows
# and gives you good options for saving and manipulating things.  It also color codes your
# scripts and help you find missing brackets, parentheses, and other errors.
# Other options include Revolution R
# 
# Rstudio also uses sections. Any line marked by at least 4 trailing #,-, or =
# signs is labelled a section that you can collapse and that is added to the 
# "label area" below
# 
# Using this exercise (in R) assumes you have a computer with R (and maybe Rstudio)
# on it.  A practice guide similar to this is available in the first three chapters of
# YaRrr! The Pirate's Guide to R
# https://bookdown.org/ndphillips/YaRrr/

# LEARNING BY DOING: SOME R EXAMPLES ####

#remember
#a # sign in front of a line means its not read by the program; its a note
#think of this as your lab notebook; write down what you are doing

#first, lets play with some simple stuff. highlight this line and the two below it,
#and hit run
2

# see, it pops up in your console. do it again for the next line
2 + 2

# a rather complicated calculator, but it works. keep going

(2 + 2) 2

# first error! Realize that you have to put in operators, like

(2 + 2) * 2

# that should be better.  
# 
# INTRO TO OBJECTS####
# Now let's make some objects#

x <- 2

#note here:  <- is equivalent in most cases to =, but style guides recommend
#using <- (and = can supposedly
#cause trouble in some instances, though I've never seen them. if you are using Rstudio,
#x is also now shown in your global environment pane, and you can call it in the console

x

#to see its value.  A few notes on naming things: follow styleguides by keeping it short,
#simple, but descriptive.  Also try not to use an existing function name!

#YOUR FIRST FUNCTION####
#now lets go back to the class thing and use our first function#
class(x)
#class (above) is the first recognizable function you've used.  A function is a set of code
#that has been given a name (saved as an  object).  The code states what type of input
#(parameters or values, x in the case above) the code needs to run and produce some type of
#output. You can recognize functions by their function_name() setup. Some functions
# (like class) operate on the data in the variables as a data set and produce
#a summary.  Others apply themselves to each value in the data set (like
log(x)
#note this doesn't change x!  You can name function output and save it, like
log_x <- log(x)

#let's assign some more variables
y <- 2
y
x + y


#simple enough, but larger lists or objects work the same way

x <- c(2, 2, 2, 2)
class(x)
x
# now you've made a list (The c stands for concatenate and you'll see it used often)

x <- matrix(c(2, 2, 2, 2), 2, 2)
class(x)
x

#now x and a matrix. note we've named x three times, and thus replaced it!  

#If you need to enter text, you can put it in quotes
location <- "alpine"
zones <- c("alpine", "valley", "meadow")
#entering or manipulating text is important when we want R to think about factors
#or grouping variables, instead of numbers(example to come).  R tries to figure
#out what you want, but you can specify by making someting a factor
location <- factor(location)
location

#Just a note, if you are doing matrix calculations directly, you
#need to use different operators:  Example)
y <- matrix(data=c(1, 1, 1, 1), nrow=2, ncol=2)
#note here:  I am usually bad about not specifying parameter inside a function
#call (compare x and y assignmnets above). R will guess what you want, but if
#you don't specify things can go wrong

y

x + y

x * y
# this is wrong

x %*% y
# this is right

#now back to lists.  You can also call out certain parts (elements) 
#of a list or vector. 
#making a random number set for illustration
x <- rnorm(10, 1, 1)
x
x[2] #this calls out the 2nd element (and also shows you can comment in-line)

#now drop an element
x [-2]

#note here: this isn't replacing x (just using or showing it) unless you assign it, like
#x <- x[-2]

#you can also call out a subset or specific options
x[c(1,2)]
x[x < 1]

# above is an example of comparing objects or elements: R compares objects and
#returns TRUE or FALSE

x <- matrix(c(2, 2, 2, 2), 2, 2)
y <- matrix(data=c(1, 1, 1, 1), nrow=2, ncol=2) #re-assigned here in case skipped above
x
y
x == y
#Notice the question is asked for each value in the variable, so the return value
#has the same length (3).
x != y
x < y
x >= y

# all of the above is relatively useless (you would never use R for these simple 
# actions), but it will help you understand later parts

# some useful functions

#making a random number set for illustration
x <- rnorm( 1000, 1, 1)
#remember above notation is lazy (not specifying what every parameter is actually being
#used for in the function).  you can also write
x <- rnorm (n = 1000, mean = 1, sd = 1)

#BASIC WAYS OF SUMMARING DATA####

mean(x)
var(x)
sd(x)
x > 100 #this returns a logical output
#but this returns the actual data
subset(x, x > 1)
#or use brackets (which I prefer because they are tedious but clear and handle compound
#statements better)
x[x>1]
subset(x, x > 102)


#now, lets play with real data and show how you might actually use this. 

#GETTING DATA IN ####
#One of the trickiest parts of R is getting the data in.  We'll use some of the built-in 
#R datasets throughout class to show you how functions work, which also allows these
#scripts be self contained, but here are a few ways to get data in.
#
#first, let's make a dataset. open excel, fill in the top row with 3 names (e.g., 
#x,y,z) and then add a few rows of "data"
#save this as a .csv file, then run
#
file <- file.choose()
#this should open a popup window. select the file you created. note this is just making a path
#to the file

file

#which is then read by the read.csv function

my_dataset <- read.csv(file, stringsAsFactors = T)

# Since ~2020 you need to as StringsAsFactors = T to read in characters as factors
# (what we typically want)

#you can also read in data from a website holding a csv (we'll do this often in class)
australia_athlete_data <- read.table("http://www.statsci.org/data/oz/ais.txt", header = T,
                                     stringsAsFactors = T)

# and thats it.  however, this is not a great way to grab data (or automate code).
#you can also put your path in directly, e.g., 
#my_dataset <- read.csv("C:/Users/SGosnell/Desktop/Example data set.csv")
#
#Note here: R is always "looking"somewhere for files - we call this place the working
#directory. It's also where R will eventually save output.   You can check the working directory using
getwd()
#Files and folders in that location can be called directly.  You can change the 
#working directory using the setwd() function. For example, I could also use
#setwd("C:/Users/SGosnell/Desktop")
#to change the working directory, then use
#my_dataset <- read.csv("Example data set.csv") 
#to call the file.  It's useful to set the working directory at the beginning 
#of a script so you know where files R going and coming from.

#you can try to call the file you made by specifying the location and/or working 
#directory 

#NOTES ON COLUMN NAMES####
#for your own datasets, note you want to use short column titles and avoid spaces
#(uses _ or SnakeCase).  You should also use "long format" (one observation per row)
# and use .csv or .txt formats.  You should also remember R won't mix groups,
#i.e. if you enter some numbers and some characters, it will coerce everything to a character
#Later in class we'll discuss using plyr and reshape packages to reformat data

#you can also make a dataset directly in R, though its tedious for larger datasets
greenness <- c(13766, 50513, 25084)
habitat <- c("forest", "forest", "grassland")
date <- c("2009-12-25", "2010-01-01", "2010-01-15")
#dates can be handled multiple ways in R, but the key idea is to make sure you
#put them in so they are more than factors
date <- as.Date(date)
#as.Date represents time relative 1970-01-01 (earlier dates are negative)
date[1] < date[2]
birds <- data.frame(greenness = greenness, habitat = habitat, date = date)

#if your data sets don't have headers, you'll need to adjust the code, but you normally
# do. remember, to check what function requires, defaults to, or does, type

?read.csv

#NOTE: AS OF EARLY 2020, R NO LONGER READS IN STRINGS AS FACTORS BY DEFAULT!  SO 
#TO GET BAR GRAPHS, ETC, YOU MAY HAVE TO MANUALLY CHANGE A COLUMN TO A FACTOR. 
#YOU CAN DO THIS FOR SINGLE INSTANCE OR CHANGE THE ACTUAL OBJECT
#
#EXAMPLE OF DATA ANALYSIS STEPS####

#LOAD THE DATA####
# its always good to make sure the data are in correctly. for today we'll be using the built in
#airquality dataset

airquality
# if this is too big, try
# 
#CHECK THE DATA####

head(airquality)

#by default this shows you the first 10 lines. note R stores data in vector formats, so we'll
#try to use commands that take advantage of that to speed up analysis
str(airquality)
#shows yous the structure of the dataframe
#also useful for checking for what type of variable R thinks you have put in (factor,integer,
# or numeric).  lets assume you need to change a column (a common example is Trial is a
# factor, not an integer). if you don't fix this R may run the wrong analyses (e.g.,
# a regression when you want an ANOVA).

airquality$Month <- as.factor(airquality$Month)
#similar commands exist (as.integer, as.numeric) for other classes.note how you use
#this as it may give unusual results. For example, if you turn factor levels into numbers
#they will go fro 1 up g (total # of levels) based on the order R had them in (often
#alphabetical).  If you need to turn numbers from factors to numbers,
#as.numeric(as.character(x)). the easier lesson is to name things rights to begin with

summary(airquality)
#gives you basic summary statistics

# 
# You can call up a column using the dollar sign and change it 
# to a transformed version of itself
# NOTE NONE OF THIS IS CHANGING THE FILE YOU READ IN TO R!


# EXPLORATORY DATA ANALYSIS - PLOTTING AND NUMERICAL SUMMARIES####
# once your classes are set, let's do some basic plotting.  there are multiple 
# plotting commands in r.  the simplest is plot.  You can plot by specifying x and 
# y coordinates or by using a the formula setup.  In r, formulas are marked as
# something ~ (explained by) something else)

plot(airquality$Ozone, airquality$Temp)
plot(Ozone~Solar.R, data = airquality)
plot(Ozone~Solar.R, airquality, type = "l")
plot(Ozone~Month, airquality)

#in Rstudio, note plots are automatically saved each session and you can scroll through,
#zoom, plot, and save as needed

#a useful function for visually exploring relationships
pairs(airquality)

#if you check out plot, you'll also notice you can change type and a thousand
#other things.
plot(Ozone~Temp, airquality, col=airquality$Month)

#We'll be introducing another graphing package, ggplot2, later this semester

#FORMAL ANALYSIS####
# Now lets do some basic analyses.  the easiest way to do this is using linear models (a more general description
# for regression, t-tests, ANOVAs, ANCOVAs.  If you prefer there are commands that specifically do these only

ozone_month_relationship <- lm(Ozone~Month+Temp+Solar.R+Wind, airquality)
#the above code creates a model object

#now lets look at what we found
summary(ozone_month_relationship)

#or we can get traditional p values (don't worry if you don't know/remember what 
#these are).  we need to install a package to do this.

#FUNCTIONS FROM NON-BASIC PACKAGES####
#Illustrating help function:  Let's say you need to use a function but when you run it  you get an error like this

Anova(ozone_month_relationship, type = "III") #could not find function

#then use can use ??Anova to figure out which package its in

??Anova

# and install the package.  To install the car package in Rstudio, in the top row, 
# click packages, install packages. then find a close mirror.  Then find the car 
# package. once the package is installed it will stay on your computer forever. 
# However, you'll have to load it when you need to use it (unless you mess with 
# start up files).  just type

library(car)
#library is better than require function. Packages are "stored" in your library 
#location. You can set and see this using the .libPaths() function
.libPaths()
#Adding a file location in the parentheses makes that a library location.  This 
#isn't necessary on school computers, but on your own machine you may want different 
#locations for versions of R if you stick with this.

Anova(ozone_month_relationship, type = "III") #always specify type 3, else order 
#matters for the model. we'll get to this later

#if we do single factors, we can also add lines to plots
ozone_month_relationship <- lm(Ozone~Temp, airquality)
plot(Ozone~Temp, airquality)
abline(ozone_month_relationship)
#other commands like points() and lines() can also add to existing plots

#we can also consider interactions
ozone_month_relationshipinteractions <- lm(Ozone ~ Month * Temp, airquality)
summary(ozone_month_relationshipinteractions)
Anova(ozone_month_relationshipinteractions, type  = "III")

# RENAMING, SUBSETTING, AND SORTING DATA####
# there are lots of ways to do this
# What if we wanted to rename months to their normal names
# factors have levels that you can change
levels(airquality$Month)
levels(airquality$Month) <- c("May","June", "July", "August", "September") 
#above is a built-in way to do this, but we'll use relevel in class (later) as i
#find its less prone to mistakes
levels(airquality$Month)

#data frames have names(headers) you can change similarly
#you can also specifically change indiviudal names or levels with
names(airquality) #gives you column names
names(airquality)[names(airquality) %in% "Month"] = "example_change" # %in% looks for matches
names(airquality)
names(airquality)[names(airquality) %in% "example_change"] = "Month"

#you could also call by index
names(airquality)[3] <- "example_change"
names(airquality)
names(airquality)[3] <- "Wind"

#or for levels
levels(airquality$Month)[levels(airquality$Month) %in% "August"] = "test_change"
levels(airquality$Month)[levels(airquality$Month) %in% "test_change"] = "August"
#other commands will do this easier (relevel) but you can use brackets to specify most changes 

#this is also useful if you want to combine months/treatments. alternatively,
#you could have duplicated the column and changed the copy. the main idea here,
#however, is you are never impacting the actual data file (unless you specifically
#save over it). for example, we could add
airquality$log_Temp <- log(airquality$Temp)

#now look at the plot again
plot(Temp ~ Month, airquality)
#note this is a box-whisker plot by default
#remember, check out ?plot to see more advanced commands
#par lets you set options to multiple plots at once
#we'll get into this more in depth later, but the par command sets graphic details for
#upcoming plots. mrfow says to set a 2x2 matrix for table, oma and mar set margins
#don't worry too much about this now
par(mfrow  = c(2,2), oma  = c(2,0,0,0), mar  = c(5,4,2,2))
#notice we are subsetting data by month for the graph
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

# what if we only cared about the impact of temperature on Ozone in July for models
july <- lm(Ozone~Temp, subset(airquality, Month == "July"))
summary(july)
Anova(july, type = "III")

#in general, you can subset dataframes using the subset command (above),
#where the subset argument specifies rows and the select argument specifies columns,
#or by using brackets like we did for vectors. The generla format is [rows, columns], like
airquality[airquality$Month == "July",]
# or you combine commands, with & (and) or | (or) statements
airquality[airquality$Month == "July" & airquality$Temp > 85,] #returns only days in June > 85
airquality[airquality$Month == "July" | airquality$Temp > 85,] #returns  days in June and days > 85

#an empty space before or afer the comma implies "all", so this is all columns
#for all rows where the month is July. you can also query by column name
airquality[,"Month"]

#note on sorting data
#you can also use this to sort data
airquality[order(airquality$Temp),]
#order (used here to order rows) puts the dataframe in ascending order of temps.
#in general, order returns the index (row number) need to put the dataset in ascending order,
#while sort returns the value itself


