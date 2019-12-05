library(lavaan)
#data from Keeley et al. 2005 and extensions in Grace and Keeley 2006 exploring how 
#fire severity interacts with stand age and cover
keeley <- read.csv("http://byrneslab.net/classes/lavaan_materials/Keeley_rawdata_select4.csv ")

#sem vs lm####
keeley_formula1 <- 'firesev ~ age'
class(keeley_formula1)
keeley_sem1 <- sem(keeley_formula1, data = keeley)
summary(keeley_sem1)

keeley_lm <- lm(firesev ~ age, data = keeley)
summary(keeley_lm)
summary(keeley_sem1, standardize = T, rsq = T)

#plot
library(lavaanPlot)
lavaanPlot(model = keeley_sem1, coefs = TRUE)
lavaanPlot(model = keeley_sem1, coefs = TRUE,
           stand=TRUE)

#2nd model####
keeley_formula2 <- '
firesev ~ age
cover ~ firesev
'

keeley_sem2 <- sem(keeley_formula2, data = keeley)
summary(keeley_sem2, standardize = T, rsq = T)
lavaanPlot(model = keeley_sem2, coefs = TRUE)
lavaanPlot(model = keeley_sem2, coefs = TRUE,
           stand=TRUE)

#3rd model####
keeley_formula3 <- '
firesev ~ age
cover ~ firesev + age
'

keeley_sem3 <- sem(keeley_formula3, data = keeley)
summary(keeley_sem3, standardize = T)
lavaanPlot(model = keeley_sem3, coefs = TRUE)
lavaanPlot(model = keeley_sem3, coefs = TRUE,
           stand=TRUE)
#another layout
lavaanPlot(model = keeley_sem3, coefs = TRUE, stand=TRUE,
           graph_options = list(layout = "circo"),sig = 0.05)

#compare####
anova(keeley_sem2, keeley_sem3) #null is that models are different!

