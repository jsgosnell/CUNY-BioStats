##R  Diversity Metrics
.libPaths("C:/Dropbox/Stephen/R_libraries_3.2.2")



#basic stats graphs from class (random numbers, so will look different)
x  <-  rnorm(n = 100)
y  <-  rnorm(n = 100, mean = 10)
z  <-  runif(n = 100, min = -20, max = 20)
z2  <-  runif(n = 100, min = 0, max = 20)
y2  <-  rnorm(n = 100, mean = -10)

wa  <-  rnorm(n = 50, mean = -10)
wb  <-  rnorm(n = 50, mean = 10)

w  <-  c(wa, wb)
par(mfrow = c(3,2), mar = c(1,4,0,1), oma = c(4,0,0,0) )
hist(w,breaks = 40, xlim = c(-20,20),  ylim = c(0,15),main = NA, xlab = NA, xaxt = "n")
hist(x, breaks = 40, xlim = c(-20,20), ylim = c(0,15), main = NA, xlab = NA, xaxt = "n")
hist(z2, breaks = 40, xlim = c(-20,20), ylim = c(0,15), main = NA, xlab = NA, xaxt = "n")
hist(y, breaks = 40, xlim = c(-20,20), ylim = c(0,15), main = NA, xlab = NA, xaxt = "n")
hist(z, breaks = 40, xlim = c(-20,20),  ylim = c(0,15), main = NA)
hist(y2, breaks = 40, xlim = c(-20,20),  ylim = c(0,15), main = NA)



#below will require the following non-base packages
#BiodiverisityR
#FD

#call libraries
#you can specify a place for these or a default
require(FD)
require(BiodiversityR)
#or macs
require(vegan)

#examples using dummy dataset from FD

#look at this dataset
#taxonomic info
dummy$abun

#Taxonomic diversity
#different options
#SCALE TO CALCULATE DIVERSITY
#for each site
H  <-  diversityresult(dummy$abun, index = "Shannon", method = "s")
#diversity for all sites pooled
H  <-  diversityresult(dummy$abun, index = "Shannon", method = "all")
#INDEX TO USE
Richness  <-  diversityresult(dummy$abun, index = "richness", method = "s")
Abundance  <-  diversityresult(dummy$abun, index = "abundance", method = "s")
Berger  <-  diversityresult(dummy$abun, index = "Berger", method = "s")
Shannon  <-  diversityresult(dummy$abun, index = "Shannon", method = "s")
Simpson  <-  diversityresult(dummy$abun, index = "Simpson", method = "s") #note this is 1-Simpson
Jevenness  <-  diversityresult(dummy$abun, index = "Jevenness", method = "s")#Eevenness uses other measure of "equal" species

#for macs
#Shannon = diversity(dummy$abun)
#Simpson = diversity(dummy$abun, index = "simpson")
#invSimpson = diversity(dummy$abun, index = "invsimpson")
#richness = specnumber(dummy$abun)
#TO CALCULATE NUMBER OF EQUALLY ABUNDANCT SPECIES
Richness
Equallyabundantshannon  <-  exp(Shannon)
Equallyabundantsimpson  <-  diversityresult(dummy$abun, index = "inverseSimpson", method = "s")


#see more
?diversityresult

#FUNCTIONAL DIVERSITY
#defaults to using relative abundance of species when possible (e.g., CWM, Rao's Q)
#defaults to standarizing (how depends of if qualitative traits are included)
#FORMATTTING THE DATA
#note the abundance and trait data hae rownames (there is no Species or Community column).  Adding one will consider
#species a functional trait and/or cause the program not to run, as the rownames in the traitdatabase must match
#the column names in the abundance database

dummy$trait
dummy$abun
functionaldiversityresults  <-  dbFD(dummy$trait, dummy$abun)
functionaldiversityresults

#merge the data

#change dbFD list output to dataframe
functionaldiversityresults  <-  as.data.frame(functionaldiversityresults)
#often useful to add community column for later analysis and necessary for merge
functionaldiversityresults$community  <-  rownames(functionaldiversityresults)
Shannon$community  <-  rownames(Shannon)
diversity  <-  merge(Shannon,functionaldiversityresults)
#take a look at the data
diversity

#can also use merge to add in envt data
#make some envt drivers
rain  <-  rnorm(n = 10,mean = 10)
precip  <-  data.frame(community = diversity$community, rain)
driversanddiversity  <-  merge(precip, diversity)
#and now can consider relationships
rainimpacts  <-  lm(Shannon~rain, driversanddiversity)
summary(rainimpacts)
#~95% of the the no impact
#why?

#P VALUES SIMULATIONS

pvalues2  <-  matrix(data = 0, nrow = 100, ncol = 1)
#note making these lists first is helpful for large datasets since R will eat up space recreating
#list each time
#run simulation
Shannonfake  <-  rnorm(n = 10, mean = 2)
for (i in 1:100){
rain  <-  rnorm(n = 10,mean = 10,sd = .5)
driversanddiversity  <-  data.frame(rain, Shannonfake)
rainimpacts  <-  lm(Shannonfake~rain, driversanddiversity)
pvalues2[i]  <-  anova(rainimpacts)$P[1]
}
hist(pvalues2, breaks = 20, xlab = "p values", main = "p-values from simulation of relationships among random variables")
#add line at .05 (our favorite number)
abline(v = .05, col = "red")
#remember p-values uniformly distributed under null hypothesis



