#11/9/17 class script
#example of anova, regression, multiple regression/anova as lm, with extension to 
#mixed models

#TEAM DATA
team <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/team_data.csv?attredirects=0&d=1")
names(team)
head(team)

#lets build a model looking at variation among continents####
#this is an ANOVA
team_model_1 <-lm(PlotCarbon.tonnes ~ Continent, team)
#plot to see if assumptions are met
plot(team_model_1)
summary(team_model_1)

#analyze multiple factors using ANOVA to get traditional p-value for category
require(car)
Anova(team_model_1, type = "III")

#where's the almost difference? use Tukey's HSD for all pairs
require(multcomp)
compare_cont_tukey <- glht(team_model_1, linfct = mcp(Continent = "Tukey"))
summary(compare_cont_tukey)

#compare to another common method for post hoc comparisons
team_model_aov <- aov(PlotCarbon.tonnes ~ Continent, team)
TukeyHSD(team_model_aov)

#or we can specify any we wanted a priori
#only interested in differences from Latin America? just test those
compare_latin_america_only <- glht(team_model_1, linfct = mcp(Continent = 
                                                         c("Asia - Latin America = 0", 
                                              "Latin America - Africa = 0")))
#error because "Latin America" has space, and multcomp doesn't like that
#can fix here or redo in excel, etc
require(plyr)
team$Continnent = revalue(team$Continent, c(Latin America = "Latin_America" ))
#neither does plyr...do it directly
levels(team$Continent)[levels(team$Continent) == "Latin America"] <- "Latin_America"
#fixed, but big lesson is avoid spaces

#now, rerun model since you changed the data
team_model_1 <-lm(PlotCarbon.tonnes ~ Continent, team)
#check assumptions
plot(team_model_1)
compare_latin_america_only <- glht(team_model_1, linfct = mcp(Continent = 
                                                         c("Asia - Latin_America = 0", 
                                                           "Latin_America - Africa = 0")))
#remember to control for error rates!
summary(compare_latin_america_only, test=adjusted("holm")) #africa is not driving the difference
#other options
summary(compare_latin_america_only, test=adjusted("fdr")) #africa is not driving the difference
#we can "find" significance more easily, but you need to justify why you did this

#model comparison####
#does rainfall add anything
#use update command if you want, . means everything on that side in initial model
team_model_2 <- update(team_model_1, . ~ . + Precip_mean.mm)
summary(team_model_2) #good for r2 value
plot(team_model_2)
#check each parameter for significance
Anova(team_model_2, type = "III")
#or compare models
anova(team_model_2, team_model_1) # does not appear to be different

#we could do one by one,but we have a lot of variables
#instead, we can use top-down or bottom-up approaches to compare

#top-down approach####
#for top-down, start with a model containing all variables you may wish to include
#and interactions.  You should ensure these are not highly correlated before including
#pairs let you visualize this.let's try continent, shannon diversity, phylogenetic 
#diveristy, a few rao q measures, rainfall, and elevation
#
#for ease just pull those out (not required, just for viewing here)
team_potential <- team[,colnames(team) %in% c(#site specific
  "Continent",
  #diversity
  "shannon", "wd.RaoQ", "maxdbh.RaoQ", "CWM.wd", "CWM.maxdbh", "PD",
  #environmental
  "Precip_mean.mm", "Elevation",
  #outcome
  "PlotCarbon.tonnes")]

pairs(team_potential)
#modification gives you r2 to compare, size of text shows how correlated they are,
#and asterisks indicate level of significance using bivariate relationships
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
pairs(team_potential, lower.panel=panel.smooth, upper.panel=panel.cor)
#some significant relationships, and shannon and pd are highly correlated...may 
#not be the best idea, but let's continue on

#. means all other variables
team_model_full <- lm(PlotCarbon.tonnes ~ ., 
                      team_potential)
drop1(team_model_full)





#but we need to consider mixed model here
require(lme4)
team_elevation_mm <- lmer (PlotCarbon.tonnes ~ Elevation + (1|Site.Name), team)
summary(team_elevation_mm)
#lets cheat a little to speed this up
variables <- c("Continent", "Country", "Precip_mean.mm", "Precip_CV", "Elevation",
               "Number.of.Tree.Genus", "shannon","wd.RaoQ", "maxdbh.RaoQ", "CWM.wd",
               "CWM.maxdbh", "PD")
#make a chart to fill
p_values <- data.frame("variable"=rep(NA,length(variables)), "p"=NA, "AIC"=NA, "coeff" = NA)
for (i in 1:length(variables)){
  p_values$variable[i]=variables[i]
  #need to handle NA's that vary throughout
  team_na <- na.omit(team[,c("Site.Name","PlotCarbon.tonnes",  variables[i])])
  fit_mm <- lmer(PlotCarbon.tonnes ~ team_na[,names(team_na) == variables[i]] + (1|Site.Name), team_na)
  fit_under <- lmer(PlotCarbon.tonnes ~ (1|Site.Name), team_na)
  p_values$p[i] <- anova(fit_mm, fit_under)$Pr[2]
  #just to show
  p_values$AIC[i] <- anova(fit_mm, fit_under)$AIC[2]
}
head(p_values)
#we can even order p_values by significance
p_values <- p_values[order(p_values$p),]
#point is, we can do single bivariate relationship(you should obviously check
#assumptions for all of these)

#with multivariate regression we can check all at once
#consider relationship among variables first to worry about relationships
#use code below to get pairs and correlations at same time
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  
  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  
  #text(0.5, 0.5, txt, cex = cex * r)
  text(0.5, 0.5, txt)
  text(.8, .8, Signif, cex=cex, col=2)
}


pairs(team[, names(team) %in% c(variables, "PlotCarbon.tonnes")], lower.panel=panel.smooth,
      upper.panel=panel.cor)
#these look ok, but "PlotBiomass.kg" is too closely related to carbon, CWM.dbh is repated, and lat
#and long and Evapoptranspiration have some NAs. lat and long should be caught by continent,
#so we'll remove those and country. na.omit will get rid of empty rows so we can keep
#evapotranspiration, which I want to incluce

#lets us automated procedure to find best subset of these
team_full <- team[,names(team) %in% c(variables, "PlotCarbon.tonnes", "Site.Name")]
team_full <- na.omit(team_full[, names(team_full) %!in% c("Latitude", "Longitude",
                                                          "Country", "maxdbh.maxdbh")])
require(MuMIn)
options(na.action = "na.fail")
team_full_mm <- lmer(PlotCarbon.tonnes ~ (1|Site.Name) + ., team_full)
#won't run due to singularities...let's try dropping annual_evapotranspiration
team_full_mm <- lmer(PlotCarbon.tonnes ~ (1|Site.Name) + . - Annual_Evapotranspiration, team_full)
#example of needing to use biology to make this work. what should we really include?
team_full_mm <- lmer(PlotCarbon.tonnes ~ (1|Site.Name) + Elevation + Precip_mean.mm + shannon +
                       Number.of.Tree.Genus + wd.RaoQ + CWM.wd + CWM.maxdbh + PD, team_full)

auto <- dredge(team_full_mm)
#write to csv to observe if needed, its sorted by AICc values so top line is bst model
#still should check assumptions
write.csv(auto, "dredge_output.csv", row.names = F)




