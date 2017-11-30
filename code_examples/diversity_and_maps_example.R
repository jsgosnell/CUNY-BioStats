#example code to work with diversity data and map it
#pull all diversity files to your desktop and set working directory there
setwd("C:/Users/Stephen/Desktop")
#notes on data manipulation####
#long to wide
cholesterol <- read.table("http://www.statsci.org/data/general/cholestg.txt", header = T)
cholesterol$day <- as.factor(cholesterol$day)
head(cholesterol)
require(reshape2)
#formula gives row ~ columns
#get daily cholesterol for every patient
cholesterol_wide <- dcast(data = cholesterol, formula = patient ~ day, 
                          value.var ="cholest" )
head(cholesterol_wide)
#use fun.aggregate to get other option
#get average cholesterol per patient
#simple function to exclude na's (other option is to melt first and then drop them)
meannona <- function (x) mean(x, na.rm=T)
cholesterol_wide <- dcast(data = cholesterol, formula = patient ~ ., #. means no variable, ... means all variables
                          value.var ="cholest", fun.aggregate = meannona )
head(cholesterol_wide)

#can also name output column by putting in quotes in formula
cholesterol_wide <- dcast(data = cholesterol, formula = patient ~ "cholest", #. means no variable, ... means all variables
                          value.var ="cholest", fun.aggregate = meannona )
head(cholesterol_wide)

#wide to long
cholesterol <- read.table("http://www.statsci.org/data/general/cholestr.txt", header = T)
head(cholesterol)

#id.vars lists independent values to keep
#measure.vars is what you are measuring (not used here, and used rarely)
#variable.name lists what to label things you group
#value.name gives name to value output
cholesterol_long <- melt(cholesterol, id.vars =c())
head(cholesterol_long)

#name outcomes
cholesterol_long <- melt(cholesterol, id.vars =c(), variable.name = "day", 
                         value.name = "cholesterol")
head(cholesterol_long)

#more id variables
sport <- read.csv("https://sites.google.com/site/stephengosnell/teaching-resources/datasets/sport.csv?attredirects=0&d=1")
sport_melted <- melt(sport, id.vars = c("Sex", "Sport"),
                     variable.name = "measure", 
                     value.name = "value")
head(sport_melted)

#team analysis####
tree_data <- read.csv("tree_data_clean_wd_dbh.csv", 
                      na.strings=c("NA","-","", "NULL", "null", "Null", "Unknown", "unknown", 
                               "unidentifiable", "Unidentifiable"), 
                      strip.white = T)

#check data
head(tree_data)
str(tree_data) 
summary(tree_data)
#note I had previously cleaned all names up for mis-spellings and here I remove all 
#unknowns (perfect world scenario)

#make wide dataframe
tree_data_wide=dcast(tree_data, Site.Name+X1ha.Plot.Number+Sampling.Period~Family_Genus, length)
#check this out
tree_data_wide[1:3,1:10]

#make other datafames
#only feed diversityresult the species columns
names(tree_data_wide)[1:15]
abundance_data <- tree_data_wide[,4:ncol(tree_data_wide)]
#and make site_info dataframe
site_info <- tree_data_wide[,1:3]

#get biodiversity measures####
#taxonomic####
require(BiodiversityR)
richness <- diversityresult(abundance_data, method="each site", index = "richness")
site_info$richness=richness$richness

berger <- diversityresult(abundance_data, method="each site", index = "Berger")
site_info$berger=berger$Berger

simpson <- diversityresult(abundance_data, method="each site", index = "Simpson")
site_info$simpson=simpson$Simpson

inv_simpson <- diversityresult(abundance_data, method="each site", index = "inverseSimpson")
site_info$berger=inv_simpson$inverseSimpson

shannon <- diversityresult(abundance_data, method="each site", index = "Shannon")
site_info$shannon=shannon$Shannon

evenness <- diversityresult(abundance_data, method="each site", index = "Jevenness")
site_info$jevenness=evenness$Jevenness

#functional diversity measures####
require(FD)
#dbFD functions requires matrices that match order for species abundance/traits
# and that have rownanes for sites and colnames for species

#make objects for FD use
#requires trait matrix (just trait info) and abundance info (just numbers) with row names matching
#for species
#make each objec
#first make as dataframe
#for wd 
wd_trait <- aggregate(wd~Family_Genus, tree_data, mean)
head(wd_trait)

#make sure taxa are in same order
names(abundance_data)
names(wd_trait)
all.equal(wd_trait$Family_Genus, names(abundance_data),
          check.attributes = F)

#now make matrix without extra columns and with rownames
#remove extra columsn (you just want species)
abundance_data_matrix <- as.matrix(abundance_data)
rownames(abundance_data_matrix) <- paste(tree_data_wide$Site.Name, tree_data_wide$X1ha.Plot.Number, 
                                         tree_data_wide$Sampling.Period, sep = "_")
colnames(abundance_data_matrix) = names(abundance_data)
wd_trait_matrix <- as.matrix(wd_trait["wd"])
rownames(wd_trait_matrix) <- wd_trait$Family_Genus

#do the calculations
calc_wd_fd <- dbFD(wd_trait_matrix,abundance_data_matrix,calc.FRic = T, calc.CWM = T, calc.FDiv = T)

calc_wd_fd=as.data.frame(calc_wd_fd)
names(calc_wd_fd)
#add wd (or trait you are working with) in front as needed
for (i in 1:length(names(calc_wd_fd))){
names(calc_wd_fd)[i] <- paste("wd",names(calc_wd_fd)[i], sep=".")
}
#pull rownames back out for use in merge
location_info <-colsplit(rownames(calc_wd_fd), pattern = "_", names = c("Site.Name",
                                                                      "X1ha.Plot.Number",
                                                                      "Sampling.Period"))
calc_wd_fd <- cbind (location_info, calc_wd_fd)
site_info <- merge(site_info,calc_wd_fd, all.x=T, all.y=T)

#phylogenetic diversity####
#make tree with genus
genus_matrix <- dcast(tree_data, Site.Name+X1ha.Plot.Number+Sampling.Period~Genus,
                     length)
#for traits, use
trait_matrix_genus <- aggregate(cbind(wd, max_dbh)~ Genus + Family, tree_data, mean)

require(brranching)
tree_genus <- phylomatic (taxa=tolower(as.character(trait_matrix_genus$Genus)), storedtree='R20120829', get='POST')
plot(tree_genus, no.margin = T)
write.tree(tree_genus, file="TEAMtreeonlygenus.tre")

#age tree####
#code below will age tree
#or
#to compare, write otu and then send to phylomatic
#can't use website, must use desktop program
#see https://greggilbertlab.sites.ucsc.edu/wp-content/uploads/sites/276/2015/10/R_Class10b_PicatePhylomatic.pdf
#what I did
#downloaded phylocom
#http://phylodiversity.net/phylocom/
#extracted
#made a new folder
#put in 
  #phylocom (exe file)
  #phylomatic (exe file)
  #wikistrom.ages (ages file)
  #copied wikistromages and renamed ages
  #R20100701.new (supertree)
  
#my output file
# mycleanoutfile<- paste("phylocom_calculations/","taxafile",".clean.new",sep="")
# #dated newick file

setwd("phylocom_calculations") #set the working directory to the phylocom folder

#run models
#my genus list
write.table(paste(tolower(trait_matrix_genus$Family), tolower(trait_matrix_genus$Genus), sep="/"),
            "taxa_for_phylomatic.txt", row.names = F, quote = F, col.names = F)
#extract a tree from the mastertree
system(paste(getwd(), "/phylomatic -f ", "R20100701.new"," -t ","taxa_for_phylomatic.txt", " > ","taxafile.new",sep="")) 

#cleanphy to remove one-daughter nodes
system(paste(getwd(),"/phylocom cleanphy -f ", "taxafile.new"," -e > ","taxafile_clean.new",sep="")) 

#date the nodes using Wikstrom dates
system(paste(getwd(),"/phylocom bladj -f ", "taxafile_clean.new"," > ",
             "taxafile_dated.new",sep="")) 

#read in dated newick file
tree_genus_dated <- read.tree("taxafile_dated.new")

#return to main working directory
setwd("C:/Users/Stephen/Desktop")

##below code only needs to be run once to install ggtree from bioconductor repository
source("http://bioconductor.org/biocLite.R") # downloads the latest bioconductor script/repos
biocLite("ggtree")

require(ggtree)
#make circular tree
circular_cladogram <- ggtree(tree_genus_dated, layout="circular", branch.length="none") +
  ggtitle("Cladogram  of 982  trees from TEAM dataset")
circular_cladogram + geom_tiplab(size=3, color="blue")

#prune tree
require(picante)

#clean up genus_matrix by makign rownames and removing extraneous columns
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0

rownames(genus_matrix) <- paste(genus_matrix$X1ha.Plot.Number,
                                genus_matrix$Sampling.Period)
genus_matrix_clean <- genus_matrix[, names(genus_matrix)
                                    %!in% c("X1ha.Plot.Number",
                                            "Sampling.Period",
                                            "Site.Name")]
names(genus_matrix_clean) <- tolower(names(genus_matrix_clean))
#remove species not found in tree; pd will run ohterwise but only counts toward
#species richness; mpd function will not run
genus_matrix_clean_in_tree <- genus_matrix_clean[,names(genus_matrix_clean) %in%
                                                   tree_genus_dated$tip.label]


#make tree with only known species
tree_genus_dated_pruned <- prune.sample (genus_matrix_clean_in_tree, tree_genus_dated)

#faith's phylogenetic diversity
TEAM_pd <- pd(genus_matrix_clean_in_tree, tree_genus_dated_pruned)
TEAM_pd$siteyear <- rownames(TEAM_pd)

#mean pairwise distance
TEAM_distance <- cophenetic.phylo(tree_genus_dated_pruned)
TEAM_mpd <- mpd(na.omit(genus_matrix_clean_in_tree), TEAM_distance)
TEAM_mpd <- as.data.frame(TEAM_mpd)
TEAM_mpd$siteyear <- rownames(genus_matrix_clean_in_tree)

#mean pairwise distance with abundance
TEAM_mpd_with_abundance <- mpd(na.omit(genus_matrix_clean_in_tree), TEAM_distance, abundance.weighted = T)
TEAM_mpd_with_abundance <- as.data.frame(TEAM_mpd_with_abundance)
TEAM_mpd_with_abundance$siteyear <- rownames(genus_matrix_clean_in_tree)

#standardized effect score of mpd (equal to -1 * nri, net relatedness index)
# TEAM_nri <- ses.mpd(na.omit(genus_matrix_clean_in_tree), TEAM_distance)
# TEAM_nri <- as.data.frame(TEAM_nri)
# TEAM_nri$siteyear <- rownames(genus_matrix_clean_in_tree)

#core ancestor cost
# require(PhyloMeasures)
# TEAM_cac <- cac.query(tree = tree_genus_dated_pruned, matrix = genus_matrix_clean_in_tree,
#                       chi = .9)
# TEAM_cac <- as.data.frame(TEAM_cac)
# TEAM_cac$siteyear <- rownames(genus_matrix_clean_in_tree)

#taxonomic distinctiveness
TEAM_td_lists <- taxondive(genus_matrix_clean_in_tree, TEAM_distance, match.force = FALSE)
TEAM_td <- as.data.frame(TEAM_td_lists[1])
for(i in 2:length(TEAM_td_lists)){
  TEAM_td <- cbind(TEAM_td,as.data.frame(TEAM_td_lists[i]))
}
TEAM_td$siteyear <- rownames(genus_matrix_clean_in_tree)

#mntd
TEAM_mntd <- mntd(genus_matrix_clean_in_tree, TEAM_distance)
TEAM_mntd <- as.data.frame(TEAM_mntd)
TEAM_mntd$siteyear <- rownames(genus_matrix_clean_in_tree)

#mntd with abundance
TEAM_mntd_with_abundance <- mntd(genus_matrix_clean_in_tree, TEAM_distance, abundance.weighted = T)
TEAM_mntd_with_abundance <- as.data.frame(TEAM_mntd_with_abundance)
TEAM_mntd_with_abundance$siteyear <- rownames(genus_matrix_clean_in_tree)

#phylogenetic entropy
require(entropart)
#need ultrametic rooted tree and only one row/site at a time
tree_genus_dated_pruned_ultrametric <- multi2di(chronos(tree_genus_dated_pruned))
for(i in 1:dim(TEAM_mntd)[1]){
  TEAM_mntd$phy_ent[i] <- as.numeric(PhyloEntropy(as.SpeciesDistribution(genus_matrix_clean_in_tree[i,]),
                                                  q=1, tree_genus_dated_pruned_ultrametric)$Total)
}

#merge in phylogenetic measures
site_info$siteyear <- paste(site_info$X1ha.Plot.Number,
                            site_info$Sampling.Period)
site_info <- merge(site_info, TEAM_pd, all.x=T)
site_info <- merge(site_info, TEAM_mpd, all.x = T)
site_info <- merge(site_info, TEAM_mpd_with_abundance, all.x = T)
#site_info <- merge(site_info, TEAM_nri, all.x = T)
site_info <- merge(site_info, TEAM_mntd, all.x = T)
site_info <- merge(site_info, TEAM_mntd_with_abundance, all.x = T)
site_info <- merge(site_info, TEAM_td, all.x = T)
#site_info <- merge(site_info, TEAM_cac, all.x = T)

#automate addingin other factors
file="biomassandclimate.csv"
biomass_and_climate=read.csv(file)
site_info <- merge(site_info,biomass_and_climate, all.x = T)

#add in long,lat,evap
file="lat_long_copy.csv"
long_lat_evapotranspiration <- read.csv(file)
site_info <- merge(site_info,long_lat_evapotranspiration,
                   all.x=T)

write.csv(site_info, "tree_data_complete.csv")

#pull out years we used for analysis
site_info_most_recent_year <- site_info[is.na(site_info$Latitude) != T, ]

#mapping
#using natural earth gis layers and ggplot2
require(rgdal)
base_map <- readOGR(paste("C:/Dropbox/Stephen/Science tools/GIS from natural earth/land", sep="/"),
                 layer="ne_10m_land")
class(base_map)
plot(base_map)
#see data
base_map@data

require(ggplot2)
base_map_fortified=fortify(base_map)
names(base_map_fortified)[names(base_map_fortified) == "long"] <- "Longitude"
names(base_map_fortified)[names(base_map_fortified) == "lat"] <- "Latitude"
#Creat a base plot
#coord_fixed keeps scale right (1:1)
#coord_map uses map projections
ggplot(data=base_map_fortified,aes_string(x="Longitude",y="Latitude", group="group")) +
         geom_polygon(colour="black", fill="white") +
         xlab("Longitude")+
         ylab("Latitude") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
       
#input collection points
ggplot(data=base_map_fortified,aes_string(x="Longitude",y="Latitude", group="group")) +
  geom_polygon(colour="black", fill="white") +
  xlab("Longitude")+
  ylab("Latitude") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) + 
  geom_point(data = site_info_most_recent_year, 
             aes_string(x="Longitude",y="Latitude", group = NA), size = 10)

#color collection points by diversity
#get average richness per site

mean_richness <- dcast(site_info_most_recent_year, Longitude + Latitude ~ "Mean_Richness", value.var = "richness", mean)
ggplot(data=base_map_fortified,aes_string(x="Longitude",y="Latitude", group="group")) +
  geom_polygon(colour="black", fill="white") +
  xlab("Longitude")+
  ylab("Latitude") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32)) + 
  geom_point(data = mean_richness, 
             aes_string(x="Longitude",y="Latitude", group = NA, color = "Mean_Richness"), size = 10) +
  scale_color_continuous(guide = guide_legend(title = "Mean richness"))
  
#add code for scale bar