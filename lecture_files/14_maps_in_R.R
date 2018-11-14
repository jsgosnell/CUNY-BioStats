#mapping
#using natural earth gis layers and ggplot2

#follows diverstiy calculations from lecture 13
#team analysis####
#download diversity_files folder from dropbox link and place files on desktop (or other working directory)
#https://www.dropbox.com/sh/ub64q6nidfa11mf/AAB8GRc8qOIlKy_iHKn2E-dJa?dl=0
#or redirect path below 

team_data <- read.csv("tree_data_complete.csv")

#pull out years we used for analysis
site_info_most_recent_year <- team_data[is.na(team_data$Latitude) != T, ]

require(rgdal)
#redrect path below to where you stored natural earth data
base_map <- readOGR(paste("C:/Dropbox/Stephen/Science tools/GIS from natural earth/land", sep="/"),
                    layer="ne_10m_land")
class(base_map)
plot(base_map)


#or
# require(rnaturalearth)
# base_map <- countries110[,c("scalerank", "featurecla")]
# class(base_map)
# plot(base_map)
# 
# #or 
# coasts <- ne_coastline(scale = 110, returnclass = c("sp", "sf"))
# base_map <- coasts
# plot(base_map)

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

#plot
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

#other map examples####
state_map <- readOGR(paste("C:/Dropbox/Stephen/Science tools/GIS from natural earth/state boundaries", sep="/"), 
                     layer="ne_10m_admin_1_states_provinces") 
plot(state_map)
head(state_map@data)
state_map_fortified=fortify(state_map)
names(state_map_fortified)[names(state_map_fortified) == "long"] <- "Longitude"
names(state_map_fortified)[names(state_map_fortified) == "lat"] <- "Latitude"

#get names and place in center of each unit
idList <- state_map@data$name
admin <- state_map@data$admin
centroids <- as.data.frame(coordinates(state_map))
label_centroids <- data.frame(name = idList, admin = admin, centroids)
label_centroids <- label_centroids[label_centroids$admin == "United States of America",]
names(label_centroids)[names(label_centroids) == "V1"] <- "Longitude"
names(label_centroids)[names(label_centroids) == "V2"] <- "Latitude"
#labels and using coord_fixed to zoom

ggplot(data=state_map_fortified,aes_string(x="Longitude",y="Latitude", group="group")) +
  geom_polygon(colour="black", fill="white") +
  coord_fixed(xlim=c(-125,-70), ylim = c(25,50)) +
  geom_text(data = label_centroids, 
            aes_string(x = "Longitude", y = "Latitude", label="name", group = NA)) +
  xlab("Longitude")+
  ylab("Latitude") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
