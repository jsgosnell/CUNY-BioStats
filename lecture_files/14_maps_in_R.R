#mapping
#natural earth gis layers and ggplot2####

#follows diverstiy calculations from lecture 13
#team analysis####

team_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRAlo2kFyYSY32mn4G1EuoUntKMCLnUfslx7Zcqq8rFXMT8BX-liEItjbvYptaQ5xztVC4O0FFZZVxP/pub?gid=442140500&single=true&output=csv")

#pull out years we used for analysis
site_info_most_recent_year <- team_data[is.na(team_data$Latitude) != T, ]

library(rnaturalearth)
sp::plot(ne_countries(type = 'countries', scale = 'small'))
library(rnaturalearthdata)
sp::plot(ne_coastline())


#see data
base_map <- ne_countries(type = 'countries', scale = 'small')
base_map@data

library(ggplot2)
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

library(reshape2)
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
#from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
library("maps")
library("sf")
library("rgeos")
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states) #need to make upper case
#from https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
states$ID <- CapStr(states$ID)
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)
head(states)
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 5) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)+
  xlab("Longitude")+
  ylab("Latitude") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))
#see rest of tutorial for nudging, etc

#google earth example#####
#fig 1, color, text on####
#see https://github.com/dkahle/ggmap
#https://stackoverflow.com/questions/49351360/how-to-plot-specific-state-using-ggmap-and-ggplot-packages-in-r
#http://rstudio-pubs-static.s3.amazonaws.com/175678_754df2122024463a89d9ebedf277714e.html
#may need https://stackoverflow.com/questions/45698588/revisiting-the-format-latitude-and-longitude-axis-labels-in-ggplot/45948668
#packages####
library(ggplot2)
library(ggmap)
library(scales)

#load sites and organize####
#this is from https://docs.google.com/spreadsheets/d/1MJYr-2Q3-pz-Q21MxYC-nYQHzP9iU-pZ_ySpu1M-oQo/edit?usp=sharing
sites=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS2q9XoX5U06r_bZ6T9_B615r-nmxbUyxv7ihv0FAm1DtUOFomd6TIY7GLAGhMLx3Re7DL3m5E7_uJf/pub?gid=1365968743&single=true&output=csv",
               strip.white = T)

#get general coordinate for each location and each site
#used for sites with boundaries or multiple points, but ok to keep
library(reshape)
sitesmelted=melt(sites, id.var=c("Location", "Site", "Color"), measure.vars=c("Latitude", "Longitude"))
bylocation=cast(sitesmelted, Location+Site + Color~variable, mean)
bysite=cast(sitesmelted, Location + Color ~variable, mean)

#use sites to get main area####
#need google api key
register_google(key = "NEED TO INPUT!")
map <- get_map(location =c(lon = mean(bysite$Longitude), lat = mean(bysite$Latitude)),
               zoom = 12, maptype = "satellite")
#toner is another option
ggmap(map)+ 
  # xlab(expression(paste("Longitude ", ( degree~W)))) +
  # ylab(expression(paste("Latitude ", ( degree~N)))) +
  xlab(expression(paste("Longitude"))) +
  ylab(expression(paste("Latitude"))) +
  geom_point(data = bysite, 
             aes(x=Longitude, y=Latitude, group = NA, 
                 color = Location), 
             shape = 21, size = 5, stroke = 1.5) +
  theme(axis.title.x = element_text(size=22), 
        axis.title.y = element_text(size=22), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.position="none",
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey")) +
  #scale_x_continous (labels=abs) to get rid of negatives
  # scale_x_continuous(labels=abs) +
  scale_color_manual(values = c("white", "yellow")) +
  annotate("text", label="Yellow Bar Hassock", x=-73.85, y=40.59, size=10, 
           #          color =  hue_pal()(2)[2]) +
           color = "yellow") +
  annotate("text", label="Black Bar Marsh", x=-73.82, y=40.64, size=10, 
           #           color =  hue_pal()(2)[1]),
           color = "white") +
  scaleBar(lon = -73.8, lat = 40.5, distanceLon = 5,
           distanceLat = 5, distanceLegend = 5.5, dist.unit = "km",
           arrow.length=0, arrow.distance=0, legend.size=5, arrow.North.size=5, 
           arrow.size=1.25, legend.colour = "white")

#use ggsave("Fig 1, color.jpg", width = 8.11, height = 5.02, units = "in")
