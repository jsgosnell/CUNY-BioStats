#mapping
#natural earth gis layers and ggplot2####

#follows diverstiy calculations from lecture 13
#team analysis####

team_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRAlo2kFyYSY32mn4G1EuoUntKMCLnUfslx7Zcqq8rFXMT8BX-liEItjbvYptaQ5xztVC4O0FFZZVxP/pub?gid=442140500&single=true&output=csv")

#pull out years we used for analysis
site_info_most_recent_year <- team_data[is.na(team_data$Latitude) != T, ]

library(rnaturalearth)
library(rnaturalearthdata)

#see data
base_map <- ne_countries(type = 'countries', scale = 'small')
base_map@data

library(ggplot2)
base_map_fortified=fortify(base_map)
names(base_map_fortified)[names(base_map_fortified) == "long"] <- "Longitude"
names(base_map_fortified)[names(base_map_fortified) == "lat"] <- "Latitude"


#input collection points####
ggplot(data=base_map_fortified,aes(x=Longitude,y=Latitude, group=group)) +
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
mean_pd <- dcast(site_info_most_recent_year, Longitude + Latitude ~ "mean_PD",
                       value.var = "PD", mean)

#plot
ggplot(data=base_map_fortified,aes(x=Longitude,y=Latitude, group=group)) +
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
  geom_point(data = mean_pd, 
             aes_string(x="Longitude",y="Latitude", group = NA, color = "mean_PD"), size = 10) +
  scale_color_continuous(guide = guide_legend(title = "mean_PD"))


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

#load site data and organize####
#this is from https://docs.google.com/spreadsheets/d/1D465AIAPKuIZEyVDn5MhlBB-P2zHTRPWuzYCAnomjzI/edit?usp=sharing
sites=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS7CXuBxXwCPze3uRl7mj6ItDnDbzbLRSkwhzZ2bw0CGa_hLd28UD7RR7mZ11Q-NiPwl8GJqiF_WkJh/pub?gid=0&single=true&output=csv",
               strip.white = T)


#use sites to get main area####
#need google api key
register_google(key = "NEED TO INPUT!")
map <- get_map(location =c(lon = mean(sites$Longitude), 
                           lat = mean(sites$Latitude)),
               zoom = 13, maptype = "satellite")

map <- get_map(location =c(lon = mean(bysite$Longitude), lat = mean(bysite$Latitude)),
               ,zoom = 12, maptype = "satellite")
ggmap(map)+ 
  xlab(expression(paste("Longitude ", ( degree~W)))) +
  ylab(expression(paste("Latitude ", ( degree~N)))) +
  geom_point(data = sites, 
             aes(x=Longitude, y=Latitude,  color = Site), 
             shape = 21, size = 5, stroke = 1.5) +
  theme(axis.title.x = element_text(size=22), 
        axis.title.y = element_text(size=22), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey")) 