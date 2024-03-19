################################################################
# Title: Making the site map
# Author: Lydia Walton
# Last updated: 1-March-2024
################################################################

dev.off() #Shut down open graphics devices
rm(list = ls(all=T)) #Clear environment

#Setting your working directory
library(here) 
#Data manipulation and analysis
library(dplyr) 
library(tidyr)
#Mapping and visualization
library(ggplot2)
library(ggrepel)
library(ggmap)
library(sf)
library(raster)
library(maps) 
library(mapdata)
library(maptools)
library(rgeos)
library(rgdal)
library(ggsn)
library(PBSmapping)
library(RColorBrewer)
library(viridis)
library(htmlwidgets)
library(plotly)
library(ggsn)


#The code for the base map was adapted from https://hecate.hakai.org/rguide/mapping-in-r.html#site-maps

# Hakai data for creating a base map
hakai <- readOGR("Shapefiles/COAST_TEST2.shp")

str(hakai)

data(nepacLLhigh)

# crop map to show the west coast of Canada
ggplot() +
  geom_polygon(data = nepacLLhigh, aes(x = X, y = Y, group = PID),
               col = "black", fill = "grey75", lwd = 0.01) +
  coord_map(xlim = c(-134, -122), ylim = c(47.5, 55)) +
  theme_classic()

#------Adding our sites to the base map
myctsitedata <- read.csv("Data/myct_mapdata.csv")

myctsitedata$Exact.Site <- factor(myctsitedata$Exact.Site, levels = c("Set 1", "Set 2", 
                                                                      "Set 3", "Set 4", "Set 12", "Set 13", "Set 14","Set 15","Set 17", "Set 18", "Set 19", "Set 20", "Set 21"))

myctsitedata$Year <- as.factor(myctsitedata$Year)


#Create a jitter object for overlapping points that is reproducible
jitter <- position_jitter(width=0.1, height = 0.1)

Site_Map <- ggplot() +
  geom_polygon(data = nepacLLhigh, aes(x = X, y = Y, group = PID),
               col = "black", fill = "grey75", lwd = 0.01) +
  coord_map(xlim = c(-134, -122), ylim = c(47.5, 55)) +
  theme_classic() +
  geom_point(data=myctsitedata, aes(x= Lon, y = Lat, color = Year), 
             size = 2.5, alpha = 0.8, position = jitter) +
  scale_color_manual(values = c("#62BBC1","#EC058E"))+
  xlab("Longitude") +
  ylab("Latitude") +
  labs(color = "Year") +
  #theme(legend.position = "bottom")
  
  #Add north arrow to the plot
  geom_segment(arrow = arrow(length = unit(0.2, "cm")),
               aes(x = -122.5, xend = -122.5, y = 54, yend = 54.75), linewidth = 0.7) +
  annotate("text", x = -122.5, y = 55, label = "N",
           fontface = "bold", size= 3.5) +
  
  #Add scale bar to plot
  annotate("rect", xmin = -134, xmax = -132, ymin = 47.5, ymax = 47.75, 
           col = "black", fill = "black", size = 0.1) +
  annotate("rect", xmin = -132, xmax = -130, ymin = 47.5, ymax = 47.75, 
           col = "black", fill = "white", size = 0.1) +
  
  annotate("text", x = -134, y = 48, label = "0", size = 2.5) +
  annotate("text", x = -132, y = 48, label = "150", size = 2.5) +
  annotate("text", x = -130, y = 48, label = "300", size = 2.5) +
  annotate("text", x = -132, y = 47.35, label = "Kilometres", size = 2.5) 

Site_Map

#Save map
 tiff("Figures/Fig.1_Site_Map.tiff", width=10, height = 12, units = "in", pointsize = "14", compression = "none", type = "cairo", res = 300)
 Site_Map
 dev.off()





