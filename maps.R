library(ggmap)
library(deltamapr)
library(sf)
library(tidyverse)

regions = st_read("RosieRegions/shpExport.shp")
regions$label = c( "Marsh", "Suisun Bay","Sac", "north", "Far north", "SJ", "South")
djfmp = read.csv("DJFMP_Site_Locations.csv")
moss =   filter(djfmp, Location == "Mossdale")
mosssf = st_as_sf(moss,coords = c("Longitude_location","Latitude_location"), 
                   crs = 4326)

ggplot()+geom_sf(data = regions, aes(fill = label), alpha = 0.5)+
  geom_sf(data = WW_Delta)+geom_sf(data = mosssf)+
  geom_sf_text(data = regions, aes(label = label), nudge_y= .05)+
  geom_sf(data = mosssf)+
  geom_sf_label(data = mosssf, label = "Mossdale", nudge_y = -.05)
load("spatialdata.RData")
