###########################################
#Interpolation

library(lubridate)
library(tidyverse)
library(vegan)
library(sf)
if (!require("rspatial")) devtools::install_github('rspatial/rspatial')
library(rspatial)
library( spgwr )

library(gstat)

#load the data
temps = readRDS("Temp_filtered (1).rds")


#first calculate the daily means, mins, and maxes
tempminmax = temps %>%
  filter(Date > as.Date("2000-1-1"), Station != "RYF") %>%
  mutate(julian = yday(Date)) %>%
  group_by(Station, julian) %>%
  summarize(Temp = mean(Temp, na.rm = T), Tempmax = max(Temp, na.rm = T),
            Tempmin = min(Temp, na.rm = T))

#read in shapefile of the delta
delta = read_sf("deltashap/hydro_delta_marsh.shp")

#add lat/longs for the stations
stas = read.csv("StationLatLongs.csv")


#attached lat/longs to mean temperature
temp2 = merge(tempminmax, stas)

#Specify a coordinate reference system and turn it into a spatial object
alb <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#Filter the Delta shapefile so it doesn't include areas where we dont have a lot of data
delta2 = sf::as_Spatial(dplyr::filter(delta, HNAME != "SAN FRANCISCO BAY", HNAME != "SAN PABLO BAY"))

#I'm not positive we have to do this transformation, but it was in teh example so I'm doing it.
ctst <- spTransform(delta2, alb)

# just look at summer temps
temp2.1 = dplyr::filter(temp2, julian < 280, julian >180)
sp = temp2.1
coordinates(sp) = ~ Longitude + Latitude
crs(sp) <- "+proj=longlat +datum=NAD83"
spt <- spTransform(sp, alb)

#Raster of the Delta
r <- raster(ctst,  res=100)
vr <- rasterize(vca, r, 'prec')

gs <- gstat(formula=Tempmax~1, locations=spt)
idw <- interpolate(r, gs)
## [inverse distance weighted interpolation]
idwr <- mask(idw, ctst)
plot(idwr,  xlim = c(-200000, -100000), ylim = c(-50000,50000))
