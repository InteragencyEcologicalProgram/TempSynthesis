#Let's try and do wetland area for the 2019 fish


library(lubridate)
library(tidyverse)
library(sf)
library(rspatial)
library( spgwr )
library(ggmap)
library(gstat)
library(rgeos)
library(raster)


#read in shapefile of tidal wetlands in the Delta
delta = read_sf("Deltawetlands.shp")
deltawet = filter(delta, Habitat_Ty == "tidal freshwater emergent wetland" )

#shapefile of tidal wetland area in Suisun
suisun =  read_sf("Suisunwetlands.shp")
suisunwet = filter(suisun, clicklabel %in% c("Estuarine Saline Natural Intertidal Emergent" ,
                                             "Estuarine Saline Natural Intertidal Non-vegetated",
                                             "Riverine Natural Vegetated"))

ggplot() + geom_sf(data = suisunwet) + geom_sf(data = deltawet) 

delta2wet = sf::as_Spatial(deltawet)
suisun2wet = sf::as_Spatial(suisunwet)

#Import EDSM data
library(readxl)
EDSM_2019 <- read_excel("EDSM 2019 Delta Smelt Survey Data 2019-11-18.xlsx", 
                        sheet = "data")

#alb <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

EDSM = EDSM_2019

EDSMs = st_as_sf(EDSM,
                 coords = c("TargetLong", "TargetLat"),
                 crs = 4326)
EDSMs2 = as_Spatial(EDSMs)

ggplot() + geom_sf(data = suisunwet) + geom_sf(data = deltawet) + geom_sf(data =EDSMs)


goodcrs =  CRS("+init=epsg:4326")
delta3wet = spTransform(delta2wet, goodcrs)
suisun3wet = spTransform(suisun2wet, goodcrs)%>%
  gBuffer(byid=TRUE, width=0)

EDSMbuff = gBuffer(EDSMs2, width = .018, byid = TRUE)
plot(EDSMbuff)

plot(delta3wet)
plot(suisun3wet, add = T)
plot(EDSMbuff, add = T)

#intersection

deltaX = intersect(EDSMbuff, delta3wet)
suisunX = intersect(EDSMbuff, suisun3wet)
deltaXdf = deltaX@data %>%
  group_by(SampleID) %>%
  summarize(wetareaD = sum(Shape_Area))

suisunXdf = suisunX@data %>%
  group_by(SampleID) %>%
  summarize(wetareaS = sum(Shape_Area))

EDSM2 = left_join(EDSM, deltaXdf) %>%
  left_join(suisunXdf)

EDSM2[, "Wetarea"] <- apply(EDSM2[, c("wetareaD", "wetareaS")], 1, max, na.rm = T)
EDSM2$Wetarea[which(is.na(EDSM2$Wetarea))] = 0
EDSM2$wetareaD = NULL
EDSM2$wetareaS = NULL
 write.csv(EDSM2, "EDSM2019wetlands.csv")
 