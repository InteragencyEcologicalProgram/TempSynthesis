stations <- read.csv("Data/StationsMetadata.csv")
stations$StartDateDataset <- lubridate::mdy(stations$StartDateDataset)
stations$Year <- factor(lubridate::year(stations$StartDateDataset))
library(sf)
library(leaflet)
stations_sf <- stations %>%
  sf::st_as_sf(coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)
 str(stations_sf)
tempstations <- mapview::mapView(stations_sf, zcol = "Year", label = "Station")

library(mapview)
mapview::mapshot(tempstations, file = "stationmapview.html")

leaflet:(stations_sf)
leaflet::leaflet(stations_sf) %>%
  addTiles() %>%
  addMarkers(~Longitude, ~Latitude, label = ~Station,
             )

daily <- readRDS("Data/tempDaily_20210811.rds")
northDaily <- filter(daily, Region %in% c("Far North", "North Delta", "Sac River"))
ggplot(northDaily, aes(x = Date, y = meanDaily, color = Station)) + geom_line()
