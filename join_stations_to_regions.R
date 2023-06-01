library(sf)
library(readr)


# Join latitude/longitude of all stations with most recent region shapefile 
# (removed Far North)

LatLon <- read.csv(here("Data", "StationsMetadata.csv"))
stations_sf <- st_as_sf(LatLon, coords = c("Longitude", "Latitude"), crs = 4326)
regions <- st_read(here::here("RosiesRegionsEdited/Rosies_regions_edited.shp"))
# regions)= <- st_transform(st_crs(stations_sf))
stations <- st_join(stations_sf, regions)
write_csv(stations, "Data/stations_w_regions_20230601.csv")
