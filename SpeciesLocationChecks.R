###### Use deltafish to check species locations

library(deltafish)
library(sf)
library(leaflet)
# Download data ---------------------------------
create_fish_db()
surv <- open_survey()
fish <- open_fish()

surv_FMWT_DJFMP <- surv %>%
  filter(source %in% c("SKT", "FMWT", "DJFMP", "STN", "TMM", "EDSM", "Suisun", "BayStudy", "SLS"))

fish_filter <- fish %>%
  filter(Taxa %in% c("Oncorhynchus tshawytscha"))

df_salmon <- left_join(surv, fish_filter) %>%
  collect()

# Filter data ------------------------------------
df_salmon2 <- df_salmon  %>%
  filter(Count>0)

hist(df_salmon2$Length) # looks like only juvenile salmon are in here

# Make a map of stations ----------------------------
locations <- df_salmon2 %>%
  select(Survey, Station, Latitude, Longitude) %>%
  distinct() %>%
  filter(!is.na(Latitude))

# make sf if we want to make a static map
locations_sf <- st_as_sf(locations, coords = c("Longitude", "Latitude"), crs = 4326)
regions <- st_read("RosieRegions/shpExport.shp")
regions$Region <- c("Suisun Marsh", "Suisun Bay","Confluence", "North Delta", "Far North", "Central" , "South") 

# But lets just use leaflet
pal <- colorFactor(viridis::viridis(9), locations$Survey)

locations %>% # call data frame.
  leaflet() %>% # call leaflet.
  addTiles() %>% # this adds the map in the background.
  addCircleMarkers(
    color = ~pal(Survey),
    stroke = FALSE, # alters whether there is a border to circle
    fillOpacity = 0.8,
    radius = 1.5,
    lng = ~Longitude, # call your longitude column name
    lat = ~Latitude, # call you latitude column name
    label = ~paste(Station, "Lat:", Latitude, "Long:", Longitude)) %>% # edit what you want to show up in your label
  addLegend(pal = pal,
            values = ~Survey,
            position = "bottomright")
