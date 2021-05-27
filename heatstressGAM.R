#GAMs for the heat stress data

library(lubridate)
library(tidyverse)
library(vegan)
library(sf)
if (!require("rspatial")) devtools::install_github('rspatial/rspatial')
library(rspatial)
library( spgwr )
library(nlme)
library(boot)
library(itsadug)
library(mgcv)
library(raster)
library(stars)

heatDays = read.csv("Data/heatDays.csv")


#read in shapefile of the delta
delta = read_sf("DeltaShapefile/hydro_delta_marsh.shp")

#add lat/longs for the stations
stas = read.csv("StationLatLongs.csv")

#attached lat/longs to mean temperature
heatDays2 = left_join(heatDays, stas) %>%
  rename(Year = fWY) %>%
  ungroup()

heatHigh = filter(heatDays2, Stress == "High")

#Specify a coordinate reference system and turn it into a spatial object
alb <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#model - high heat stress, all years
#maybe use "betar"? Or can I just use a binomial?
hs5 =  gam(propDays ~  
            te(Latitude, Longitude, Year, d = c(2,1), k = c(40, 6)), 
          data =heatHigh, method = "REML", family = "betar")
plot(hs5)
#Hmmm... that's worse...

#maybe use "betar"? Or can I just use a binomial?
hs5b =  gam(cbind(nDays, totalDays) ~  
             te(Latitude, Longitude, Year), 
           data =heatHigh, method = "REML", family = "binomial")
plot(hs5b)


#model - high heat stress, average

heatave = group_by(heatHigh, Station, Region, Latitude, Longitude) %>%
  summarise(propDays = mean(propDays), nDays = round(mean(nDays)), totDays = round(mean(totalDays)))

hs6 =  gam(propDays ~  
             te(Latitude, Longitude), 
           data =heatave, method = "REML", family = "betar")
plot(hs6)

hs6b =  gam(cbind(nDays, totDays) ~  
              te(Latitude, Longitude), 
            data =heatave, method = "REML", family = "binomial")
plot(hs6b)


load("spatialdata.RData")
regions = read_sf("RosieRegions/shpExport.shp")
regions = st_transform(regions, crs = 4326)

stas = read.csv("StationLatLongs.csv")%>%
  filter(Station != "DV7", Station != "RIP", 
         Station != "RPN", Station != "RCS")
#Create a convex hull around the points and buffer by  0.01
#so we aren't creating preditions too far from our sensors. 

ch = chull(stas$Longitude, stas$Latitude)
hull = stas[ch,]
#  coordinates(hull) = ~ Longitude + Latitude
#crs(hull) <- "+proj=longlat +datum=NAD83"
library(sfheaders)
hullp = sf_polygon(
  obj = hull
  , x = "Longitude"
  , y = "Latitude"
)
st_crs(hullp) = 4326

chbuff = st_as_sf(hullp) %>%
  st_buffer(dist = 0.02)


#now turn the stations into a spatial thingy
coordinates(stas) = ~ Longitude + Latitude
crs(stas) <- "+proj=longlat +datum=NAD83"


delta = st_transform(delta,crs=4326)
stas = st_as_sf(stas) %>%
  st_transform(stas, crs=4326) 



HD_pred<-function(Full_data=Data,
                  Delta_subregions = regions,
                  Delta_water=delta,
                  Stations = stas,
                  n=500, 
                  Years=2009:2019) {
  
  # Create point locations on a grid for predictions
  Points<-st_make_grid(Delta_subregions, n=n)%>%
    st_as_sf(crs=st_crs(Delta_subregions))%>%
    st_join(Delta_water)%>% 
    
    # Joining a map of delta waterways (from my spacetools package) to ensure all these points are over water.
    st_coordinates()%>%
    as_tibble()%>%
    mutate(Location=1:nrow(.))%>%
    dplyr::select(Longitude=X, Latitude=Y, Location)
  
  
  # Create full dataset for predictions
  newdata<-expand.grid(Location=1:nrow(Points),
                       Year=Years)%>% # Create all combinations of predictor variables
    left_join(Points, by="Location")%>% #Add Lat/Longs to each location
    mutate(Latitude_s=(Latitude-mean(Full_data$Latitude, na.rm=T))/sd(Full_data$Latitude, na.rm=T), # Standardize each variable based on full dataset for model
           Longitude_s=(Longitude-mean(Full_data$Longitude, na.rm=T))/sd(Full_data$Longitude, na.rm=T)) %>%
    st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=FALSE)%>% # Turn into sf object
    
    st_transform(crs=st_crs(Delta_subregions))%>% # transform to crs of Delta shapefile
    
    st_join(Delta_subregions, join = st_intersects)
  
  return(newdata)
}


newdata_year <- HD_pred(Full_data=heatHigh,
                        Year = 2009:2019)
newdata_ave = HD_pred(Full_data = heatave,
                      Year = 2019)

modellc4_predictions<-predict(hs5, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T) # Create predictions
modellave<-predict(hs6, newdata=newdata_ave, type="response", se.fit=TRUE, discrete=T) # Create predictions

#binomial models
modellc4b<-predict(hs5b, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T) # Create predictions
modellaveb<-predict(hs6b, newdata=newdata_ave, type="response", se.fit=TRUE, discrete=T) # Create predictions


newdataHD<-newdata_year%>%
  mutate(Prediction=modellc4_predictions$fit)%>%
  mutate(SE=modellc4_predictions$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96)


newdataave<-newdata_ave%>%
  mutate(Prediction=modellave$fit)%>%
  mutate(SE=modellave$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96)

newdataaveb<-newdata_ave%>%
  mutate(Prediction=modellaveb$fit)%>%
  mutate(SE=modellaveb$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96) 




# Function to rasterize all dates. Creates a 3D raster Latitude x Longitude x Year
Rasterize_all <- function(data, var, out_crs=4326, n=100){
  var<-rlang::enquo(var)
  rlang::as_name(var)
  preds<-map(unique(data$Year), 
             function(x) st_rasterize(data %>%
             filter(Year==x)%>%
              dplyr::select(!!var), 
               template=st_as_stars(st_bbox(delta2), 
                dx=diff(st_bbox(delta2)[c(1, 3)])/n, 
                dy=diff(st_bbox(delta2)[c(2, 4)])/n, values = NA_real_))%>%
               st_warp(crs=out_crs))
  
  # Then bind all dates together into 1 raster
  out <- exec(c, !!!preds, along=list(Year=unique(data$Year)))
  return(out)
}

# Create full rasterization of all predictions for interactive visualizations
rastered_preds<-Rasterize_all(newdataHD, Prediction)
rastered_preds2 = st_as_stars(rastered_preds, crs = 4326)
# Same for SE
rastered_SE<-Rasterize_all(newdataHD, SE)

# Bind SE and predictions together
rastered_predsSE<-c(rastered_preds, rastered_SE)

deltabuff = st_buffer(delta, dist = 0.005) %>%
  st_intersection(chbuff)


#avergge year raster
rastered_predave = st_rasterize(dplyr::select(newdataave, Prediction), 
                                template=st_as_stars(st_bbox(delta2), 
                                                     dx=diff(st_bbox(delta2)[c(1, 3)])/100, 
                                                     dy=diff(st_bbox(delta2)[c(2, 4)])/100, values = NA_real_))%>%
  st_warp(crs=4326)
  
#binomial model version
rastered_predaveb = st_rasterize(dplyr::select(newdataaveb, Prediction), 
                                template=st_as_stars(st_bbox(delta2), 
                                                     dx=diff(st_bbox(delta2)[c(1, 3)])/500, 
                                                     dy=diff(st_bbox(delta2)[c(2, 4)])/500, values = NA_real_))%>%
  st_warp(crs=4326)

#############################################################################
#plotting functions

raster_plot<-function(data, labels="All"){
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    facet_wrap(~Year)+
   # scale_fill_viridis_c(name="Temperature", na.value="white",
  #                       limits=lims,
   #                      labels= function(x) ifelse((x/2)==as.integer(x/2), as.character(x), ""),
    #                     guide = guide_colorbar(direction="horizontal", title.position = "top", barwidth = 10, ticks.linewidth = 2,
    #                                            barheight=1, title.hjust=0.5, label.position="bottom", label.theme=element_text(size=12), 
    #                                            title.theme=element_text(size=13)))+
    #coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "top")
}

raster_plot2<-function(data, Year, labels="All"){
  data = data[,,,Year]
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    # facet_wrap(~Date)+
    scale_fill_viridis_c(name="Temperature", na.value="white", breaks=seq(6,26,by=1), limits = c(5,30),
                         labels= function(x) ifelse((x/2)==as.integer(x/2), as.character(x), ""),
                         guide = guide_colorbar(direction="horizontal", title.position = "top", barwidth = 10, ticks.linewidth = 2,
                                                barheight=1, title.hjust=0.5, label.position="bottom", label.theme=element_text(size=12), 
                                                title.theme=element_text(size=13)))+
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "top")
}


#percentage heat stress days
raster_plot2(rastered_preds, 2009) + ggtitle("heatstress - 2009")

#I need to figure out how to avoid negative heat stress days
#And why does my model predict heat stress days higher than ever occcured?

raster_plot(rastered_predave) + ggtitle("heatstress")

cropped = st_crop(rastered_predave, deltabuff)
ggplot()+
  geom_stars(data=cropped)+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw() + theme(legend.position = "top")

#binomial version

cropped = st_crop(rastered_predaveb, deltabuff)
ggplot()+
  geom_stars(data=cropped)+
  geom_sf(data = stas, color = "white")+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw() + theme(legend.position = "top")

#look at number of days 

ggplot()+
  geom_stars(data=cropped*365)+
  geom_sf(data = stas, color = "white")+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw() + theme(legend.position = "top")

#TO DO:
#Crop Delta map so nothing is too far from a temp sensor
