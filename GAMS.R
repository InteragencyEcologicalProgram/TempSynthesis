#Let's pull out everything for GAMs

knitr::opts_chunk$set(echo = TRUE)
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


#############################################################################
#let's check and see how long things have been around and how many samples from each month


#load the data
temps = readRDS("Temp_filtered (1).rds")

tempmeanx2 = temps %>%
  filter(Date > as.Date("2010-1-1"), Station != "RYF",
         Station != "DV7", Station != "RPN", Station != "RIP", Station != "RCS") %>%
  mutate(julian = yday(Date), Year = year(Date), Month = month(Date)) %>%
  group_by(Station, julian, Year, Month, Date) %>%
  summarize(Tempave = mean(Temp, na.rm = T), TempMax = max(Temp, na.rm = T), 
            TempMin = min(Temp, na.rm = T), Temprange = TempMax-TempMin, n = length(Temp))

tempmonth = tempmeanx2 %>%
  mutate(monthyear = paste(Year, Month)) %>%
  group_by(Station, Month, Year, monthyear) %>%
  summarize(Tempave = mean(Tempave, na.rm = T), TempMax = max(TempMax, na.rm = T), 
            TempMin = min(TempMin, na.rm = T), Temprange = TempMax-TempMin, n = sum(n))

tempN = tempmonth %>%
  group_by(Station) %>%
  summarize(nx = sum(n)) %>%
  filter(nx >30000)

ggplot(tempmonth) + geom_tile(aes(x = monthyear, y = Station, fill = n))

#Maybe drop anything with less than 30000 samples? Or a higher cutoff?

tempmonth2 = merge(tempmonth, tempN)

ggplot(tempmonth2) + geom_tile(aes(x = monthyear, y = Station, fill = n))

###############################################################################
#load teh data we need to run the models
load("tempmeanx.RData")

#merge it to subset teh stations we want
tempmeanx = merge(tempmeanx, tempN)

#read in shapefile of the delta
delta = read_sf("DeltaShapefile/hydro_delta_marsh.shp")

#add lat/longs for the stations
stas = read.csv("StationLatLongs.csv")

#attached lat/longs to mean temperature
tempmean2 = left_join(tempmeanx, stas) %>%
  mutate(Year = year(Date)) %>%
  arrange(Station, Date) %>%
  ungroup()
tempmean2 = start_event(as.data.frame(tempmean2), column="Date", event=c("Station", "Year"), label.event="Event")

#Specify a coordinate reference system and turn it into a spatial object
alb <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")


#basic model of max temp based on day and location

g5 =  bam(TempMax ~  
            te(Latitude, Longitude, julian, bs = c("cr","cr", "cc")), 
          data =tempmean2, method = "REML")

r5 = acf(resid(g5),  plot=FALSE)$acf[2]
g5 =  bam(TempMax ~  
            te(Latitude, Longitude, julian, bs = c("cr","cr", "cc")), 
          data =tempmean2, method = "REML")

#basic model of mean temp based on day and location

g5ave =  bam(Tempave  ~  
               te(Latitude, Longitude, julian, bs = c("cr","cr", "cc")),
          data =tempmean2, method = "REML")

r5ave = acf(resid(g5ave), plot=FALSE)$acf[2]



g5ave =  bam(Tempave  ~  
               te(Latitude, Longitude, julian, bs = c("cr","cr", "cc")), 
          data =tempmean2, method = "REML",  rho=r5ave, AR.start=tempmean2$start.event)

plot(g5ave)


#basic model of mean temp based on day and location

g5min =  bam(TempMin  ~  
               te(Latitude, Longitude, julian, bs = c("cr","cr", "cc")),
             data =tempmean2, method = "REML")

r5min = acf(resid(g5min), plot=FALSE)$acf[2]



g5min =  bam(TempMin  ~  
               te(Latitude, Longitude, julian, bs = c("cr","cr", "cc")),
             data =tempmean2, method = "REML",  rho=r5min, AR.start=tempmean2$start.event)





#temperature range


#basic model of temperature range based on day and location

g5range =  bam(Temprange  ~  
                 te(Latitude, Longitude, julian, bs = c("cr","cr", "cc")), 
             data =tempmean2, method = "REML")

r5range = acf(resid(g5range), plot=FALSE)$acf[2]



g5range =  bam(Temprange ~  
                 te(Latitude, Longitude, julian, bs = c("cr","cr", "cc")),
             data =tempmean2, method = "REML",  rho=r5range, AR.start=tempmean2$start.event)



load("spatialdata.RData")
regions = read_sf("RosieRegions/shpExport.shp")
regions = st_transform(regions, crs = 4326)

stas = read.csv("StationLatLongs.csv")
coordinates(stas) = ~ Longitude + Latitude
crs(stas) <- "+proj=longlat +datum=NAD83"


delta = st_transform(delta,crs=4326)
stas = st_as_sf(stas) %>%
  st_transform(stas, crs=4326) %>%
  filter(Station != "DV7", Station != "RIP", Station != "RPN", Station != "RCS")

WQ_pred<-function(Full_data=Data,
                  Delta_subregions = regions,
                  Delta_water=delta,
                  Stations = stas,
                  n=500, 
                  Julian_days=yday(ymd(paste("2014", c(1,4,7,10), "15", sep="-"))) #Jan, Apr, Jul, and Oct 15 for a non-leap year
                  
){
  
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
                       Julian_day=Julian_days)%>% # Create all combinations of predictor variables
    left_join(Points, by="Location")%>% #Add Lat/Longs to each location
    mutate(Latitude_s=(Latitude-mean(Full_data$Latitude, na.rm=T))/sd(Full_data$Latitude, na.rm=T), # Standardize each variable based on full dataset for model
           Longitude_s=(Longitude-mean(Full_data$Longitude, na.rm=T))/sd(Full_data$Longitude, na.rm=T),
           Julian_day_s = (Julian_day-mean(Full_data$julian, na.rm=T))/sd(Full_data$julian, na.rm=T)) %>%
    st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=FALSE)%>% # Turn into sf object
    
    st_transform(crs=st_crs(Delta_subregions))%>% # transform to crs of Delta shapefile
    
    st_join(Delta_subregions, join = st_intersects)
  
  return(newdata)
}


newdata_year <- WQ_pred(Full_data=tempmean2, 
                        Julian_days = yday(ymd(paste("2014", 1:12, "15", sep="-"))))

newdata_year = rename(newdata_year, julian = Julian_day)

modellc4_predictions<-predict(g5, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T) # Create predictions
modellave_predictions<-predict(g5ave, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T) # Create predictions
modellminpredictions<-predict(g5min, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T)
modellrange_predictions<-predict(g5range, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T)
#mean temp model
newdataave<-newdata_year%>%
  mutate(Prediction=modellave_predictions$fit)%>%
  mutate(SE=modellave_predictions$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96)


#max temp model
newdata<-newdata_year%>%
  mutate(Prediction=modellc4_predictions$fit)%>%
  mutate(SE=modellc4_predictions$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96) %>%
  filter(Prediction <35, Prediction >0)

#min temp model
newdatamin<-newdata_year%>%
  mutate(Prediction=modellminpredictions$fit)%>%
  mutate(SE=modellminpredictions$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96)

#range temp model
newdatarange<-newdata_year%>%
  mutate(Prediction=modellrange_predictions$fit)%>%
  mutate(SE=modellrange_predictions$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96) %>%
  filter(Prediction <10)

# Function to rasterize all dates. Creates a 3D raster Latitude x Longitude x Date 
Rasterize_all <- function(data, var, out_crs=4326, n=100){
  var<-rlang::enquo(var)
  rlang::as_name(var)
  preds<-map(unique(data$julian), function(x) st_rasterize(data%>%
                                                           filter(julian==x)%>%
                                                           dplyr::select(!!var), 
                                                         template=st_as_stars(st_bbox(delta2), dx=diff(st_bbox(delta2)[c(1, 3)])/n, dy=diff(st_bbox(delta2)[c(2, 4)])/n, values = NA_real_))%>%
               st_warp(crs=out_crs))
  
  # Then bind all dates together into 1 raster
  out <- exec(c, !!!preds, along=list(Date=unique(data$julian)))
  return(out)
}

# Create full rasterization of all predictions for interactive visualizations
rastered_preds<-Rasterize_all(newdata, Prediction)
rastered_predsave = Rasterize_all(newdataave, Prediction)
rastered_predsmin = Rasterize_all(newdatamin, Prediction)
rastered_predsrange = Rasterize_all(newdatarange, Prediction)


# Same for SE
rastered_SE<-Rasterize_all(newdata, SE)
rastered_SEave<-Rasterize_all(newdataave, SE)
rastered_SEmin<-Rasterize_all(newdatamin, SE)
rastered_SErange<-Rasterize_all(newdatarange, SE)
# Bind SE and predictions together
rastered_predsSE<-c(rastered_preds, rastered_SE)

deltabuff = st_buffer(delta, dist = 0.01)

#############################################################################
#plotting functions

raster_plot<-function(data, labels="All", type){
  if (type == "range") lims = c(0,6) else lims = c(5,30)
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    facet_wrap(~Date)+
    scale_fill_viridis_c(name="Temperature", na.value="white",
                         limits=lims,
                         labels= function(x) ifelse((x/2)==as.integer(x/2), as.character(x), ""),
                         guide = guide_colorbar(direction="horizontal", title.position = "top", barwidth = 10, ticks.linewidth = 2,
                                                barheight=1, title.hjust=0.5, label.position="bottom", label.theme=element_text(size=12), 
                                                title.theme=element_text(size=13)))+
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "top")
}

raster_plot2<-function(data, date, labels="All"){
  data = data[,,,date]
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


raster_plot_range<-function(data, labels="All"){
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    # facet_wrap(~Date)+
    scale_fill_viridis_c(name="Temperature", na.value="white", breaks=seq(6,26,by=1), limits = c(0,8),
                         labels= function(x) ifelse((x/2)==as.integer(x/2), as.character(x), ""),
                         guide = guide_colorbar(direction="horizontal", title.position = "top", barwidth = 10, ticks.linewidth = 2,
                                                barheight=1, title.hjust=0.5, label.position="bottom", label.theme=element_text(size=12), 
                                                title.theme=element_text(size=13)))+
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "top")
}

#maximum temperatures
raster_plot(rastered_preds) + ggtitle("Max Temperature")
raster_plot2(rastered_preds, 3) + ggtitle("Max Temperature - March")
raster_plot2(rastered_preds, 4) + ggtitle("Max Temperature - April")
raster_plot2(rastered_preds, 10) + ggtitle("Max Temperature - Octover")
#It's treating the impact of space as constant accross time. I'd have to put in another interaction
#if i want it to vary.

#average temperatures
raster_plot(rastered_predsave) + ggtitle("Mean Temperature")
raster_plot2(rastered_predsave, 3) + ggtitle("Mean Temperature - March")
raster_plot2(rastered_predsave, 6) + ggtitle("Mean Temperature - June")
raster_plot2(rastered_predsave, 9) + ggtitle("Mean Temperature - September")
raster_plot2(rastered_predsave, 12) + ggtitle("Mean Temperature - December")


#minimum temps
raster_plot(rastered_predsmin) + ggtitle("minimum temperature")
raster_plot2(rastered_predsmin, 3) + ggtitle("minimum Temperatur -March")
raster_plot2(rastered_predsmin, 6) + ggtitle("minimum Temperatur -June")
raster_plot2(rastered_predsmin, 9) + ggtitle("minimum Temperatur -September")
raster_plot2(rastered_predsmin, 12) + ggtitle("minimum Temperatur -December")

#temp range
raster_plot(rastered_predsrange) + ggtitle("daily temperature range")
raster_plot2(rastered_predsrange, 3) + ggtitle("daily temperature range - March")
raster_plot2(rastered_predsrange, 6) + ggtitle("daily temperature range - June")
raster_plot2(rastered_predsrange, 9) + ggtitle("daily temperature range - Sep")
raster_plot2(rastered_predsrange, 12) + ggtitle("daily temperature range - Dec")


#OK, there are some very high "rang" and "maxtemp" predictions for some reason. Let's fiture this out.

maxs = filter(newdata, Prediction >35)
plot(maxs)
rastered_max<-Rasterize_all(maxs, Prediction)
raster_plot(rastered_max) + geom_sf(data = stas) +   geom_sf_label(data = stas, aes(label = Station))
geom_st(rastered_max)+ geom_sf(data = stas) +   geom_sf_label(data = stas, aes(label = Station))
#I might take out the stations down there just to make life easier. It looks like it's just station DV7
#Ugh. That iddn't work. I might have to get creative.

#It woudl be easier to see what was going on if I scale the colors for each day rather than across the seasons. 

test = st_crop(rastered_preds, delta)
plot(test)

#the places with the highest max tempsa nd greatest daily range are in the east delta and upper Suisun Marsh
#but I'm a little skeptical. Let's dig in.

foo = filter(newdatarange, Prediction >6)
foo2 = filter(tempmean2, Temprange >6)

#For some reason station FLT, which is near Martinez, has the highest range. It also has a
#lot of high max temps

foo3 = filter(newdata, Prediction >30)
foo4 = filter(tempmean2, TempMax >30)

#station RCS also has a lot of really high max temps and high range values. 
#that's the one all the way up at Kights' landing we were talking about dropping.

################################################################################
#What happens if I increase or decrease the tmperatures?

rastered_preds2 = rastered_preds %>%
mutate(Prediction = Prediction+2)

rastered_predsx2 = rastered_preds %>%
  mutate(Prediction = Prediction*1.02)

raster_plot(rastered_preds, type = "Max") + ggtitle("Max Temperature")
raster_plot(rastered_preds2, type = "Max") + ggtitle("Max Temperature with 2 degree increase")
raster_plot2(rastered_preds2, 3) + ggtitle("Max Temperature +2 - March")
raster_plot2(rastered_preds2, 6) + ggtitle("Max Temperature +2 - June")
raster_plot2(rastered_preds2, 10) + ggtitle("Max Temperature +2 - October")


raster_plot(rastered_predsx2) + ggtitle("Max Temperature with 2 percent increase")
raster_plot2(rastered_predsx2, 3) + ggtitle("Max Temperature - March")
raster_plot2(rastered_predsx2, 6) + ggtitle("Max Temperature - June")
raster_plot2(rastered_predsx2, 10) + ggtitle("Max Temperature - October")


rastered_preds2ave = rastered_predsave %>%
  mutate(Prediction = Prediction+2)

raster_plot(rastered_preds2ave) + ggtitle("Mean Temperature with 2 degree increase")
raster_plot2(rastered_preds2ave, 3) + ggtitle("Mean Temperature +2 - March")
raster_plot2(rastered_preds2ave, 6) + ggtitle("Mean Temperature +2 - June")
raster_plot2(rastered_preds2ave, 10) + ggtitle("Mean Temperature +2 - October")

#ten percent increase in temperature range
rastered_preds2range = rastered_predsrange %>%
  mutate(Prediction = Prediction+1.1)


raster_plot(rastered_preds2range, type = "range") + ggtitle("Temp range with 10% increase")
raster_plot(rastered_predsrange, type = "range") + ggtitle("Temp range")
raster_plot_range(rastered_preds2range, 3) + ggtitle("Temp range +10%- March")
raster_plot_range(rastered_preds2range, 6) + ggtitle(" Temp range +10% - June")
raster_plot_range(rastered_preds2range, 10) + ggtitle("Temp range +10% - October")

#Save all teh results
save(deltabuff, rastered_preds, rastered_preds2, rastered_preds2ave, rastered_predsave, rastered_predsmin,
     rastered_predsrange, rastered_predsSE, rastered_preds2range, file = "GAMresults.RData")
