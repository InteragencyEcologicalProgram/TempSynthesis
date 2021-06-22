#Let's pull out everything for GAMs

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
            te(Latitude, Longitude, julian, bs = c("cs","cs", "cc")), 
          data =tempmean2, method = "REML")

r5 = acf(resid(g5),  plot=FALSE)$acf[2]
g5 =  bam(TempMax ~  
            te(Latitude, Longitude, julian, bs = c("cs","cs", "cc")), 
          data =tempmean2, method = "REML")
gam.check(g5)
acf(resid(g5))

###############################################
#OK, I need to optomize k
g5.1 =  bam(TempMax ~  
            te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
          data =tempmean2, method = "fREML", family = "scat", discrete = TRUE, nthreads = 3)

r5 = acf(resid(g5.1),  plot=FALSE)$acf[2]
g5.1 =  bam(TempMax ~  
            te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
          data =tempmean2, method = "fREML",  rho=r5, AR.start=tempmean2$start.event, family = "scat",
          discrete = TRUE, nthreads = 3)
gam.check(g5.1)
acf(resid_gam(g5.1))


###################################################################################
#basic model of mean temp based on day and location

g5ave =  bam(Tempave  ~  
               te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
             data =tempmean2, method = "fREML", family = "scat", discrete = TRUE, nthreads = 3)

r5ave = acf(resid(g5ave), plot=FALSE)$acf[2]



g5ave =  bam(Tempave  ~  
               te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
          data =tempmean2, method = "fREML",  rho=r5ave, AR.start=tempmean2$start.event, family = "scat",
          discrete = T, nthreads = 3)

plot(g5ave)
gam.check(g5ave)
acf(resid_gam(g5ave))
pacf(resid_gam(g5ave))
acf(resid(g5ave))

#OK, we still haven't dealt with autocorrelation very well. Let's try differencing. 
tempmean2 = mutate(tempmean2, Tempavediff = diff(c(NA, Tempave)))

g5ave =  bam(Tempavediff  ~  
               te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
             data =tempmean2, method = "fREML", family = "scat", discrete = TRUE, nthreads = 3)

r5ave = acf(resid(g5ave), plot=FALSE)$acf[2]



g5ave =  bam(Tempavediff  ~  
               te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
             data =tempmean2, method = "fREML",  rho=r5ave, AR.start=tempmean2$start.event, family = "scat",
             discrete = T, nthreads = 3)

plot(g5ave)
gam.check(g5ave)
acf(resid_gam(g5ave))
pacf(resid_gam(g5ave))
acf(resid(g5ave))

############################################################
#try a different autocorrelation function
library(forecast)
library(lme4)

gam_6_ar = gamm(Tempave  ~  
                   te(Latitude, Longitude, julian, d = c(2,1), k = c(25, 5), bs = c("cr", "cc")), 
                 data =tempmean2, method = "fREML",  family = "scat")

arma_res <- auto.arima(resid(gam_6_ar$lme, type = "normalized"),
                       stationary = TRUE, seasonal = FALSE)

arma_res$coef


gam_6_ar1 = gamm(Tempave  ~  
                  te(Latitude, Longitude, julian, d = c(2,1), k = c(25, 5), bs = c("cr", "cc")), 
                data =tempmean2, method = "fREML",  family = "scat", 
                correlation = corARMA(form = ~ 1|julian, p = 4, q = 3))


#test it with a smaller data set
test = filter(tempmean2, Year == 2015)
gam_6_test = gamm(Tempave  ~  
                   te(Latitude, Longitude, julian, d = c(2,1), k = c(25, 5), bs = c("cr", "cc")), 
                 data =test, method = "fREML",  family = "scat", 
                 correlation = corARMA(form = ~ 1|julian, p = 4, q = 3))
summary(gam_6_test)
plot(gam_6_test$gam)
gam.check(gam_6_test$gam)
acf(resid_gam(gam_6_test))

#OK, this isn't working.

##############################################################################
#basic model of minimum temp based on day and location

g5min =  bam(TempMin  ~  
               te(Latitude, Longitude, julian,  d = c(2,1), k = c(50, 12), bs = c("cr", "cc")),
             data =tempmean2, method = "fREML", family = "scat", discrete = TRUE, nthreads = 3)

r5min = acf(resid(g5min), plot=FALSE)$acf[2]



g5min =  bam(TempMin  ~  
               te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")),
             data =tempmean2, method = "fREML", family = "scat", discrete = TRUE, nthreads = 3,
             rho=r5min, AR.start=tempmean2$start.event)


summary(g5min)
plot(g5min)
gam.check(g5min)
acf(resid_gam(g5min))


load("spatialdata.RData")
regions = read_sf("RosieRegions/shpExport.shp")
regions = st_transform(regions, crs = 4326)

stas = read.csv("StationLatLongs.csv")
ch = chull(stas$Longitude, stas$Latitude)
hull = stas[ch,]


coordinates(stas) = ~ Longitude + Latitude
crs(stas) <- "+proj=longlat +datum=NAD83"


delta = st_transform(delta,crs=4326)
stas = st_as_sf(stas) %>%
  st_transform(stas, crs=4326) %>%
  filter(Station != "DV7", Station != "RIP", Station != "RPN", Station != "RCS")



#map of the delt with just sites close to stations



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


deltabuff = st_buffer(delta, dist = 0.01) %>%
  st_intersection(chbuff)

save(deltabuff, file = "deltabuff.RData")

#####################################################################
#predicions

WQ_pred<-function(Full_data=Data,
                  Delta_subregions = regions,
                  Delta_water=deltabuff,
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

#my computer will only run six months at a time.
newdata_year <- WQ_pred(Full_data=tempmean2, n=500,
                        Julian_days = yday(ymd(paste("2014", c(1,4,7,10), "15", sep="-"))))

newdata_year = rename(newdata_year, julian = Julian_day)

#Now the second half of the year
newdata_year2 <- WQ_pred(Full_data=tempmean2, n=500,
                        Julian_days = yday(ymd(paste("2014", 7:12, "15", sep="-"))))

newdata_year2 = rename(newdata_year2, julian = Julian_day)

#can I stick them all together?
newdata_yearx = bind_rows(newdata_year, newdata_year2)
#Nope. Strange.


modellc4_predictions<-predict(g5.1, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T) # Create predictions
modellave_predictions<-predict(g5ave, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T) # Create predictions
modellminpredictions<-predict(g5min, newdata=newdata_year, type="response", se.fit=TRUE, discrete=T)

modellc4_predictions2<-predict(g5.1, newdata=newdata_year2, type="response", se.fit=TRUE, discrete=T) # Create predictions
modellave_predictions2<-predict(g5ave, newdata=newdata_year2, type="response", se.fit=TRUE, discrete=T) # Create predictions
modellminpredictions2<-predict(g5min, newdata=newdata_year2, type="response", se.fit=TRUE, discrete=T)

#mean temp model
newdataave<-newdata_year%>%
  mutate(Prediction=modellave_predictions2$fit)%>%
  mutate(SE=modellave_predictions2$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96)


#max temp model
newdata<-newdata_year%>%
  mutate(Prediction=modellc4_predictions2$fit)%>%
  mutate(SE=modellc4_predictions2$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96) %>%
  filter(Prediction <35, Prediction >0)

#min temp model
newdatamin<-newdata_year%>%
  mutate(Prediction=modellminpredictions2$fit)%>%
  mutate(SE=modellminpredictions2$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96)


save(g5.1, g5ave, g5min, g5range, newdata, newdata_year, newdataave,
     newdatamin, newdatarange, file = "GAMresults5MAY2021.RData")

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
rastered_predsrange = rastered_preds - rastered_predsmin
  
save(rastered_preds, rastered_predsave, rastered_predsmin, 
     rastered_predsrange, file = "RasteredPreds5MAY2021.RData")













############################################################################3
#analysis for TUCP

#######
#just the summer
newdata_yearsummer <- WQ_pred(Full_data=tempmean2, n=500,
                        Julian_days = yday(ymd(paste("2014", c(6,7,8), "15", sep="-"))))

newdata_yearsummer = rename(newdata_yearsummer, julian = Julian_day)
modellc4_predictionsS<-predict(g5.1, newdata=newdata_yearsummer, type="response", se.fit=TRUE, discrete=T) # Create predictions

newdataS<-newdata_yearsummer%>%
  mutate(Prediction=modellc4_predictionsS$fit)%>%
  mutate(SE=modellc4_predictionsS$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96) %>%
  filter(Prediction <35, Prediction >0)

save(g5.1, newdata_yearsummer, modellc4_predictionsS, file = "droughtstuff.RData")

rastered_predsSum<-Rasterize_all(newdataS, Prediction)

preds<-map(unique(data$julian), function(x) st_rasterize(data%>%
                                                           filter(julian==x)%>%
                                                           dplyr::select(Prediction), 
                                                         template=st_as_stars(st_bbox(delta2), dx=diff(st_bbox(delta2)[c(1, 3)])/n, 
                                                                              dy=diff(st_bbox(delta2)[c(2, 4)])/n, values = NA_real_))%>%
             st_warp(crs=out_crs))

# Then bind all dates together into 1 raster
out <- exec(c, !!!preds, along=list(Date=unique(data$julian)))

######################################################################################################
#Do it for just 2014, 2015, and 2017
temp2014 = filter(tempmean2, Year == 2014)

#OK, I need to optomize k
g5.12014 =  bam(TempMax ~  
              te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
            data =temp2014, method = "fREML", family = "scat", discrete = TRUE, nthreads = 3)

r5 = acf(resid(g5.12014),  plot=FALSE)$acf[2]
g5.12014 =  bam(TempMax ~  
              te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
            data =tempmean2, method = "fREML",  rho=r5, AR.start=tempmean2$start.event, family = "scat",
            discrete = TRUE, nthreads = 3)

newdata_2014 <- WQ_pred(Full_data=temp2014, n=500,
                              Julian_days = yday(ymd(paste("2014", c(6,7,8), "15", sep="-"))))

newdata_2014 = rename(newdata_2014, julian = Julian_day)
pred2014<-predict(g5.12014, newdata=newdata_2014, type="response", se.fit=TRUE, discrete=T) # Create predictions

newdataS2014<-newdata_2014%>%
  mutate(Prediction=pred2014$fit)%>%
  mutate(SE=pred2014$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96) %>%
  filter(Prediction <35, Prediction >0)

save(g5.12014, newdataS2014, pred2014, file = "droughtstuff2014.RData")

rastered_preds2014<-Rasterize_all(newdataS2014, Prediction)


temp2015 = filter(tempmean2, Year == 2015)

#2015
g5.12015 =  bam(TempMax ~  
                  te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
                data =temp2015, method = "fREML", family = "scat", discrete = TRUE, nthreads = 3)

r5 = acf(resid(g5.12015),  plot=FALSE)$acf[2]
g5.12015 =  bam(TempMax ~  
                  te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
                data =tempmean2, method = "fREML",  rho=r5, AR.start=tempmean2$start.event, family = "scat",
                discrete = TRUE, nthreads = 3)

pred2015<-predict(g5.12015, newdata=newdata_2014, type="response", se.fit=TRUE, discrete=T) # Create predictions

newdataS2015<-newdata_2014%>%
  mutate(Prediction=pred2015$fit)%>%
  mutate(SE=pred2015$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96) %>%
  filter(Prediction <35, Prediction >0)

save(g5.12015, newdataS2015, pred2015, file = "droughtstuff2015.RData")

rastered_preds2015<-Rasterize_all(newdataS2015, Prediction)


#2017
temp2017 = filter(tempmean2, Year == 2017)


g5.12017 =  bam(TempMax ~  
                  te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
                data =temp2017, method = "fREML", family = "scat", discrete = TRUE, nthreads = 3)

r5 = acf(resid(g5.12017),  plot=FALSE)$acf[2]
g5.12017 =  bam(TempMax ~  
                  te(Latitude, Longitude, julian, d = c(2,1), k = c(50, 12), bs = c("cr", "cc")), 
                data =tempmean2, method = "fREML",  rho=r5, AR.start=tempmean2$start.event, family = "scat",
                discrete = TRUE, nthreads = 3)

pred2017<-predict(g5.12017, newdata=newdata_2014, type="response", se.fit=TRUE, discrete=T) # Create predictions

newdataS2017<-newdata_2014%>%
  mutate(Prediction=pred2017$fit)%>%
  mutate(SE=pred2017$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96) %>%
  filter(Prediction <35, Prediction >0)

save(g5.12017, newdataS2015, pred2017, file = "droughtstuff2017.RData")

rastered_preds2017<-Rasterize_all(newdataS2017, Prediction)
