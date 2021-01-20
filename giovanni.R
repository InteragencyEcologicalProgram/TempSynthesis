#Let's try downloading the pre-filtered data frames from Giovanni instead

library(ncdf4)
library(raster)
library(rgdal)
library(rgeos)
library(curl)
library(readr)
library(httr)
library(lubridate)
library(tidyverse)
my.crs = CRS("+proj=longlat")

# I pre-filtered just the area around central california and asked for aerosol optical depth 
# data from MODIS for the past ten years. It gave me the mean monthly data. THen I downloaded a text file with teh URLs. This 
#for all teh compontnent data.
URLs = read_csv("wildfire/Gdatatest.txt")

for(i in 31:120) {

  GET(unlist(URLs[i,]), authenticate("rkhartman", "H@ppy0625"), 
      write_disk(paste("wildfire/GIOVANNI/AOD", i, ".nc",sep = ""), overwrite = T))
  
  
  }

vname = "MYD08_M3_6_1_Aerosol_Optical_Depth_Land_Ocean_Mean_Mean"

#bounding box for the estuary
#-123.8159,36.2549,-118.2349,39.7705
xmin = 36
xmax = 40
ymin = -123
ymax = -118




read.giovanni = function(fname, vname) {
  my.crs = CRS("+proj=longlat")
  nc = nc_open(fname)
  data = ncvar_get(nc, vname)
  lon = ncvar_get(nc, "lon_bnds")
  lat = ncvar_get(nc, "lat_bnds")
  e <- extent(-123,-118, 36, 40) 
  ras = raster(data, xmn = min(lat[1,]), xmx = max(lat[2,]), ymn = min(lon[1,]), ymx = max(lon[2,]), crs = my.crs)
 test = flip(t(ras), direction = "y")
 final = crop(test, e)
}

fnames = rep("foo", nrow(URLs))

for(i in 1:nrow(URLs)) {
  fnames[i] = paste("wildfire/GIOVANNI/AOD", i, ".nc",sep = "")
}

alldata = map(fnames, read.giovanni, vname = vname) 

plot(alldata[[10]])

#extract a time series!!!

timeS = as.data.frame(raster::stack(alldata))

#This time I asked GIovanni to give me an area-averaged time series for central california. 
# I man want better spatial resolution late, but hey! Let's try i!

smoke = nc_open("wildfire/GIOVANNI/areaAvgTimeSeries.MOD08_D3_6_1_AOD_550_Dark_Target_Deep_Blue_Combined_Mean.20100101-20200131.123W_36N_118W_39N.nc")
vname = "MOD08_D3_6_1_AOD_550_Dark_Target_Deep_Blue_Combined_Mean"
data = ncvar_get(smoke, vname)
time = ncvar_get(smoke, "time")
time = as.POSIXct(time, origin = as.POSIXct("1970-01-01 00:00:00"))
smoke2 = data.frame(AOD = data, Time = as.Date(time))

#plot the average daily aerosol optical depth
ggplot(smoke2, aes(x= Time, y = AOD)) + geom_line() +
  xlab("Date") + ylab("Aerosol Optical Depth")

#filter to just the summers

smoke2sum = smoke2 %>%
  mutate(Year = year(Time), Month = month(Time), yday = yday(Time)) %>%
  filter(Month %in% c(7,8,9,10, 11))

ggplot(smoke2sum, aes(x= Time, y = AOD)) + geom_line()

#how does this line up with the fire time series?

fire = read.csv("fire_TS_8K.csv")
fire = mutate(fire, Time = ymd(date), X = NULL, bigfire = NA)
fire$bigfire[which(fire$acres> 50000)] = 2
fire$bigfire[which(fire$acres < 50000)] = 0

fire2010 = filter(fire, Time > ymd("2009-12-31"))


ggplot() + geom_line(data = filter(smoke2,Time <  ymd("2018-12-31")), aes(x=Time, y = AOD)) + 
  geom_area(data = fire2010, aes(x=Time, y = bigfire), fill = "pink", alpha = 0.5)


ggplot() + geom_point(data = filter(smoke2,Time <  ymd("2018-12-31")), aes(x=Time, y = AOD)) + 
  geom_area(data = fire2010, aes(x=Time, y = bigfire), fill = "pink", alpha = 0.5)



ggplot() + geom_point(data = filter(smoke2sum,Time <  ymd("2018-12-31")), aes(x=Time, y = AOD)) + 
  geom_area(data = fire2010, aes(x=Time, y = bigfire), fill = "pink", alpha = 0.5)

library(cder)

