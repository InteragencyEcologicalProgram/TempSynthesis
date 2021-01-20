#Look at wildfire data

library(sf)
library(maptools)
library(ggmap)
library(tidyverse)
library(lubridate)


fires = read_sf("wildfire/Cal_Hist_1950_2018.shp")
badpoly = fires[which(st_is_valid(fires) == FALSE),]

fires2 = fires[which(st_is_valid(fires) == TRUE),]


#something is wrong with point 14442
fires2 = filter(fires2, OBJECTID != 14442)
fires2$YEAR_ = as.numeric(fires2$YEAR_)


plot(fires2)

#circle around Chipps island
Chipps = data.frame(
  ID = "Chipps",
  longitude = as.numeric(-121.896778),
  latitude = as.numeric(38.045170)
)

library(rgeos)
library(sp)
d <- SpatialPointsDataFrame(coords = Chipps[, -1], 
                            data = Chipps, 
                            proj4string = CRS("+init=epsg:4326"))
d_mrc <- spTransform(d, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
#Now, the width can be specified in meters:
  
  d_mrc_bff_mrc <- gBuffer(d_mrc, byid = TRUE, width = 800000)
 
  d_mrc_bff <- spTransform(d_mrc_bff_mrc, CRS("+init=epsg:4326"))
 Chipps2 = st_as_sf(d_mrc_bff)
 
#Transform it back and add it to the plot using geom_path:
  

ggplot() + geom_sf(data = fires2, aes(fill = YEAR_)) + geom_sf(data = Chipps2, alpha = 0.5, fill = "grey")

#Clip the fires so they are just ones that might effect the Delta

fires2.1 = st_transform(fires2, 4326)
firesclose = st_intersection(Chipps2, fires2.1)
ggplot() + geom_sf(data = firesclose, aes(fill = YEAR_))

write.csv(firesclose, "fires_within_400K.csv")

#Now I just need to turn this into a time series

#this works, but it can't handle more than one decade at a time.
df <- data.frame(date = seq(as.Date('1950-01-01'),as.Date('2018-12-31'),by = 1))

decadesum = function(decade, yrs, fires) {
  start = as.Date(paste(decade, "-01-01", sep = ""))
  end = as.Date(paste((decade+yrs), "-12-31", sep = ""))
df2 <- data.frame(date = seq(start, end,by = 1))
firesclose1 = filter(fires, YEAR_ < (decade+yrs))
                 
df2a = merge(df2, firesclose1, all=TRUE) %>% 
  filter(date >= ALARM_DATE, date <= CONT_DATE) 

df2b =   group_by(df2a, date) %>% 
  summarise(n(), acres = sum(GIS_ACRES))

return(df2b)
}

#We cant run it all at once because it gets too big, so I have to chunk it
#there is probably a more elegant way of doing this.

test = decadesum(1950, 10, firesclose)
test60s = decadesum(1960, 10, firesclose)
test70s = decadesum(1970, 2, firesclose)
test73s = decadesum(1973, 3, firesclose)
test77s = decadesum(1977, 2, firesclose)
test80s = decadesum(1980, 5, firesclose)
test86s = decadesum(1986, 4, firesclose)
test90s = decadesum(1990, 5, firesclose)
test96s = decadesum(1996, 4, firesclose)
test00s = decadesum(2000, 5, firesclose)
test06s = decadesum(2006, 3, firesclose)
test10s = decadesum(2010, 3, firesclose)
test14s = decadesum(2014, 3, firesclose)
test15s = decadesum(2017, 2, firesclose)

fireTS = distinct(rbind(test, test60s, test70s, test73s, test77s, 
               test80s, test86s, test90s, test96s, test00s, 
               test06s, test10s, test14s, test15s))


fireTS2 = left_join(df, fireTS)
fireTS2$acres[which(is.na(fireTS2$acres))] = 0
write.csv(fireTS2, "fire_TS_8K.csv")

#probably just want the summer for analysis, since that's when most
#of the fires are and that's when the highest chlorophyll occurs
fireSummer = mutate(fireTS2, month = month(date)) %>%
  filter(month %in% c(7,8,9, 10, 11))
fireSummer$acres[which(is.na(fireSummer$acres))] = 0

#Start with chlorophyll from EMP's dataset. Try continuous data later
EMP = read.csv("SacSJ_delta_water_quality_1975_2019.csv")
EMPc = EMP %>%
  mutate(Chla = as.numeric(Chla), logCh = log(Chla), date = mdy(Date), month = month(date)) %>%
  filter(month %in% c(7,8,9, 10, 11))


fireEMP = left_join(EMPc, fireSummer)
smokeEMP = rename(smoke2sum, date = Time) %>%
  merge(EMPc)

ggplot(fireEMP, aes(x = acres, y = logCh)) + geom_point()
ggplot(smokeEMP, aes(x = AOD, y = logCh)) + geom_point() + geom_smooth(method = "lm") +
  xlab("Aerosol Optical Depth") + ylab("Log EMP Chlorophyll")
ggplot(smokeEMP, aes(x = AOD, y = Chla)) + geom_point() + geom_smooth(method = "lm")

ggplot(smokeEMP, aes(x = AOD, y = DOSurface)) + geom_point() + geom_smooth(method = "lm")


EMPsum = mutate(fireEMP, Year = year(date)) %>%
  group_by(month, Year) %>%
  summarise(acres = mean(acres, na.rm = T), chl = mean(logCh, na.rm = T))

ggplot(EMPsum, aes(x = acres, y = chl)) + geom_point()

library(lme4)
library(lmerTest)
library(visreg)
lm1 = lmer(logCh~ acres + month + (1|StationCode), data = fireEMP)
summary(lm1)
#warning, Some predictor variables are on very different scales: consider rescaling

fireEMP = mutate(fireEMP, acres_scaled = scale(acres), 
                 julian = yday(date), Year = year(date))

lm1x = lmer(logCh~ AOD + (1|StationCode), data = smokeEMP)
summary(lm1x)

lm1x = lmer(Result~ AOD + (1|StationCode), data = filter(smokeEMP, AOD < 0.4))
summary(lm1x)
visreg(lm1x)


lm2 = lmer(logCh~ acres_scaled + Year + (1|StationCode), data = fireEMP)
summary(lm2)
visreg(lm2)

#Dang it. The year effect swamps any fire effect. 


lm3 = lmer(acres_scaled~ Year + (1|StationCode), data = fireEMP)
summary(lm3)
visreg(lm3)

ggplot(fireEMP, aes(x=date, y = acres)) + geom_point()

#what if we just look at high-fire versus low fire years?

fireEMPav = group_by(fireEMP, Year) %>%
  summarize(avCH = mean(logCh, na.rm = T), Fire = max(acres, na.rm = T),
            Firecat = NA)
fireEMPav$Firecat[which(fireEMPav$Fire> 25190)] = "Low"

fireEMPav$Firecat[which(fireEMPav$Fire< 25190)] = "High"

lm4 = glm(avCH~ Year + Firecat, data = fireEMPav)
summary(lm4)
visreg(lm4)

#Meh

fireEMP2 = merge(fireEMP, fireEMPav)
fireEMPhigh = filter(fireEMP2, Firecat == "High")

lm2 = lmer(logCh~ acres_scaled + Year + (1|StationCode), data = fireEMPhigh)
summary(lm2)
visreg(lm2)

ggplot(data = fireEMPhigh, aes(x = acres, y = logCh)) + geom_point() +geom_smooth(method = "lm")
