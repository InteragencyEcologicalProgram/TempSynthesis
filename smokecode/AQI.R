#let's try EPA's air quality sensors instead of fires

#and, of course, there's a package for that.
library(PWFSLSmoke)
library(tidyverse)

#download data
testaqi= airsis_loadAnnual(2017) %>%
  monitor_subset(stateCodes = c("CA"))


monitor_leaflet(testaqi)

#it looks like the only site in the Delta is lon_.122.133_lat_38.211_arb2.1032 (Fairfield)
#but I could also maybe use the Sacramento ones

Fairfield = 
  testaqi %>%
  monitor_subset(monitorIDs = c("lon_.122.133_lat_38.211_arb2.1032",
                                "lon_.122.133_lat_38.211_arb2.1034",
                                "lon_.122.157_lat_38.054_arb2.1037",
                                "lon_.122.137_lat_38.080_arb2.1032"))

monitor_dygraph(Fairfield)

#I don't understand why tehre isnt' continuous data. 

#Instead I used used teh GUI to download it: https://www.epa.gov/outdoor-air-quality-data/download-daily-data

readAQ = function(prefix, years) {
  AQ = read.csv(paste(prefix, years[1], ".csv", sep = "")) 
  for (i in 2:length(years)) {
    AQ2 = read.csv(paste(prefix, years[i], ".csv", sep = ""))
    AQ = rbind(AQ, AQ2)
  }
  return(AQ)
}

solanoAQ = readAQ("wildfire/AQsolano", 2010:2018)
solanoAQ = mutate(solanoAQ, date = as.Date(Date, format = "%m/%d/%Y"),
                  Month = month(date), Year = year(date))


ggplot(solanoAQ, aes(x = Date, y = Daily.Mean.PM2.5.Concentration)) + geom_line()

solanoAQsummer = filter(solanoAQ, Month %in% c(6,7,8,9))

EMP2010 = mutate(EMPc, date = as.Date(SampleDate), Month = month(date), Year = year(date)) %>%
  filter(Month %in% c(6,7,8,9), Year > 2009)

EMP20202 = merge(EMP2010, solanoAQsummer, by = "date", all.x = T, all.y = FALSE)

ggplot(EMP20202, aes(y = logCh, x = Daily.Mean.PM2.5.Concentration)) + geom_point()
ggplot(EMP20202, aes(y = logCh, x = DAILY_AQI_VALUE)) + geom_point()

#merge with USGS data and filter so it's just the grizzly bAY site
USGSAQI = merge(USGSsites3, solanoAQsummer) %>%
  filter(site_no == 380631122032201)
ggplot(USGSAQI, aes(y = logchla, x = Daily.Mean.PM2.5.Concentration)) + geom_point()
ggplot(USGSAQI, aes(y = chla, x = DAILY_AQI_VALUE)) + geom_point()

#I may want to try: https://www.ospo.noaa.gov/Products/land/hms.html