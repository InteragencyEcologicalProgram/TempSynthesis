#Some exploritory smoke data analysis for Nicole
#Rosemary Hartman, 12/30/2020

library(tidyverse)
library(lubridate)

#I just put all the data I"ve been playing with together in a data file
load("smoke.RData")

#This includes the following data sets

#All the discrete EMP water quality data 1975-2019 (as downloaded from EDI)
#I also adeded "logchla" for a log-transformed version of chla if you want it.
View(EMPc)

#Average aerosol optical depth (AOD: smoke) over central california from 2010 through present.
#I'd like to go back and get more data, it's available through 2001, but this was a start.
#I downloaded this from Giovanni, https://giovanni.gsfc.nasa.gov/giovanni/
#there were a lot of different options for AOD, and I'm not sure I picked the right one.
#That would be a good question for Erin Hestir.
#I used a pretty big bounding box to filter the data, we could make it smaller to get just the Delta.
View(smoke2)

#I then filtered the smoke data to include just July-November, to encompass most of the fire season.
View(smoke2sum)

#I merged the summer smoke data with EMP's data
View(smokeEMP)

#I downloaded continuous chlorophyll data from a few different stations in the Delta from USGS's NWIS site.
View(USGSsites2)

#I merged three of these sites with the smoke data (just the summers)
View(USGSsmoke)

#I also have a time series acres of active fire parimeters within 800Km of the Delta. 
View(fires_TS)

#Then I played around with a few plots
#continuous chlorophyll
ggplot(USGSsmoke, aes(x = AOD, y = chla)) + geom_point() +
  facet_grid(.~site_no)

#Plot fire and smoke versus EMP data

#first restrict the fire data to just summer and join it with EMP
fireSummer = mutate(fires_TS, month = month(date)) %>%
  filter(month %in% c(7,8,9, 10, 11))
fireSummer$acres[which(is.na(fireSummer$acres))] = 0

fireEMP = left_join(EMPc, fireSummer)

#look at a linear model
lm1 = lmer(logCh~ acres + month + (1|StationCode), data = fireEMP)
summary(lm1)
#warning, Some predictor variables are on very different scales: consider rescaling

#rescale the "acres" value
fireEMP = mutate(fireEMP, acres_scaled = scale(acres), 
                 julian = yday(date), Year = year(date))
lm2 = lmer(logCh~ acres_scaled + Year + (1|StationCode), data = fireEMP)
summary(lm2)
visreg(lm2)

#Now try it versus smoke.
lm1x = lmer(logCh~ AOD + (1|StationCode), data = smokeEMP)
summary(lm1x)

#hmmm.... very high AOD values might be throwing it off?
lm1x = lmer(Result~ AOD + (1|StationCode), data = filter(smokeEMP, AOD < 0.4))
summary(lm1x)
visreg(lm1x)

#make some plots of smoke and fire together
#I made a new "bigfire" column to identify periods where we had a lot of fires burning
smoke2 = rename(smoke2, date = Time)
fire = mutate(fires_TS, bigfire = NA)
fire$bigfire[which(fire$acres> 50000)] = 2
fire$bigfire[which(fire$acres < 50000)] = 0

fire2010 = filter(fire, date > ymd("2009-12-31"))


ggplot() + geom_line(data = filter(smoke2,date <  ymd("2018-12-31")), aes(x=date, y = AOD)) + 
  geom_area(data = fire2010, aes(x=date, y = bigfire), fill = "pink", alpha = 0.5)

#let's look at just the summers
ggplot() + geom_point(data = filter(smoke2sum,date <  ymd("2018-12-31")), aes(x=date, y = AOD)) + 
  geom_area(data = fire2010, aes(x=date, y = bigfire), fill = "pink", alpha = 0.5)
#wow. Most of the summers were all "bigfire" periods!


