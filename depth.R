#Let's take a quick look at the depth dataset
library(tidyverse)
library(lubridate)
library(nlme)

depth = read_rds("~/TempSynthesis/DepthAnalysis/Depth_data/depthdata_heatstress_20200728.rds")


#but first we will create some new date variables to make mainuplation easier
depth =  mutate(depth, Year = year(Datetime), #new variable for year
                Month = month(Datetime), #new variable for month
                Day = date(Datetime), #new variable for date (without time)
                julian = yday(Datetime), #new variable for day-of-year
                Station = as.factor(Station),
                WaterCol = as.factor(WaterCol))


#make a quick plot of the data
ggplot(depth, aes(x = Datetime, y = Temp, color = WaterCol)) + 
  geom_line() + facet_wrap(~Station)
#not a lot of clear differences

#let's just look at 2019-2020
depth2019 = filter(depth, year(Datetime) >2018)

ggplot(depth2019, aes(x = Datetime, y = Temp, color = WaterCol)) + 
  geom_line() + facet_wrap(~Station)

#let's pull out the daily mins, means, maxes

depth2019_s = group_by(depth2019, Station, WaterCol, Year, Month, Day, julian) %>% #group by day
  summarise(Temp2 = mean(Temp, na.rm = T), #caclulate means, mins, and maxes for each day
            maxTemp = max(Temp, na.rm = T), 
            mintemp = min(Temp, na.rm = T),
            range = maxTemp - mintemp) %>%
  ungroup()

#quick plot of mean temperature
ggplot(depth2019_s, aes(x = Day, y = Temp2, color = WaterCol)) + 
  geom_line() + facet_wrap(~Station)

ggplot(depth2019_s, aes(x = Day, y = mintemp, color = WaterCol)) + 
  geom_line() + facet_wrap(~Station)

ggplot(depth2019_s, aes(x = Day, y = maxTemp, color = WaterCol)) + 
  geom_line() + facet_wrap(~Station)
#bigger differences bewteen max temp on top and bottom.

ggplot(depth2019_s, aes(x = Day, y = range, color = WaterCol)) + 
  geom_line() + facet_wrap(~Station)

#We'll have to check for autocorrelation before we model any of this
acf(depth2019_s$Temp2)
pacf(depth2019_s$Temp2)

#highly autocorrelated

m1 = gls(Temp2~ julian, 
         data = depth2019_s, correlation = corAR1(form= ~ Day | Station/WaterCol))
summary(m1)
