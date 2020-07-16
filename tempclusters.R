#try cluster analysis with Cat's temperature data

library(tidyverse)
library(lubridate)

#load the filtered data set
temps = readRDS("Temp_filtered (1).rds")

#I think I"m going to try running the cluster analysis on the mean daily temperature for the past five years. At least
#that's a start. 

#first calculate the daily means
tempmean = temps %>%
  filter(Date > as.Date("2014-1-1"), Station != "RYF") %>%
  mutate(julian = yday(Date)) %>%
  group_by(Station, julian) %>%
  summarize(Temp = mean(Temp, na.rm = T))

#put it into wide format for the cluster analysis
tempwide = pivot_wider(tempmean, id_cols = c(Station), 
                       names_from = julian, values_from = Temp)

row.names(tempwide) = tempwide$Station

#calculate distance and cluster
tempdist = dist(tempwide, "euclidean")
tempfit = hclust(tempdist, method = "ward.D")
plot(tempfit, main = "Clusters based on daily ave temp")

############################################################################################
#now I'll do it based on average daily max and min temp

#first max and min for each day
tempmax = temps %>%
  filter(Date > as.Date("2014-1-1"), Station != "RYF") %>%
  mutate(julian = yday(Date), Year = year(Date)) %>%
  group_by(Station, julian, Year) %>%
  summarize(maxTemp = max(Temp, na.rm = T), minTemp = min(Temp))

#now the average max and min over five years
tempmax2 = tempmax %>%
  group_by(Station, julian) %>%
  summarize(maxTemp = mean(maxTemp, na.rm = T), minTemp = mean(minTemp, na.rm = T))


#put it into wide format for the cluster analysis
tempwidemx = pivot_wider(tempmax2, id_cols = c(Station), 
                       names_from = julian, values_from = maxTemp)
tempwidemn = pivot_wider(tempmax2, id_cols = c(Station), 
                         names_from = julian, values_from = minTemp)
tempmaxmin = cbind(tempwidemx, tempwidemn)

row.names(tempmaxmin) = tempmaxmin$Station...1
#tempmaxmin$Station...368 = NULL


#calculate distance and cluster
tempdist2 = dist(tempmaxmin, "euclidean")
tempfit2 = hclust(tempdist2, method = "ward.D")
plot(tempfit2, main = "Clusters based on daily max and min temp")

################################################################################################
#now let's try monthly min, mean, and max


#monthly mean, min, and max per year
tempmo = temps %>%
  filter(Date > as.Date("2014-1-1"), Station != "RYF") %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  group_by(Station, Month, Year) %>%
  summarize(Temp = mean(Temp, na.rm = T), min = min(Temp), max = max(Temp))

#average over five years
tempmo2 = tempmo %>%
  group_by(Station, Month) %>%
  summarize(Temp = mean(Temp, na.rm = T), min = mean(min, na.rm = T), max = mean(max, na.rm = T))

#put it into wide format for the cluster analysis
tempmowide = pivot_wider(tempmo2, id_cols = c(Station), 
                       names_from = Month, values_from = Temp)
tempmowide2 = pivot_wider(tempmo2, id_cols = c(Station), 
                         names_from = Month, values_from = min)
tempmowide3 = pivot_wider(tempmo2, id_cols = c(Station), 
                         names_from = Month, values_from = max)

tempmowide4 = cbind(tempmowide, tempmowide2[,-1], tempmowide3[,-1])

row.names(tempmowide4) = tempmowide4$Station

#calculate distance and cluster
tempdist3 = dist(tempmowide4, "euclidean")
tempfit3 = hclust(tempdist3, method = "ward.D")
plot(tempfit3, main = "Clusters based on monthly ave, min max")

library(vegan)
tempNMDS = metaMDS(tempdist3, trymax = 200)
plot(tempNMDS, type = "n")
text(tempNMDS, "sites", labels = tempmowide4$Station)
