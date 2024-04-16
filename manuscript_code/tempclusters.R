#try cluster analysis with Cat's temperature data

library(tidyverse)
library(lubridate)

#load the filtered data set
temps = readRDS("manuscript_code/Data/tempDaily.rds")

#I think I"m going to try running the cluster analysis on the mean daily temperature for the past five years. At least
#that's a start. 

#percentage of day above 22
tempabove = Temp_filtered %>%
  mutate(above22 = case_when(Temp >= 22 ~ 1,
                             Temp <22 ~ 0)) %>%
  group_by(Station, StationName, Date) %>%
  summarize(percentabove22 = sum(above22)/n())

temps2 = left_join(temps, tempabove)

#first calculate the daily means
tempmean = temps2 %>%
  mutate(julian = yday(Date)) %>%
  group_by(Station, julian, Region) %>%
  summarize(Temp = mean(meanDaily, na.rm = T), Tempmax = mean(maxDaily, na.rm =T),
            TempMin = mean(minDaily, na.rm =T), percent = mean(percentabove22, na.rm =T))

#put it into wide format for the cluster analysis
tempwide = pivot_wider(tempmean, id_cols = c(Station), 
                       names_from = julian, values_from = Temp)

row.names(tempwide) = tempwide$Station

#calculate distance and cluster
tempdist = dist(tempwide, "euclidean")
tempfit = hclust(tempdist, method = "ward.D")
plot(tempfit, main = "Clusters based on daily ave temp")


#this is the new plot that Cat likes
ggplot(tempmean, aes(x = julian, y = Station, fill = Temp)) +
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(name = "Average")

ggsave("manuscript_code/Figures/stationsheatmap.tiff", device = "tiff", width =8, height =10)

ggplot(tempmean, aes(x = julian, y = Station, fill = percent)) +
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "A", name = "Percent of \nDay above 22")

ggsave("manuscript_code/Figures/stationsheatmap_above22.tiff", device = "tiff", width =8, height =10)


ggplot(tempmean, aes(x = julian, y = Station, fill = Tempmax)) +
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "A", name = "Max")


ggplot(tempmean, aes(x = julian, y = Station, fill = TempMin)) +
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "C", name = "Min")

ggplot(tempmean, aes(x = julian, y = Station, fill = Tempmax-TempMin))+
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "turbo", name = "Range")

ggplot(filter(tempmean, Station %in% c("BDL", "RVB")),aes(x = julian, y = Station, fill = Tempmax-TempMin))+
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "turbo", name = "Range")


############################################################################################
#now I'll do it based on average daily max and min temp

#first max and min for each day
tempmax = temps %>%
  filter(Station != "RYF") %>%
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
