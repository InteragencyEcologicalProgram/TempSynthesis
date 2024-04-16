
library(tidyverse)
library(lubridate)

#load the filtered data set
temps = readRDS("manuscript_code/Data/tempDaily.rds")
Temp_filtered = readRDS("Temp_filtered.rds")

#percentage of day above 22
tempabove = Temp_filtered %>%
  mutate(above22 = case_when(Temp >= 22 ~ 1,
                             Temp <22 ~ 0),
         above25 = case_when(Temp >= 25 ~ 1,
                             Temp <25 ~ 0)) %>%
  group_by(Station, StationName, Date) %>%
  summarize(percentabove22 = sum(above22)/n(),
            percentabove25 = sum(above25)/n())

temps2 = left_join(temps, tempabove)

#first calculate the daily means
tempmean = temps2 %>%
  mutate(julian = yday(Date)) %>%
  group_by(Station, julian, Region) %>%
  summarize(Temp = mean(meanDaily, na.rm = T), Tempmax = mean(maxDaily, na.rm =T),
            TempMin = mean(minDaily, na.rm =T), percent = mean(percentabove22, na.rm =T), 
            percent25 = mean(percentabove25, na.rm =T))


#this is the new plot that Cat likes
ggplot(tempmean, aes(x = julian, y = Station, fill = Temp)) +
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(name = "Average")

ggsave("manuscript_code/Figures/stationsheatmap.tiff", device = "tiff", width =8, height =10)

ggplot(tempmean, aes(x = julian, y = Station, fill = percent)) +
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "A", name = "Percent of \nDay above 22")

ggsave("manuscript_code/Figures/stationsheatmap_above22.tiff", device = "tiff", width =8, height =10)

ggplot(tempmean, aes(x = julian, y = Station, fill = percent25)) +
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "A", name = "Percent of \nDay above 25")

ggsave("manuscript_code/Figures/stationsheatmap_above25.tiff", device = "tiff", width =8, height =10)



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


##############################################################


