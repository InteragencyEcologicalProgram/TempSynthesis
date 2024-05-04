
library(tidyverse)
library(lubridate)
library(scales)

#load the filtered data set
temps = readRDS("manuscript_code/Data/tempDaily.rds")
Temp_filtered = readRDS("Data/temp10years_20230613.rds")
latlon <- read_csv("Data/StationsMetadata.csv") %>%
  select(Station, Latitude, Longitude)

# grouping years
hotyears <- c(2014, 2015, 2016)
coolyears <- c(2010, 2011, 2012)

#percentage of day above 22
tempabove = Temp_filtered %>%
  mutate(above22 = case_when(Temp >= 22 ~ 1,
                             Temp <22 ~ 0),
         above25 = case_when(Temp >= 25 ~ 1,
                             Temp <25 ~ 0)) %>%
  group_by(Station, StationName, Date, WY) %>%
  summarize(percentabove22 = sum(above22)/n(),
            percentabove25 = sum(above25)/n()) %>%
  ungroup() 

temps2 = temps %>% 
  left_join(tempabove)%>%
  left_join(latlon) %>%
  arrange(Latitude) %>%
  mutate(Station = fct_inorder(factor(Station, ordered = TRUE)),
         Region = factor(Region, levels = c("North Delta", "Confluence", "Central", "South", "Suisun Bay", "Suisun Marsh")))

#calculate the daily means
tempmean = temps2 %>%
  mutate(dummyDate = ymd(paste0("2024-", format(Date, format = "%m-%d")))) %>%
  group_by(Station, dummyDate, Region) %>%
  summarize(Temp = mean(meanDaily, na.rm = T), 
            Tempmax = mean(maxDaily, na.rm =T),
            TempMin = mean(minDaily, na.rm =T), 
            percent = mean(percentabove22, na.rm =T), 
            percent25 = mean(percentabove25, na.rm =T))


# warm years
temps_warm <- temps2 %>%
  filter(WY %in% hotyears) %>%
  mutate(dummyDate = ymd(paste0("2024-", format(Date, format = "%m-%d")))) %>%
  group_by(Station, dummyDate, Region) %>%
  summarize(Temp = mean(meanDaily, na.rm = T), 
            Tempmax = mean(maxDaily, na.rm =T),
            TempMin = mean(minDaily, na.rm =T), 
            percent = mean(percentabove22, na.rm =T), 
            percent25 = mean(percentabove25, na.rm =T))

# cool years
temps_cool <- temps2 %>%
  filter(WY %in% coolyears) %>%
  mutate(dummyDate = ymd(paste0("2024-", format(Date, format = "%m-%d")))) %>%
  group_by(Station, dummyDate, Region) %>%
  summarize(Temp = mean(meanDaily, na.rm = T), 
            Tempmax = mean(maxDaily, na.rm =T),
            TempMin = mean(minDaily, na.rm =T), 
            percent = mean(percentabove22, na.rm =T), 
            percent25 = mean(percentabove25, na.rm =T))


#Percent of day over thresholds -----------------------

vlines <- data.frame(dummyDate=rep(seq(as.Date(0, origin="2024-01-01"),
                            length=12, by="1 month")),
                  Station=24)

# plotting function
# @df = data frame of interest
# @threshold = temperature threshold of interest
# @plottype = used for naming the plot file, needs to be in quotations

plot_percent <- function(df, threshold, plottype) {
 (pplot <- ggplot() + 
    geom_tile(data = df, aes(x = dummyDate, y = Station, fill = percent)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 expand = c(0,0)) + 
    geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray25", linetype = "dashed", size = 0.8) + 
    facet_grid(Region~., scales = "free", space = "free")+ 
    scale_fill_viridis_c(option = "B", name = paste0("Percent of Day above", threshold)) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank(),
          legend.position = "top"))
  
  ggsave(paste0("manuscript_code/Figures/",plottype, "_above",threshold, ".png"), device = "png", width =8, height =10)
  
  
}

# for testing function 
df = tempmean
threshold = 22
plottype = "stationheatmap"
##########

# Fun functions for different cases
plot_percent(tempmean, 22, "stationheatmap")
plot_percent(tempmean, 25, "stationheatmap")
plot_percent(temps_warm, 22, "warmyears")
plot_percent(temps_warm, 25, "warmyears")
plot_percent(temps_cool, 22, "coolyears")
plot_percent(temps_cool, 25, "coolyears")








# Old plots of mean 

ggplot(tempmean, aes(x = julian, y = Station, fill = Temp)) +
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(name = "Average")

ggsave("manuscript_code/Figures/stationsheatmap.tiff", device = "tiff", width =8, height =10)
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


