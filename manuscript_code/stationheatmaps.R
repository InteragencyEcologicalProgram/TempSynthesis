############################
# Created by Rosie Hartman (DWR) and edited by Cat Pien (USBR)
# cpien@usbr.gov
# Last edited 6/11/2025 (made notations and changed tiffs to pngs)
# This code creates: 
# Figure 6 - heat map looking at proportion of days/stations >22C
# Figure A4 - no recovery barplot comparing warm years and mean
# Other plots include summary stat heatmaps that were not included in the manuscript
#############################

library(tidyverse)
library(lubridate)
library(scales)
library(here)

# Load data -----------------------------
temps = readRDS(here("manuscript_code/Data/tempDaily.rds"))
Temp_filtered = readRDS(here("manuscript_code/Data/temp10years_20230613.rds"))
latlon <- read_csv(here("manuscript_code/Data/StationsMetadata.csv")) %>%
  dplyr::select(Station, Latitude, Longitude)

# Process data ---------------

## grouping years ------------------------
hotyears <- c(2014, 2015, 2016)
coolyears <- c(2010, 2011, 2012)

## assign if above 22/25 -------------------
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
  arrange(desc(Longitude)) %>%
  mutate(Station = fct_inorder(factor(Station, ordered = TRUE)),
         Region = factor(Region, levels = c("North Delta", "Confluence", "Central", "South", "Suisun Bay", "Suisun Marsh")))

## calculate means across stations -------------------
dataset_means <- Temp_filtered %>%
  group_by(Station) %>%
  summarize(Tempmean = round(mean(Temp, na.rm = T),1), 
            Tempmax = round(max(Temp, na.rm =T),1),
            TempMin = round(min(Temp, na.rm =T),1)) %>%
  ungroup() 

# write summary stats
write_csv(dataset_means, "manuscript_code/Data/temp_summary_stats_station.csv")


## calculate the daily means -----------------------

tempmean = temps2 %>%
  mutate(dummyDate = ymd(paste0("2024-", format(Date, format = "%m-%d")))) %>%
  group_by(Station, dummyDate, Region) %>%
  summarize(Temp = mean(meanDaily, na.rm = T), 
            Tempmax = mean(maxDaily, na.rm =T),
            TempMin = mean(minDaily, na.rm =T), 
            percent = mean(percentabove22, na.rm =T), 
            percent25 = mean(percentabove25, na.rm =T)) %>%
  ungroup() %>%
  mutate(month = month(dummyDate)) %>%
  group_by(dummyDate) %>%
  mutate(Tempmax_s = scale(Tempmax),
         Tempmin_s = scale(TempMin)) %>% ungroup()


# warm years
temps_warm <- temps2 %>%
  filter(WY %in% hotyears) %>%
  mutate(dummyDate = ymd(paste0("2024-", format(Date, format = "%m-%d")))) %>%
  group_by(Station, dummyDate, Region) %>%
  summarize(Temp = mean(meanDaily, na.rm = T), 
            Tempmax = mean(maxDaily, na.rm =T),
            TempMin = mean(minDaily, na.rm =T), 
            percent = mean(percentabove22, na.rm =T), 
            percent25 = mean(percentabove25, na.rm =T)) %>%
  ungroup() %>%
  mutate(month = month(dummyDate))%>%
  mutate(Tempmax_s = scale(Tempmax),
         Tempmin_s = scale(TempMin)) %>% ungroup()

# cool years
temps_cool <- temps2 %>%
  filter(WY %in% coolyears) %>%
  mutate(dummyDate = ymd(paste0("2024-", format(Date, format = "%m-%d")))) %>%
  group_by(Station, dummyDate, Region) %>%
  summarize(Temp = mean(meanDaily, na.rm = T), 
            Tempmax = mean(maxDaily, na.rm =T),
            TempMin = mean(minDaily, na.rm =T), 
            percent = mean(percentabove22, na.rm =T), 
            percent25 = mean(percentabove25, na.rm =T)) %>%
  mutate(month = month(dummyDate))


# Plot Percent of day over thresholds -----------------------

# create dummy data to draw vert lines
vlines <- data.frame(dummyDate=rep(seq(as.Date(0, origin="2024-01-01"),
                            length=12, by="1 month")),
                  Station=24)

## plotting function ---------------------
# @df = data frame of interest
# @threshold = temperature threshold of interest
# @plottype = used for naming the plot file, needs to be in quotations

plot_percent <- function(df, threshold, plottype) {
 (pplot <- ggplot() + 
    geom_tile(data = df, aes(x = dummyDate, y = Station, fill = percent)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 expand = c(0,0)) + 
    geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray40", linetype = "dashed", size = 0.8) + 
    facet_grid(Region~., scales = "free", space = "free")+ 
    scale_fill_viridis_c(option = "B", name = paste0("Percent of Day above", threshold)) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, size = 13),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 11),
          axis.title.x = element_blank(),
          legend.position = "top"))
  
  ggsave(paste0("manuscript_code/Figures/",plottype, "_above",threshold, ".tiff"), device = "tiff", width =6, height =10)
}

# for testing function 
df = tempmean
threshold = 22
plottype = "stationheatmap"


## Calculate percent in each region that is fully above threshold ------
temp_all_warm <- bind_rows(temps_warm %>% mutate(TempClass = "Warm Years (2014-2016)"),
                       tempmean %>% mutate(TempClass = "All Years"))
recov <- temp_all_warm %>%
  filter(month %in% c(4, 5, 6, 7, 8, 9)) %>%
  group_by(Region, TempClass) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  mutate(over95 = if_else(percent<1, "Recovery", "No Recovery"))%>%
  group_by(Region, TempClass, over95, total) %>%
  reframe(n = n()) %>%
  mutate(percent = round(n/total *100,1))

temprecov_summary <- recov %>% dplyr::select(-total, -n) %>% 
  pivot_wider(names_from = "TempClass", values_from = "percent") %>%
  filter(over95 == "No Recovery") %>%
  mutate(Difference = (`Warm Years (2014-2016)` - `All Years`),
         Days = Difference*365/100)

## Figures ---------------
## Functions for different cases
plot_percent(tempmean %>% filter(month > 3 & month < 11), 22, "stationheatmap")
plot_percent(tempmean%>% filter(month > 3 & month < 11), 25, "stationheatmap")
plot_percent(temps_warm%>% filter(month > 3 & month < 11), 22, "warmyears")
plot_percent(temps_warm%>% filter(month > 3 & month < 11), 25, "warmyears")
plot_percent(temps_cool%>% filter(month > 3 & month < 11), 22, "coolyears")
plot_percent(temps_cool%>% filter(month > 3 & month < 11), 25, "coolyears")

# This one used in manuscript
(plot_all_warm <- ggplot() + 
  geom_tile(data = temp_all_warm %>%filter(month>3 & month<11), aes(x = dummyDate, y = Station, fill = percent)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = c(0,0)) + 
  geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray40", linetype = "dashed", size = 0.8) + 
  facet_grid(Region~TempClass, scales = "free", space = "free")+ 
  scale_fill_viridis_c(option = "B", name = paste0("Proportion of Day\nabove ", threshold, "°C")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 13),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "top"))

ggsave("manuscript_code/Figures/Fig6_stations_above22_all_warm.png", device = "png", width =7.25, height =10)

(compare_warm_cool_barplot <- ggplot(recov, aes(Region, percent)) + 
  geom_col(aes(fill = over95), width = 0.8) +
  # geom_label(aes(Region, percent, label = percent)) + 
  # geom_text(aes(label = percent), position = position_stack(vjust = 0.5), size = 4.5) +
  facet_wrap(~TempClass, nrow = 2) +
  scale_fill_manual(values = c("gold1", "deepskyblue4")) + 
  labs(y = "Percent of Stations and Days\n(April-September)")+
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.title.x = element_blank()))

# Used in manuscript
ggsave(paste0("manuscript_code/Figures/FigA4_norecovery_barplot.png"), device = "png", width =6, height =7)





# Summary stats ---------------------

## Minimum -------------

ggplot() + 
  geom_tile(data = tempmean, aes(x = dummyDate, y = Station, fill = TempMin)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = c(0,0)) + 
  geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray40", linetype = "dashed", size = 0.8) + 
  facet_grid(Region~., scales = "free", space = "free")+ 
  scale_fill_viridis_c(option = "mako", name = "Minimum Temp") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 13),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "top")
ggsave("manuscript_code/Figures/stationsheatmap_min.tiff", device = "tiff", width =8, height =10)

## Mean -------------------
ggplot(tempmean, aes(x = dummyDate, y = Station, fill = Temp)) +
  facet_grid(Region~., scales = "free", space = "free")+ 
  geom_tile()+
  scale_fill_viridis_c(name = "Average")+
  geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray20", linetype = "dashed", linewidth = 0.8) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = c(0,0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 13),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "top")

ggsave("manuscript_code/Figures/stationsheatmap_mean.tiff", device = "tiff", width =8, height =10)

##  Max ------------------
ggplot(tempmean, aes(x = dummyDate, y = Station, fill = Tempmax)) +
  scale_fill_viridis_c(option = "C", name = "Max")+
  facet_grid(Region~., scales = "free", space = "free")+ 
  geom_tile()+
  geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray20", linetype = "dashed", linewidth = 0.8) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = c(0,0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 13),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "top")

ggsave(paste0("manuscript_code/Figures/TempMax_plot.png"), device = "png", width =8, height =10)

## Range ---------------------------------
ggplot(tempmean, aes(x = dummyDate, y = Station, fill = Tempmax-TempMin))+
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "turbo", name = "Range") +
  geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray20", linetype = "dashed", linewidth = 0.8) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = c(0,0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 13),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "top")

ggsave(paste0("manuscript_code/Figures/Range_plot.png"), device = "png", width =8, height =10)

## Plot for scaled temperature --------------
temp_scaled_long <- tempmean %>%
  dplyr::select(-Temp,-Tempmax, -TempMin, -percent, -percent25) %>%
  pivot_longer(cols = c("Tempmax_s", "Tempmin_s"), names_to = "Temp_class", values_to = "Temp_s")


ggplot(data = temp_scaled_long, aes(x = dummyDate, y = Station, fill = Temp_s)) +
  geom_tile()+
  geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray20", linetype = "dashed", linewidth = 0.8) + 
  scale_fill_viridis_c(option = "turbo", name = "Scaled Maximum Temp")+
  facet_grid(Region~Temp_class, scales = "free", space = "free")+ 
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = c(0,0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 13),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "top")

ggsave(paste0("manuscript_code/Figures/TempMax_scaled_plot.png"), device = "png", width =8, height =10)






## Maximum only

ggplot(tempmean, aes(x = dummyDate, y = Station, fill = Tempmax_s)) +
  geom_tile()+
  geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray20", linetype = "dashed", linewidth = 0.8) + 
  scale_fill_viridis_c(option = "turbo", name = "Scaled Maximum Temp")+
  facet_grid(Region~., scales = "free", space = "free")+ 
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = c(0,0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 13),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "top")

## Minimum only 

ggplot(tempmean, aes(x = dummyDate, y = Station, fill = Tempmin_s)) +
  geom_tile()+
  geom_vline(data = vlines, aes(xintercept = dummyDate), color = "gray20", linetype = "dashed", linewidth = 0.8) + 
  scale_fill_viridis_c(option = "C", name = "Scaled Minimum Temp")+
  facet_grid(Region~., scales = "free", space = "free")+ 
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = c(0,0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 13),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "top")






ggplot(filter(tempmean, Station %in% c("BDL", "RVB")),aes(x = julian, y = Station, fill = Tempmax-TempMin))+
  facet_grid(Region~., scales = "free", space = "free")+ geom_tile()+
  scale_fill_viridis_c(option = "turbo", name = "Range")


##############################################################


