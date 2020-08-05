library(tidyverse)
library(lubridate)

depth <- readRDS("DepthAnalysis/Data/depthdata_heatstress_20200728.rds")

# Transfer this to RMarkdown!


# Summarize data
table <- depth %>%
  group_by(Station, WaterCol, Stress) %>%
  summarize(n = n(),
            days = n/24,
            start = first(Date), # first date in the dataset
            end = last(Date),
            span = as.numeric(end-start),
            prop = days/span) %>%
  filter(!is.na(Stress),
         WaterCol!="Middle")

# Order factors to be low - medium - high 
  # Look up code 

# Get data on the same timeline 
  # Still need to do this

# Start and End Dates
dates <- depth %>%
  group_by(Station, WaterCol) %>%
  summarize(start = first(Date),
            end = last(Date))

# Add variables for dates
depthB <-  depth %>%
  mutate(year = year(Date),
         month= month(Date),
         day = day(Date))

# Make a bar plot!
# ndays in each stress category, faceted into station 
ggplot(table, aes(x = Stress, y = prop)) + geom_col() +
  facet_grid(WaterCol~Station)

# Add stacked bar plot
ggplot(table, aes(x = Stress, y = days, fill = WaterCol)) + geom_col(position = "fill") +
  facet_wrap(~Station)

# Classify seasons
# Explore over time (years)
# How do results change if only 1+ day at different temperature thresholds?
  # Number of days extra in each category if 1+ day, 2+ day, vs. 3+ day
