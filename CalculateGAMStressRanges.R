library(tidyverse)
library(sf)
library(stars)

# Warp to the right CRS
rastered_preds21abin[[1]] <- st_warp(rastered_preds21abin[[1]], crs = 32610)
rastered_preds21abin[[2]] <- st_warp(rastered_preds21abin[[2]], crs = 32610)
rastered_preds25abin[[1]] <- st_warp(rastered_preds25abin[[1]], crs = 32610)
rastered_preds25abin[[2]] <- st_warp(rastered_preds25abin[[2]], crs = 32610)
rastered_preds21NRabin[[1]] <- st_warp(rastered_preds21NRabin[[1]], crs = 32610)
rastered_preds21NRabin[[2]] <- st_warp(rastered_preds21NRabin[[2]], crs = 32610)
deltabuff <- st_transform(deltabuff, crs = 32610)

# Crop each dataset
Avg21 <- rastered_preds21abin[[1]] %>% sf::st_crop(deltabuff)
Anom21 <- rastered_preds21abin[[2]] %>% sf::st_crop(deltabuff)
Avg25 <- rastered_preds25abin[[1]] %>% sf::st_crop(deltabuff)
Anom25 <- rastered_preds25abin[[2]] %>% sf::st_crop(deltabuff)
NRAvg21 <- rastered_preds21NRabin[[1]] %>% sf::st_crop(deltabuff)
NRAnom21 <- rastered_preds21NRabin[[2]] %>% sf::st_crop(deltabuff)

save(Avg21, Anom21, Avg25, Anom25, NRAvg21, NRAnom21,raster25Diff_crop, raster21Diff_crop, raster21NRDiff_crop, file= "cropped_datasets.RData")
load("cropped_datasets.RData")

# Make dataframes
df_Avg21 <- Avg21 %>% as.data.frame %>% mutate(Model = "Avg21")
df_Anom21 <- Anom21 %>% as.data.frame %>% mutate(Model = "Anom21")
df_Avg25 <- Avg25 %>% as.data.frame %>% mutate(Model = "Avg25")
df_Anom25 <- Anom25 %>% as.data.frame %>% mutate(Model = "Anom25")
df_NRAvg21 <- NRAvg21 %>% as.data.frame %>% mutate(Model = "NRAvg21")
df_NRAnom21 <- NRAnom21 %>% as.data.frame %>% mutate(Model = "NRAnom21")
df_diff21 <- raster21Diff_crop %>% as.data.frame %>% mutate(Model = "Diff21")
df_diff25 <- raster25Diff_crop %>% as.data.frame %>% mutate(Model = "Diff25")
df_NRdiff21 <- raster21Diff_crop %>% as.data.frame %>% mutate(Model = "NRDiff21")

# Calculate ranges
All <- rbind(df_Avg21, df_Anom21, df_Avg25, df_Anom25, df_NRAvg21, df_NRAnom21, df_diff21, df_diff25, df_NRdiff21)
Ranges <- All %>% group_by(Model) %>% summarize(min = min(Prediction2, na.rm = TRUE), max = max(Prediction2, na.rm = TRUE))
