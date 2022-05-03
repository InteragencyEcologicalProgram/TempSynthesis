# Calculate max temps each species observed

# Get data------------------------------------------------
species <- c("Menidia beryllina", "Menidia audens", "Morone saxatilis", "Pogonichthys macrolepidotus", "Micropterus salmoides", "Oncorhynchus tshawytscha", "Oncorhynchus mykiss", "Hypomesus transpacificus", "Spirinchus thaleichthys", "Hypomesus nipponensis", "Acipenser medirostris", "Acipenser transmontanus", "Gasterosteus aculeatus", "Cottus asper", "Ptychocheilus grandis", "Catostomus occidentalis", "Lavinia exilicauda", "Orthodon microlepidotus", "Hysterocarpus traskii")

yolosubset <- c("Inland Silverside", "Striped Bass", "Splittail", "Largemouth Bass", "Longfin Smelt", "Delta Smelt", "Wakasagi", "Chinook Salmon", "Rainbow / Steelhead Trout", "White Sturgeon", "Green Sturgeon", "Hitch", "Prickly Sculpin", "Sacramento Blackfish", "Sacramento Sucker", "Sacramento Pikeminnow", "Tule Perch", "Threespine Stickleback")

# Yolo
#yolo_id <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=015e494911cf35c90089ced5a3127334")
yolo_file <- contentid::resolve("hash://sha256/e0dc10d7f36cfc5ac147956abb91f24f2b2df9f914a004bbf1e85e7d9cf52f41")
#yolo_stations_id <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=6a82451e84be1fe82c9821f30ffc2d7d")
yolo_stations_file <- contentid::resolve("hash://sha256/e25498ffc0208c3ae0e31a23204b856a9309f32ced2c87c8abcdd6f5cef55a9b")
#yolo_taxonomy_id <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=0532048e856d4bd07deea11583b893dd")
yolo_taxonomy_file <- contentid::resolve("hash://sha256/1473de800f3c5577da077507fb006be816a9194ddd417b1b98836be92eaea49d")
yolo_taxonomy <- readr::read_csv(yolo_taxonomy_file) %>%
  mutate(Taxa = paste(Genus, Species)) %>%
  select(CommonName, Taxa) %>%
  mutate(Taxa = replace(Taxa, Taxa == "Menidia beryllina", "Menidia audens"),
         Taxa = replace(Taxa, Taxa == "Hysterocarpus traski", "Hysterocarpus traskii"))
yolo_fish <- readr::read_csv(yolo_file) %>%
  filter(CommonName %in% yolosubset) %>%
  left_join(yolo_taxonomy)

yolo <- yolo_fish %>%
  mutate(SampleID = paste("YBFMP", 1:nrow(.)),
         Notes_catch = NA,
         SampleDate = lubridate::mdy(SampleDate), 
         Source = "YBFMP")%>%
  select(SampleID, Source, Date = SampleDate, StationCode, WaterTemperature, Length =  ForkLength,
         Count, Notes_catch, Taxa) 

# Integrated
library(deltafish)

create_fish_db()
surv <- open_survey()
fish <- open_fish()

surveys2 <- surv %>% 
  filter(Source %in% c("DJFMP", "Suisun", "BayStudy", "EDSM", "FMWT", "SKT", "TMM", "SLS", "STN")) %>% 
  select(SampleID, Source, Date, Latitude, Longitude, Temp_surf)

fish_sp2 <- fish %>%
  filter(Taxa %in% species)

df2 <- left_join(surveys2, fish_sp2) %>% 
  collect() 

df3 <- df2 %>%
  mutate(Date = lubridate::ymd(Date)) %>%
  rename(WaterTemperature = Temp_surf)


# Salvage
salvage_file <- contentid::resolve("hash://sha256/7e6d32127c22411ca5089d33a6444223302f05983035b5f3966c629a63f1d636")

salvage2 <- readRDS(salvage_file) %>%
  dplyr::mutate(Taxa = paste(Genus, Species),
                Source = "Salvage",
                SampleID = paste(Source, SampleRowID),
                Region = "South & Central",
                WaterTemperature = (WaterTemperature -32) * 5/9) %>%
  dplyr::filter(Taxa %in% species,
                !is.na(ForkLength)) %>%
  dplyr::select(Source, SampleID,
                Date = SampleDate,
                Region,
                Count, 
                WaterTemperature,
                Length=ForkLength,
                LengthFrequency, Taxa, CommonName) 


# Combine data and add life stage info----------------------------------
combine <- bind_rows(yolo, salvage2, df3)
fishpresent <- combine %>% filter(Count > 0,
                                  Length >0, WaterTemperature>0) %>%
  mutate(Taxa = replace(Taxa, Taxa == "Hysterocarpus traski", "Hysterocarpus traskii")) %>%
  mutate(Taxa = replace(Taxa, Taxa == "Menidia beryllina", "Menidia audens")) %>%
  mutate(LifeStage = case_when(Taxa == "Morone saxatilis" & Length < 20~"Larvae",
                               Taxa == "Morone saxatilis" & Length >= 20 & Length < 350~"Juvenile",
                               Taxa == "Morone saxatilis" & Length >= 350 ~ "Adult",
                               Taxa == "Micropterus salmoides" & Length < 25 ~ "Larvae",
                               Taxa == "Micropterus salmoides" & Length >= 25 & Length < 180 ~ "Juvenile",
                               Taxa == "Micropterus salmoides" & Length >= 180 ~ "Adult",
                               Taxa == "Pogonichthys macrolepidotus" & Length < 20.2 ~ "Larvae",
                               Taxa == "Pogonichthys macrolepidotus" & Length >= 20.2 & Length < 170 ~ "Juvenile",
                               Taxa == "Pogonichthys macrolepidotus" & Length >= 170 ~ "Adult",
                               Taxa == "Menidia audens" & Length < 9 ~ "Larvae",
                               Taxa == "Menidia audens" & Length >=9 & Length < 50~"Juvenile",
                               Taxa == "Menidia audens" & Length >= 50 ~ "Adult",
                               Taxa %in% c("Hypomesus transpacificus", "Hypomesus nipponensis") & Length <20 ~ "Larvae",
                               Taxa %in% c("Hypomesus transpacificus", "Hypomesus nipponensis") & Length >=20 & Length <59 ~ "Juvenile",
                               Taxa %in% c("Hypomesus transpacificus", "Hypomesus nipponensis") & Length >=59 ~ "Adult",
                               Taxa == "Spirinchus thaleichthys" & Length <20 ~ "Larvae",
                               Taxa == "Spirinchus thaleichthys" & Length >=20 & Length <80 ~ "Juvenile",
                               Taxa == "Spirinchus thaleichthys" & Length>=80 ~ "Adult",
                               Taxa == "Acipenser medirostris" & Length <20 ~ "Larvae",
                               Taxa == "Acipenser medirostris" & Length >=20 & Length <=1500 ~ "Juvenile",
                               Taxa == "Acipenser medirostris" & Length >1500 ~ "Adult",
                               Taxa == "Acipenser transmontanus" & Length <100 ~ "Larvae",
                               Taxa == "Acipenser transmontanus" & Length >=100 & Length <=1050 ~ "Juvenile",
                               Taxa == "Acipenser transmontanus" & Length >1050 ~ "Adult",
                               Taxa == "Oncorhynchus tshawytscha"  & Length <35 ~ "Larvae",
                               Taxa == "Oncorhynchus tshawytscha" & Length >=35 & Length <=700 ~ "Juvenile",
                               Taxa == "Oncorhynchus tshawytscha" & Length >700 ~ "Adult",
                               Taxa == "Oncorhynchus mykiss"  & Length <25 ~ "Larvae",
                               Taxa == "Oncorhynchus mykiss" & Length >=25 & Length <=390 ~ "Juvenile",
                               Taxa == "Oncorhynchus mykiss" & Length >390 ~ "Adult",
                               Taxa == "Orthodon microlepidotus" & Length < 250 ~ "Juvenile-Larvae",
                               Taxa == "Orthodon microlepidotus" & Length >=250  ~ "Adult",
                               Taxa == "Lavinia exilicauda" & Length <150 ~ "Juvenile-Larvae",
                               Taxa == "Lavinia exilicauda" & Length >=150 ~ "Adult",
                               Taxa == "Hysterocarpus traskii" & Length <100 ~ "Juvenile-Larvae",
                               Taxa == "Hysterocarpus traskii" & Length >=100 ~ "Adult",
                               Taxa == "Ptychocheilus grandis" & Length <=220 ~ "Juvenile-Larvae",
                               Taxa == "Ptychocheilus grandis" & Length >220 ~ "Adult",
                               Taxa == "Catostomus occidentalis" & Length < 14 ~ "Larvae",
                               Taxa == "Catostomus occidentalis" & Length >=14 & Length <200 ~ "Juvenile",
                               Taxa == "Catostomus occidentalis" & Length >=200 ~ "Adult",
                               Taxa == "Gasterosteus aculeatus" & Length < 41 ~ "Juvenile-Larvae",
                               Taxa == "Gasterosteus aculeatus" & Length >=41 ~ "Adult",
                               Taxa == "Cottus asper" & Length <40 ~ "Juvenile-Larvae",
                               Taxa == "Cottus asper" & Length >=40 ~ "Adult",
                               TRUE ~ as.character("Undifferentiated")))

# Write file ---------------------------------------------------------
# saveRDS(fishpresent, "SpeciesTables/SurveyTempsAllData.rds")

# Look at high temps
hightemps <- fishpresent %>%
  filter(WaterTemperature >25)

hightempSummary <- hightemps %>%
  mutate(Taxa_LS = paste(Taxa, LifeStage)) %>% 
  dplyr::select(-Length, -Count) %>%
  distinct() %>%
  group_by(WaterTemperature, Taxa_LS) %>%
  summarize(n = n())

ggplot(hightempSummary, aes(x = WaterTemperature, y = Taxa_LS, color = n)) + geom_point() + scale_color_viridis()  

# Plot  --------------------------------
plot <- ggplot(fishpresent) + geom_boxplot(aes(x = LifeStage, y = WaterTemperature, fill = LifeStage)) + facet_wrap(~Taxa) + theme(legend.position = "top",
                                                                                                                                                      axis.text.x = element_text(angle = 90)) 
plotV <- ggplot(fishpresent) + 
  geom_hline(yintercept = 21, color = "red") +
  geom_hline(yintercept = 25, color = "tan", linetype = "longdash")+
  geom_violin(aes(x = LifeStage, y = WaterTemperature, fill = LifeStage)) + facet_wrap(~Taxa) + theme(legend.position = "top",
                                                                                                                                   axis.text.x = element_text(angle = 90))
png("Figures/FieldTemps_Fish3.png", width = 9.5, height = 6, units = "in", res = 300)
plotV
dev.off()

# Make table for publication ---------------------
TempTable <- fishpresent %>%
  group_by(Taxa, LifeStage) %>%
  summarize(meanTemp = round(mean(WaterTemperature, na.rm = TRUE),1),
            q3 = round(quantile(WaterTemperature, 0.75, na.rm = TRUE),1),
            maxTemp = round(max(WaterTemperature, na.rm = TRUE),1),
            sd = round(sd(WaterTemperature, na.rm =TRUE),1),
            n = n()) %>%
  arrange(Taxa, LifeStage)

# ----- Write table
write.csv(TempTable, "SpeciesTables/surveymaxtemps_v3.csv", row.names = FALSE)
