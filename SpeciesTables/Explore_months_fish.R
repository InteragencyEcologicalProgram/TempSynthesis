absence_presence_all <- readRDS("SpeciesTables/absencepresence_lifestage.rds")

head(absence_presence_all)
glimpse(absence_presence_all)


hot_presence <- filter(absence_presence_all, Month %in% c(5,6,7,8,9,10))
whatswhere <- hot_presence %>%
  group_by(Month, Region, Taxa, LifeStage) %>%
  summarize(n = n())

ordered <- whatswhere %>%
  sf::st_drop_geometry() %>%
  mutate(Month = month(Month, label = TRUE, abbr = TRUE)) %>%
  arrange(Month, Region, n) %>%
  filter(n>100) %>%
  filter(Taxa %in% c("Morone saxatilis", "Pogonichthys macrolepidotus", "Oncorhynchus tshawytscha", "Oncorhynchus mykiss", "Hypomesus transpacificus", "Spirinchus thaleichthys", "Acipenser medirostris", "Acipenser transmontanus")) 
new <- ordered %>%
  tidyr::pivot_wider(names_from = Month, values_from = n) %>%
  mutate(across(.cols = May:Oct, ~ ifelse(!is.na(.x), Region, NA))) %>%
  arrange(Taxa, LifeStage)









whatswheretime <- hot_presence %>%
  group_by(Year, Month, Region, Taxa, LifeStage) %>%
  summarize(n = n())

time <- whatswheretime %>%
  arrange(Month, Region, n) %>%
  filter(n>100) %>%
  filter(Taxa %in% c("Morone saxatilis", "Pogonichthys macrolepidotus", "Oncorhynchus tshawytscha", "Oncorhynchus mykiss", "Hypomesus transpacificus", "Spirinchus thaleichthys", "Acipenser medirostris", "Acipenser transmontanus"))

ggplot()