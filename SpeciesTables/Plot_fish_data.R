library(deltafish)
library(ggplot2)

create_fish_db()
surv <- open_survey()
fish <- open_fish()

surveys <- surv %>% 
  filter(Source %in% c("DJFMP", "Suisun", "BayStudy", "EDSM", "FMWT", "SKT", "TMM", "SLS", "STN")) %>% 
  select(SampleID, Source, Date, Latitude, Longitude)

fish_sp <- fish %>%
  filter(Taxa %in% "Ptychocheilus grandis")

stick <- fish %>%
  filter(Taxa %in% "Gasterosteus aculeatus")

df <- left_join(surveys, stick) %>% 
  collect() 

pikeminnow <- df %>%
  filter(Count>0)
ggplot2::ggplot(pikeminnow) + geom_density(aes(x = Length, color = Source)) + scale_x_continuous(limits = c(0, 550), breaks = c(seq(0, 550, by = 50))) + theme(axis.text.x = element_text(angle = 90))
ggplot(pikeminnow) + geom_point(aes(x = Length, y = Count, color = Source))+ 
  scale_x_continuous(limits = c(0, 500), breaks = seq((0, 500, by = 50)))

stickle <- df %>%
  filter(Count>0)

ggplot(stickle) + geom_density(aes(x = Length, , color = Source))+ scale_x_continuous(limits = c(0, 300), breaks = c(seq(0, 300, by = 25))) + theme(axis.text.x = element_text(angle = 90))
ggplot(stickle) + geom_point(aes(x = Length, y = Count, color = Source))
