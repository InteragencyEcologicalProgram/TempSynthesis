library(here)
library(readxl)
library(dplyr)
library(ggplot2)
native <- c("Chinook Salmon", "Green Sturgeon", "Sacramento Splittail", "White Sturgeon", "Longfin Smelt", "Delta Smelt", "Steelhead/Rainbow Trout", "Sacramento Blackfish", "Hitch", "Tule Perch", "Sacramento Pikeminnow", "Sacramento Sucker", "Threespine Stickleback", "Prickly Sculpin")

thresholds <- readxl::read_excel(here::here("SpeciesTables", "SpeciesThresholds.xlsx")) %>%
  mutate(Suboptimum_Upper = replace(Suboptimum_Upper, Suboptimum_Upper == "NA", NA),
        Tolerance_Upper = replace(Tolerance_Upper, Tolerance_Upper == "NA", NA)) %>%
  filter(Species != "Curleyleaf Pondweed") %>%
  # mutate(Species = case_when(Species == "STB" ~ "Striped Bass",
  #                     Species == "LFS" ~ "Longfin Smelt",
  #                     Species == "LMB" ~ "Largemouth Bass",
  #                     Species == "DSM" ~ "Delta Smelt",
  #                     Species == "Chinook" ~ "Chinook Salmon",
  #                     Species == "O. mykiss" ~ "Steelhead/RB Trout",
  #                     Species == "Hyacinth" ~ "Water Hyacinth",
  #                     Species == "Egeria" ~ "Brazilian Waterweed",
  #                     TRUE~as.character(Species)),
  mutate(Native = ifelse(Species %in% native, "native", "non-native")) %>%
  mutate(Suboptimum_Upper = as.numeric(Suboptimum_Upper),
         Tolerance_Upper = as.numeric(Tolerance_Upper))

# Make plots ------------------------------------------
(plot_subopt <- ggplot(thresholds) + 
    ggrepel::geom_text_repel(aes(x = Species, y = Suboptimum, colour = LifeStage, size = Native, label = Species)) +
   geom_hline(yintercept = 21, color = "orange", linetype = "dashed") + 
   geom_hline(yintercept = 25, color = "red", linetype = "dotted") +
   scale_color_manual(values = viridis::viridis(10, option = "turbo")[c(1,3,5,7, 9)]) + 
   scale_size_manual(values = c(2.5,4)) + 
  labs(y = "Suboptimum Temperature Threshold (◦C)", color = "Life Stage") + 
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "right"))

png("Figures/SuboptimumSpecies.png", width = 6.5, height = 4.5, units = "in", pointsize = 12, res = 300, family = "sans")
plot_subopt
dev.off()

(plot_tol <- ggplot(thresholds) + 
    ggrepel::geom_text_repel(aes(x = Species, y = Tolerance, colour = LifeStage, size = Native, label = Species)) + scale_color_manual(values = viridis::viridis(10, option = "turbo")[c(1,3,5,7, 9)]) +  
    scale_size_manual(values = c(2.5,4)) + labs(y = "Tolerance Temperature Threshold (◦C)", color = "Life Stage") + 
    theme_bw() +
    theme(axis.text.x = element_blank(),                                                      axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right"))

png("Figures/ToleranceSpecies.png", width = 6.5, height = 4.5, units = "in", pointsize = 12, res = 300, family = "sans")
plot_tol
dev.off()


# Long version --------------------------------------
thresholds_long <- thresholds %>% select(-Tolerance, -Suboptimum) %>% 
  rename(Tolerance = Tolerance_Upper, Suboptimum = Suboptimum_Upper) %>%
  tidyr::pivot_longer(cols = c(Suboptimum, Tolerance), names_to = "Threshold", values_to = "Temperature")

thresholds_long$Threshold <- factor(thresholds_long$Threshold, levels = c("Tolerance", "Suboptimum"))

(plot_both <- ggplot(thresholds_long) + 
    ggrepel::geom_text_repel(aes(x = Species, y = Temperature, colour = LifeStage, size = Native, label = Species), max.overlaps = 22,angle = 0) + 
    geom_hline(yintercept = 21, color = "orange", linetype = "dashed") + 
    geom_hline(yintercept = 25, color = "red", linetype = "dotted") +
    facet_grid(Threshold~., scales = "free") + 
    scale_color_manual(values = viridis::viridis(10, option = "turbo")[c(1,3,5,7, 9)]) +   
    scale_size_manual(values = c(2,3)) + 
    labs(y = "Temperature Threshold (◦C)", color = "Life Stage") + 
    theme_bw() +
    theme(axis.text.x = element_blank(),                                                      axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right",
          strip.text = element_text(size = 12)))


png("Figures/ThresholdsSpecies.png", width = 8.5, height = 6.5, units = "in", pointsize = 12, res = 300, family = "sans")
plot_both
dev.off()
