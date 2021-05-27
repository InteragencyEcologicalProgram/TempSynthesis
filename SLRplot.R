#quick sea level rise plot

library(tidyverse)

SLR = read_csv("slr.csv")

ggplot(SLR, aes(x = Sedimentsupply, y = SeaLevelRise)) + geom_polygon(aes(fill = Sustainability), alpha = 0.5) +
  scale_fill_manual(values = c("red", "chartreuse4", "darkorange"), guide = NULL)+
  annotate("text", x = c(20, 60, 75), y = c(160, 100, 50), 
           label = c("Most Marshes \n Drown", "Most high marsh is lost. \n Trainsition to \n low marsh", "Most high  \n marsh remains"))+
  ylab("Sea Level Rise (cm)")+ xlab("Suspended Sediment (mg/L)")
