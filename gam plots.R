# Gam Plots


library(lubridate)
library(tidyverse)

library(sf)
library(rspatial)

library(raster)
library(stars)






#Delta shapefile
load("deltabuff.RData")


#############################################################################
#plotting functions

raster_plot<-function(data, labels="All", lims = c(5,30)){
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    facet_wrap(~Date)+
    scale_fill_viridis_c(name="Temperature", na.value="white", option = "turbo",
                         limits=lims,
                         labels= function(x) ifelse((x/2)==as.integer(x/2), as.character(x), ""),
                         guide = guide_colorbar(direction="horizontal", title.position = "top", barwidth = 10, ticks.linewidth = 2,
                                                barheight=1, title.hjust=0.5, label.position="bottom", label.theme=element_text(size=12), 
                                                title.theme=element_text(size=13)))+
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "top")
}

raster_plot2<-function(data, date, labels="All", palette = "E"){
  data = data[,,,date]
  data = st_transform(data, crs = 32610)
  deltabuff = st_transform(deltabuff, crs = 32610)
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    # facet_wrap(~Date)+
    scale_fill_viridis_c(option = palette, name = NULL, na.value="white",  limits = c(0,6),
                         guide = guide_colorbar(direction="vertical",  barwidth = .5, ticks.linewidth = 2,
                                                barheight=5, title.hjust=0.5, label.theme=element_text(size=8)
                         ))+
    scale_x_continuous(breaks = c(-122.2, -121.8, -121.4, -121.0)) +
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "right")
}


raster_plot3<-function(data, date, labels="All", palette = "D" ){
  data = data[,,,date]
  data = st_transform(data, crs = 32610)
  deltabuff = st_transform(deltabuff, crs = 32610)
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    # facet_wrap(~Date)+
    scale_fill_viridis_c(option = palette, name=NULL, na.value="white", 
                         guide = guide_colorbar(direction="vertical",  barwidth = .5, ticks.linewidth = 2,
                                                barheight=5, title.hjust=0.5, label.theme=element_text(size=8)
                                                ))+
    scale_x_continuous(breaks = c(-122.2, -121.8, -121.4, -121.0)) +
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "right")
}

#load teh data with all the predictions
load("RasteredPreds12Aug2021.RData")
#maximum temperatures

JanMax = raster_plot3(rastered_preds, 1, palette = "B") #+ ggtitle("Max Temp - Jan")
ArpMax = raster_plot3(rastered_preds, 2, palette = "B")# + ggtitle("Max Temperature - Arp")
JulMax = raster_plot3(rastered_preds, 3, palette = "B")# + ggtitle("Max Temperature - Jul")
OctMax = raster_plot3(rastered_preds, 4, palette = "B")# + ggtitle("Max Temperature - Oct")
library(gridExtra)

#minimum temps
#limsM2 =  summary(rastered_predsmin$Prediction)[c("Min.", "Max.")]
#raster_plot(rastered_predsmin, lims = limsM2) + ggtitle("Min Temperature")
JanMin = raster_plot3(rastered_predsmin, 1)# + ggtitle("Min Temp - Jan")
ArpMin = raster_plot3(rastered_predsmin, 2)# + ggtitle("Min Temperature - Arp")
OctMin = raster_plot3(rastered_predsmin, 4)# + ggtitle("Min Temp - Oct")
JulMin = raster_plot3(rastered_predsmin, 3)# + ggtitle("Min Temp - Jul")

#average temperatures
#raster_plot(rastered_predsave, type = "Mean")# + ggtitle("Mean Temperature")
MeanJan = raster_plot3(rastered_predsave, 1, palette = "A")# + ggtitle("Mean Temp - Jan")
MeanArp = raster_plot3(rastered_predsave, 2, palette = "A")# + ggtitle("Mean Temperature - Sep")
MeanJul = raster_plot3(rastered_predsave, 3, palette = "A")# + ggtitle("Mean Temp - Jul")
MeanOct = raster_plot3(rastered_predsave, 4, palette = "A")# + ggtitle("Mean Temp - Oct")

#MeanJulb = raster_plot3(rastered_predsave, 1) + ggtitle("Mean Temp - Jul")
#MinJul
#MeanJulb

#temp range
#limsM2 =  summary(rastered_predsrange$Prediction)[c("Min.", "Max.")]
#raster_plot(rastered_predsrange, lims = c(0,6)) + ggtitle("daily temperature range")
RangeJan = raster_plot2(rastered_predsrange, 1, palette = "turbo")# + ggtitle("Range - Jan")
RangeArp = raster_plot2(rastered_predsrange, 2, palette = "turbo")# + ggtitle("daily temperature range -Arp")
RangeJul = raster_plot2(rastered_predsrange, 3, palette = "turbo")# + ggtitle("Range - Jul")
RangeOct = raster_plot2(rastered_predsrange, 4, palette = "turbo")# + ggtitle("Range - Jul")


library(gridExtra)


#Janlab = ggplot() + ylab("January") + 
#  theme(axis.title.y = element_text(face = "bold", size = 12), plot.background = element_blank())

library(grid)
allplots = grid.arrange( JanMin, JanMax, MeanJan, RangeJan, 
             ArpMin, ArpMax, MeanArp, RangeArp,
             JulMin, JulMax, MeanJul, RangeJul,
            OctMin, OctMax, MeanOct, RangeOct, nrow  =4)  #, 
            # left = textGrob(
            #   label = "October                July                  April                 January",
            #   gp = gpar(fontsize = 20), rot = 90,
            #   x = 0.5))
allplots
ggsave("MinMeanMax.svg", plot = allplots, device = "svg", width = 11, height = 8, units = "in")

margin = theme(plot.margin = unit(c(.5,1,.5,1), "cm"))
grid.arrange(MinJan + margin,  Minapr + margin, MinJul + margin,
              MinOct + margin,  nrow  =2)

grid.arrange( MeanJan + margin,  MeanApr + margin, MeanJul + margin,
              MeanOct + margin,  nrow  =2)


grid.arrange( JanM + margin,  aprM + margin, julM + margin,
              julN + margin,  nrow  =2)

grid.arrange( JanRange + margin,  ArpRange + margin, RangeJul + margin,
              RangeOct + margin,  nrow  =2)


ArpRange + geom_sf(data = stas)
RangeJul + geom_sf(data = stas)



mask <- delta %>%
st_transform(crs = 32610)%>% 
  st_bbox() %>% 
  st_as_sfc() %>%
  st_difference(st_transform(delta, 32610)) 

mask2 = st_as_sf(mask)

ggplot() + geom_sf(data = mask2, aes(geometry = x), color = "red", fill = NA) #+ geom_sf(data = delta)
plot(mask)

ArpRange + geom_sf(data = mask, fill = "blue", color = "red")

###############################################################
#TUCP stuff
#Data for summer temperatures

load("summer.RData")
limsS =  summary(out$Prediction)[c("Min.", "Max.")]
raster_plot(out, lims = limsS) + ggtitle("Max Temperature")

limsS2014 =  summary(rastered_preds2014$Prediction)[c("Min.", "Max.")]

raster_plot(rastered_preds2014, lims = limsS2014) + ggtitle("Max Temperature")
raster_plot(rastered_preds2015, lims = limsS2014) + ggtitle("Max Temperature")


