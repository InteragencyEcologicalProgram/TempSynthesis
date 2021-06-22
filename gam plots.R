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



#maximum temperatures
#data for July-Dec
load("RasteredPreds5MAY2021.RData")
rastered_predsrange$Prediction = rastered_preds$Prediction - rastered_predsmin$Prediction

julM = raster_plot3(rastered_preds, 1, palette = "B")# + ggtitle("Max Temperature - Jul")
julS = raster_plot3(rastered_preds, 2, palette = "B")# + ggtitle("Max Temperature - Nov")
julS = raster_plot3(rastered_preds, 3, palette = "B")# + ggtitle("Max Temperature - Sep")
julN = raster_plot3(rastered_preds, 5, palette = "B")# + ggtitle("Max Temperature - Nov")
julN = raster_plot3(rastered_preds, 4, palette = "B")# + ggtitle("Max Temperature - Oct")
library(gridExtra)

#minimum temps
#limsM2 =  summary(rastered_predsmin$Prediction)[c("Min.", "Max.")]
#raster_plot(rastered_predsmin, lims = limsM2) + ggtitle("Min Temperature")
MinJul = raster_plot3(rastered_predsmin, 1)# + ggtitle("Min Temperature - Jul")
MinSep = raster_plot3(rastered_predsmin, 3)# + ggtitle("Min Temperature - Sep")
MinNov = raster_plot3(rastered_predsmin, 5)# + ggtitle("Min Temperature - Nov")
MinOct = raster_plot3(rastered_predsmin, 4)# + ggtitle("Min Temperature - Oct")

#average temperatures
#raster_plot(rastered_predsave, type = "Mean")# + ggtitle("Mean Temperature")
MeanNov = raster_plot3(rastered_predsave, 5, palette = "A")# + ggtitle("Mean Temperature - Nov")
MeanSep = raster_plot3(rastered_predsave, 3, palette = "A")# + ggtitle("Mean Temperature - Sep")
MeanJul = raster_plot3(rastered_predsave, 1, palette = "A")# + ggtitle("Mean Temperature - Jul")
MeanOct = raster_plot3(rastered_predsave, 4, palette = "A")# + ggtitle("Mean Temperature - Oct")

#temp range
#limsM2 =  summary(rastered_predsrange$Prediction)[c("Min.", "Max.")]
#raster_plot(rastered_predsrange, lims = c(0,6)) + ggtitle("daily temperature range")
RangeJul = raster_plot2(rastered_predsrange, 1, palette = "turbo")# + ggtitle("daily temperature range - Jul")
RangeSep = raster_plot2(rastered_predsrange, 3, palette = "turbo")# + ggtitle("daily temperature range - Sep")
RangeOct = raster_plot2(rastered_predsrange, 4, palette = "turbo")# + ggtitle("daily temperature range - Oct")


#data for Jan-June
load("RasteredPreds3MAY2021.RData")
rastered_predsrange$Prediction = rastered_preds$Prediction - rastered_predsmin$Prediction

JanM = raster_plot3(rastered_preds, 1, palette = "B") + ggtitle("Max Temp")
MarM = raster_plot3(rastered_preds, 3)# + ggtitle("Max Temperature - Mar")
aprM = raster_plot3(rastered_preds, 4, palette = "B")# + ggtitle("Max Temperature - Apr")
library(gridExtra)

MeanJan = raster_plot3(rastered_predsave, 1, palette = "A") + ggtitle("Mean Temp")
MeanApr = raster_plot3(rastered_predsave, 4, palette = "A")# + ggtitle("Mean Temperature - Apr")


MinJan = raster_plot3(rastered_predsmin, 1) + ggtitle("Minimum Temp")
MinMar = raster_plot3(rastered_predsmin, 3) #+ ggtitle("Min Temperature - Mar")
Minapr = raster_plot3(rastered_predsmin, 4) #+ ggtitle("Min Temperature - Apr")


JanRange = raster_plot2(rastered_predsrange, 1, palette = "turbo") + ggtitle("Temp Range")
ArpRange= raster_plot2(rastered_predsrange, 4, palette = "turbo") #+ ggtitle("daily temperature range - Apr")


Janlab = ggplot() + ylab("January") + 
  theme(axis.title.y = element_text(face = "bold", size = 12), plot.background = element_blank())

library(grid)
grid.arrange( MinJan, JanM, MeanJan, JanRange, 
             Minapr, aprM, MeanApr, ArpRange,
             MinJul, julM, MeanJul, RangeJul,
            MinOct, julN, MeanOct, RangeOct, nrow  =4, 
            left = textGrob(
              label = "October                July                  April                 January",
              gp = gpar(fontsize = 20), rot = 90,
              x = 0.5))

ArpRange + geom_sf(data = stas)
RangeJul + geom_sf(data = stas)
###############################################################
#TUCP stuff
#Data for summer temperatures

load("summer.RData")
limsS =  summary(out$Prediction)[c("Min.", "Max.")]
raster_plot(out, lims = limsS) + ggtitle("Max Temperature")

limsS2014 =  summary(rastered_preds2014$Prediction)[c("Min.", "Max.")]

raster_plot(rastered_preds2014, lims = limsS2014) + ggtitle("Max Temperature")
raster_plot(rastered_preds2015, lims = limsS2014) + ggtitle("Max Temperature")


