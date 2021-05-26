# Gam Plots


library(lubridate)
library(tidyverse)

library(sf)
library(rspatial)

library(raster)
library(stars)



#data for July-Dec
load("RasteredPreds5MAY2021.RData")



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

raster_plot2<-function(data, date, labels="All"){
  data = data[,,,date]
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    # facet_wrap(~Date)+
    scale_fill_viridis_c(name="Temperature", na.value="white", breaks=seq(6,26,by=1), limits = c(5,30),
                         labels= function(x) ifelse((x/2)==as.integer(x/2), as.character(x), ""),
                         guide = guide_colorbar(direction="horizontal", title.position = "top", barwidth = 10, ticks.linewidth = 2,
                                                barheight=1, title.hjust=0.5, label.position="bottom", label.theme=element_text(size=12), 
                                                title.theme=element_text(size=13)))+
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "top")
}


raster_plot3<-function(data, date, labels="All"){
  data = data[,,,date]
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    # facet_wrap(~Date)+
    scale_fill_viridis_c(name="Temperature", na.value="white",
                         guide = guide_colorbar(direction="horizontal", title.position = "top", barwidth = 10, ticks.linewidth = 2,
                                                barheight=1, title.hjust=0.5, label.position="bottom", label.theme=element_text(size=12), 
                                                title.theme=element_text(size=13)))+
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "top")
}


raster_plot_range<-function(data, labels="All"){
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    # facet_wrap(~Date)+
    scale_fill_viridis_c(name="Temperature", na.value="white", breaks=seq(6,26,by=1), limits = c(0,8),
                         labels= function(x) ifelse((x/2)==as.integer(x/2), as.character(x), ""),
                         guide = guide_colorbar(direction="horizontal", title.position = "top", barwidth = 10, ticks.linewidth = 2,
                                                barheight=1, title.hjust=0.5, label.position="bottom", label.theme=element_text(size=12), 
                                                title.theme=element_text(size=13)))+
    coord_sf()+
    ylab("Latitude")+
    xlab("Longitude")+
    theme_bw() + theme(legend.position = "top")
}

#maximum temperatures
limsM =  summary(rastered_preds$Prediction)[c("Min.", "Max.")]
raster_plot(rastered_preds, lims = limsM) + ggtitle("Max Temperature")
julM = raster_plot3(rastered_preds, 1) + ggtitle("Max Temperature - Jul")
julS = raster_plot3(rastered_preds, 2) + ggtitle("Max Temperature - Nov")
julS = raster_plot3(rastered_preds, 3) + ggtitle("Max Temperature - Sep")
julN = raster_plot3(rastered_preds, 5) + ggtitle("Max Temperature - Nov")
library(gridExtra)
grid.arrange(julM, julS, julN)

#minimum temps
limsM2 =  summary(rastered_predsmin$Prediction)[c("Min.", "Max.")]
raster_plot(rastered_predsmin, lims = limsM2) + ggtitle("Min Temperature")
raster_plot3(rastered_predsmin, 1) + ggtitle("Min Temperature - Jul")
raster_plot3(rastered_predsmin, 3) + ggtitle("Min Temperature - Sep")
raster_plot3(rastered_predsmin, 5) + ggtitle("Min Temperature - Nov")

#average temperatures
raster_plot(rastered_predsave, type = "Mean") + ggtitle("Mean Temperature")
raster_plot3(rastered_predsave, 5) + ggtitle("Mean Temperature - Nov")
raster_plot3(rastered_predsave, 3) + ggtitle("Mean Temperature - Sep")
raster_plot3(rastered_predsave, 1) + ggtitle("Mean Temperature - Jul")

#temp range
limsM2 =  summary(rastered_predsrange$Prediction)[c("Min.", "Max.")]
raster_plot(rastered_predsrange, lims = limsM2) + ggtitle("daily temperature range")
raster_plot3(rastered_predsrange, 1) + ggtitle("daily temperature range - Jan")
raster_plot3(rastered_predsrange, 3) + ggtitle("daily temperature range - Mar")
raster_plot3(rastered_predsrange, 6) + ggtitle("daily temperature range - Jun")
raster_plot3(rastered_predsrange, 1) + ggtitle("daily temperature range - Jan")

#ok, something strange is going on there.


#data for Jan-June
load("RasteredPreds3MAY2021.RData")
JanM = raster_plot3(rastered_preds, 1) + ggtitle("Max Temperature - Jan")
MarM = raster_plot3(rastered_preds, 3) + ggtitle("Max Temperature - Mar")
MayM = raster_plot3(rastered_preds, 5) + ggtitle("Max Temperature - May")
library(gridExtra)
grid.arrange(JanM, MarM, MayM, julM, julS, julN, nrow  =2)


limsM =  summary(rastered_preds$Prediction)[c("Min.", "Max.")]
raster_plot(rastered_preds, lims = limsM) + ggtitle("Max Temperature")

#Data for summer temperatures

load("summer.RData")
limsS =  summary(out$Prediction)[c("Min.", "Max.")]
raster_plot(out, lims = limsS) + ggtitle("Max Temperature")

limsS2014 =  summary(rastered_preds2014$Prediction)[c("Min.", "Max.")]

raster_plot(rastered_preds2014, lims = limsS2014) + ggtitle("Max Temperature")
raster_plot(rastered_preds2015, lims = limsS2014) + ggtitle("Max Temperature")


