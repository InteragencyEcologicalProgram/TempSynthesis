# Gam Plots


library(lubridate)
library(tidyverse)

library(sf)
library(rspatial)

library(raster)
library(stars)


#data for Jan-June
#load("RasteredPreds3MAY2021.RData")

#data for July-Dec
load("RasteredPreds5MAY2021.RData")

#Delta shapefile
load("deltabuff.RData")


#############################################################################
#plotting functions

raster_plot<-function(data, labels="All", type){
  if (type == "range") lims = c(0,6) else lims = c(5,30)
  data = st_crop(data, deltabuff)
  ggplot()+
    geom_stars(data=data)+
    facet_wrap(~Date)+
    scale_fill_viridis_c(name="Temperature", na.value="white",
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
raster_plot(rastered_preds, type = "Max") + ggtitle("Max Temperature")
raster_plot3(rastered_preds, 1) + ggtitle("Max Temperature - Jul")
raster_plot3(rastered_preds, 3) + ggtitle("Max Temperature - Sep")
raster_plot3(rastered_preds, 5) + ggtitle("Max Temperature - Nov")


#minimum temps
raster_plot3(rastered_predsmin, 1) + ggtitle("Min Temperature - Jul")
raster_plot3(rastered_predsmin, 3) + ggtitle("Min Temperature - Sep")
raster_plot3(rastered_predsmin, 5) + ggtitle("Min Temperature - Nov")

#average temperatures
raster_plot(rastered_predsave, type = "Mean") + ggtitle("Mean Temperature")
raster_plot3(rastered_predsave, 5) + ggtitle("Mean Temperature - Nov")
raster_plot3(rastered_predsave, 3) + ggtitle("Mean Temperature - Sep")
raster_plot3(rastered_predsave, 1) + ggtitle("Mean Temperature - Jul")

#temp range
raster_plot(rastered_predsrange) + ggtitle("daily temperature range")
raster_plot3(rastered_predsrange, 1) + ggtitle("daily temperature range - Jan")
raster_plot3(rastered_predsrange, 3) + ggtitle("daily temperature range - Mar")
raster_plot3(rastered_predsrange, 6) + ggtitle("daily temperature range - Jun")
raster_plot3(rastered_predsrange, 1) + ggtitle("daily temperature range - Jan")

#ok, something strange is going on there.