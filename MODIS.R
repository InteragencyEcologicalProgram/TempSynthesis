#See if I can download some MODIS data

#I'm basing this on the MODIStsp package
# L. Busetto, L. Ranghetti (2016) MODIStsp: 
#An R package for automatic preprocessing of MODIS Land Products time series, 
#Computers & Geosciences, Volume 97, Pages 40-48, ISSN 0098-3004, 
#https://doi.org/10.1016/j.cageo.2016.08.020, URL: https://github.com/ropensci/MODIStsp.

library(tidyverse)
library(lubridate)
library(MODIStsp)

MODIStsp()


MODIStsp(
  gui = FALSE,
  out_folder = "/modisdata",
  out_folder_mod = "/modisdata2",
  selprod = "Surf_Ref_Daily_005dg (M*D09CMG)",
  quality_bandsel = "IntCM_aot",
  indexes_bandsel = "SR",
  user = "rkhartman" ,
  password = "H@ppy0625",
  start_date = "2020.06.01",
  end_date = "2020.06.2",
  verbose = FALSE,
  parallel = TRUE
)

MODIStsp_get_prodlayers("M*D09CMG")
