####################################################
### subsample shp files and turn them into RData ###
####################################################
rm(list = ls())

library(maptools)
library(rgdal)
library(maps)
library(sf)

isl <- readOGR("data/5km_buffer/ALLPacific_Sectors_Islands_5km_buffer.shp")
isl = recenter(isl); plot(isl); map(add = T)
isl <- rmapshaper::ms_simplify(isl, keep = 0.001, keep_shapes = F); plot(isl); map(add = T)
isl <- isl %>% st_as_sf()
save(isl, file = 'data/isl_sf_dataframe_0.001.RData') 

eez <- readOGR(dsn = "G:/GIS/eez/World_EEZ_v11_20191118_HR_0_360", layer = "eez_v11_0_360")
eez <- rmapshaper::ms_simplify(eez, keep = 0.001, keep_shapes = F); plot(eez)
eez <- eez %>% st_as_sf()
save('data/eez_sf_dataframe_0.001.RData') 

