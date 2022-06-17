####################################################################################
### before you begin... download OISSTv2 1982-2021 and save them on your desktop ###
### https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html            ###
####################################################################################

rm(list = ls())

library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(readr)
library(doParallel)

cores = detectCores()-2
registerDoParallel(cores = cores)

nc_list = list.files(path = "/Users/kisei.tanaka/Desktop/SST_CRW_Monthly/Island_Level_Data/", pattern = "\\.nc$", full.names = T); nc_list

r <- foreach(isl = 1:length(nc_list), .combine = rbind, .packages = c("raster", "dplyr")) %dopar% {
  
  # isl = 1
  
  df = stack(nc_list[isl], varname = "sea_surface_temperature")
  df <- df %>% rasterToPoints() %>% data.frame()
  colnames(df)[3:dim(df)[2]] <- substring(colnames(df)[3:dim(df)[2]] , 1, 11)
  df
  
}

monthly_CRW = as.data.frame(r)
plot(monthly_CRW[,1:2])

save(monthly_CRW, file = "data/CRW_1985-2021.RData")
