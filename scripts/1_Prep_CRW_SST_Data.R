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

# cores = detectCores()-2
# registerDoParallel(cores = cores)

nc_list = list.files(path = "G:/CRW_SST/", pattern = "\\.nc$", full.names = T); nc_list

df = stack(nc_list, varname = "sea_surface_temperature")
df <- df %>% rasterToPoints() %>% data.frame()
df <- df %>% select(-matches("00\\.00\\.00\\.1"))
df <- df %>% select(-matches("00\\.00\\.00\\.2"))
colnames(df)[3:dim(df)[2]] <- substring(colnames(df)[3:dim(df)[2]] , 1, 11)
df

# r <- foreach(isl = 1:length(nc_list), .combine = rbind, .packages = c("raster", "dplyr")) %dopar% {
#   
#   # isl = 3
#   
#   df = stack(nc_list[isl], varname = "sea_surface_temperature")
#   df <- df %>% rasterToPoints() %>% data.frame()
#   colnames(df)[3:dim(df)[2]] <- substring(colnames(df)[3:dim(df)[2]] , 1, 11)
#   df
#   
# }

monthly_CRW = as.data.frame(r)
monthly_CRW = df

plot(monthly_CRW[,1:2])

save(monthly_CRW, file = "G:/CRW_SST/CRW_1985-2022.RData")
