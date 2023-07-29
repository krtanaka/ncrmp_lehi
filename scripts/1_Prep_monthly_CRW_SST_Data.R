########################################################################################
### before you begin... download CRW SST 1982-2022 and save them on your drives      ###
### https://coastwatch.pfeg.noaa.gov/erddap/info/NOAA_DHW_monthly_Lon0360/index.html ###
########################################################################################

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

nc_list = list.files(path = "G:/SST/CRW_SST/", pattern = "\\.nc$", full.names = T); nc_list

df = stack(nc_list, varname = "sea_surface_temperature")
df = df[[1:459]]
df <- df %>% rasterToPoints() %>% data.frame()
df <- df %>% select(-matches("00\\.00\\.00\\.2"))
colnames(df)[3:dim(df)[2]] <- substring(colnames(df)[3:dim(df)[2]] , 1, 8)
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

# monthly_CRW = as.data.frame(r)
df = df[complete.cases(df), ]

plot(unique(df[,1:2]), pch = ".")

save(df, file = "G:/CRW_SST/CRW_1985-2022.RData")
