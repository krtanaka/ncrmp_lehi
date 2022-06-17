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

nc_list = list.files(path = "M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Island_Level_Data/", pattern = "\\.nc$", full.names = T); nc_list

monthly_CRW = NULL

for (isl in 1:length(nc_list)) {
  
  # isl = 1
  
  print(nc_list[isl])
  
  df = stack(nc_list[isl], varname = "analysed_sst")
  df <- df %>% rasterToPoints() %>% data.frame()
  colnames(df)[3:dim(df)[2]] <- substring(colnames(df)[3:dim(df)[2]] , 1, 11)
  month_vec = paste0(1985:2019, ".", sprintf("%02d", 1:12)); month_vec
  
  monthly_mean = NULL
  
  for (i in 1:length(month_vec)) {
    
    # i = 1
    df_i = select(df, contains(month_vec[i]))  
    df_i$mean = rowMeans(df_i)
    df_i = df_i$mean
    names(df_i) = month_vec[i]
    monthly_mean = cbind(monthly_mean, df_i)
    
  }
  
  monthly_mean = as.data.frame(monthly_mean)
  
  colnames(monthly_mean) = paste0("X", month_vec)
  
  df = cbind(df[,1:2], monthly_mean)
  
  rownames(df) <- NULL
  
  monthly_CRW = rbind(monthly_CRW, df)
  
}

save(monthly_CRW, file = "data/CRW_1985-2019.RData")
