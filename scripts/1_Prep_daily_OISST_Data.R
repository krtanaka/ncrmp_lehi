####################################################################################
### before you begin... download OISSTv2 1982-2022 and save them on your desktop ###
### https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html            ###
####################################################################################

rm(list = ls())

library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
# library(rgdal)
library(dplyr)
library(readr)

for (p in 1:4) {
  
  # p = 4
  
  # too big to save so save them in blocks
  if (p == 1) period = c(1982:1991)
  if (p == 2) period = c(1992:2001)
  if (p == 3) period = c(2002:2011)
  if (p == 4) period = c(2012:2022)
  
  # e = extent(211.125, 214.875, 58.625, 62.375) # Prince William Sound
  e = extent(143, 207, -16, 30) # pacific NCRMP region
  
  monthly_OISST = NULL
  
  for (y in period) {
    
    # y = 2022

    df = stack(paste0("G:/SST/OISST/sst.day.mean.", y, ".nc"), varname = "sst")
    df = crop(df, e)
    df <- df %>% rasterToPoints() %>% data.frame()
    colnames(df)[3:367] <- substring(colnames(df)[3:367] , 2, 8)
    month_vec = paste0(y, ".", sprintf("%02d", 1:12))
    monthly_mean = NULL
    
    for (i in 1:length(month_vec)) {
      
      # i = 1
      df_i = df[grepl(month_vec[i], names(df))]  
      df_i$mean = rowMeans(df_i)
      monthly_mean = cbind(monthly_mean, df_i$mean)
      
    }
    
    monthly_mean = as.data.frame(monthly_mean)
    colnames(monthly_mean) = paste0("X", y, ".", sprintf("%02d", 1:12))
    
    if (y %in% c(1982, 1992, 2002, 2012)) {
      
      df = cbind(df[,1:2], monthly_mean)
      monthly_OISST = df
      
    } else {
      
      monthly_OISST = cbind(monthly_OISST, monthly_mean)
    }
    
    print(y)
    
    save(monthly_OISST, file = paste0("G:/SST/OISST/OISST_", period[1], "-", period[length(period)], ".RData"))
    
  }
  
}

load("G:/SST/OISST/OISST_1982-1991.RData"); s1 = monthly_OISST
load("G:/SST/OISST/OISST_1992-2001.RData"); s2 = monthly_OISST
load("G:/SST/OISST/OISST_2002-2011.RData"); s3 = monthly_OISST
load("G:/SST/OISST/OISST_2012-2022.RData"); s4 = monthly_OISST

s = left_join(s1, s2)
s = left_join(s, s3)
s = left_join(s, s4)

monthly_OISST = s

save(monthly_OISST, file = "G:/SST/OISST/OISST.RData")
