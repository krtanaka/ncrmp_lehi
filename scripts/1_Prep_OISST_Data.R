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

# too big to save so save them in blocks
period = c(1982:1991)
period = c(1992:2001)
period = c(2002:2011)
period = c(2012:2021)

e = extent(143, 207, -16, 30) #just pacific NCRMP region

monthly_OISST = NULL

for (y in period) {
  
  # y = 1992
  df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/sst.day.mean.", y, ".nc"), varname = "sst")
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
  
}

save(monthly_OISST, file = "data/OISST_1982-1991.RData")
save(monthly_OISST, file = "data/OISST_1992-2001.RData")
save(monthly_OISST, file = "data/OISST_2002-2011.RData")
save(monthly_OISST, file = "data/OISST_2012-2021.RData")
