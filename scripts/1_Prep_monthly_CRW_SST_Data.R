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
library(data.table)

# cores = detectCores()-2
# registerDoParallel(cores = cores)

# I had to download CRW SST files from both Coastwatch and Oceanwatch URLs separately. The Coastwatch URL was missing files for February 2022 and November 2023, so I obtained them from the Oceanwatch URLs. - KRT 2024/01/12

# CoastWatch files
nc_list_cw = list.files(path = "G:/SST/CRW_SST/coastwatch/", pattern = "\\.nc$", full.names = T); nc_list_cw

# Set up parallel processing
cores <- detectCores()
cl <- makeCluster(cores/2)
registerDoParallel(cl)

e = extent(143, 207, -16, 30) # pacific NCRMP region

# set base lat lon points because of potential mismatch later
base_latlon = rast(nc_list_cw[1], subds = "sea_surface_temperature") %>% 
  rotate(left = FALSE) %>% 
  crop(e) %>% 
  terra::as.data.frame(xy = T) %>% 
  select(x, y)

# stacked separately because of changes in crs half way
df1 <- foreach(i = 1:430, .combine = cbind, .packages = c("terra", "dplyr", "raster")) %dopar% {
  
  # i = 1
  
  df_i = rast(nc_list_cw[i], subds = "sea_surface_temperature")
  df_i <- rotate(df_i, left = FALSE) 
  df_i = crop(df_i, e)
  df_i <- df_i %>% terra::as.data.frame(xy = T)
  df_i <- df_i %>% dplyr::select(-matches("00\\.00\\.00\\.2"))
  colnames(df_i)[3] <- paste0("X", substring(nc_list_cw[i], 54, 59))
  
  if (i != 1) {
  df_i = left_join(base_latlon, df_i)
  df_i = df_i[,3] %>% as.data.frame()
  colnames(df_i)[1] <- paste0("X", substring(nc_list_cw[i], 54, 59))
  }
  
  df_i
  
}

df2 <- foreach(i = 431:466, .combine = cbind, .packages = c("terra", "dplyr", "raster")) %dopar% {
  
  # i = 431
  
  df_i = rast(nc_list_cw[i], subds = "sea_surface_temperature")
  df_i <- rotate(df_i, left = FALSE) 
  df_i = crop(df_i, e)
  df_i <- df_i %>% terra::as.data.frame(xy = T)
  df_i <- df_i %>% dplyr::select(-matches("00\\.00\\.00\\.2"))
  colnames(df_i)[3] <- paste0("X", substring(nc_list_cw[i], 54, 59))
  
  if (i != 431) {
    df_i = left_join(base_latlon, df_i)
    df_i = df_i[,3] %>% as.data.frame()
    colnames(df_i)[1] <- paste0("X", substring(nc_list_cw[i], 54, 59))
  }
  
  df_i
  
}

# Stop parallel processing
stopCluster(cl)



# monthly_CRW = as.data.frame(r)
df = df[complete.cases(df), ]

plot(unique(df[,1:2]), pch = ".")

save(df, file = "G:/CRW_SST/CRW_SST.RData")
