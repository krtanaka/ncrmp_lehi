########################################################################################
### before you begin... download CRW SST 1982-2022 and save them on your drives      ###
### https://coastwatch.pfeg.noaa.gov/erddap/info/NOAA_DHW_monthly_Lon0360/index.html ###
########################################################################################

rm(list = ls())

library(raster)
library(terra)
library(colorRamps)
library(rnaturalearth)
library(sf)
# library(rgdal)
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
  dplyr::select(x, y)

df <- foreach(i = 1:468, .combine = cbind, .packages = c("terra", "dplyr", "raster")) %dopar% {
  
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

# Stop parallel processing
stopCluster(cl)

df = df[complete.cases(df), ]

plot(unique(df[,1:2]), pch = ".")

save(df, file = "G:/SST/CRW_SST/CRW_SST.RData")

load("G:/SST/CRW_SST/CRW_SST.RData")

shp <- st_read(file.path("N:/GIS/Projects/CommonMaps/5km_buffer/ALLPacific_Sectors_Islands_5km_buffer.shp")) %>% as("Spatial") #World EEZ v10 0-360
# shp = recenter(shp)

CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(shp) <- CRS.new # proj4string(latlon) <- CRS.new

df$x = ifelse(df$x > 180, df$x - 360, df$x)

latlon = df[,c(1:2)]
coordinates(latlon) = ~x+y
proj4string(latlon) <- CRS.new
area <- over(latlon, shp)
area = as.data.frame(area[,"Region"])
colnames(area)[1] = "Region"
df = cbind(area, df) %>% na.omit()
crw_region_names = df[, c("Region", "x", "y")]
df = df[ , -which(names(df) %in% "Region")]

df$x = ifelse(df$x < 0, df$x + 360, df$x)

save(df, file = "G:/SST/CRW_SST/CRW_SST_5km_coast.RData")
save(crw_region_names, file = "G:/SST/CRW_SST/crw_region_names.RData")
