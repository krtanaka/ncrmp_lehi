########################################################################################
### before you begin... download CRW SST 1982-2022 and save them on your drives      ###
### https://coastwatch.pfeg.noaa.gov/erddap/info/NOAA_DHW_monthly_Lon0360/index.html ###
########################################################################################

rm(list = ls())

library(terra)
library(sf)
library(dplyr)
library(readr)
library(doParallel)
library(data.table)
library(tidyr)
library(raster)

var = c("DHW", "BL")[2]

if (var == "DHW") nc_list_cw = list.files(path = "M:/Environmental_Data_Summary/Data_Download/Degree_Heating_Weeks_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = T)

if (var == "BL") nc_list_cw = list.files(path = "M:/Environmental_Data_Summary/Data_Download/Bleaching_Hotspot_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = T)

nc_list_cw

# Set up parallel processing
cores <- detectCores()
cl <- makeCluster(cores-2)
registerDoParallel(cl)

df <- foreach(i = 1:57, .combine = rbind, .packages = c("terra", "dplyr")) %dopar% {
  
  # i = 5
  
  df_i <- terra::rast(nc_list_cw[i])
  time_stamp = terra::time(df_i) %>% as.character() %>% substring(1, 10)
  df_i <- df_i %>% terra::as.data.frame(xy = T)
  colnames(df_i)[3:dim(df_i)[2]] = time_stamp
  
  df_i
  
}

# Stop parallel processing
stopCluster(cl); beepr::beep(2)

shp <- st_read(file.path("N:/GIS/Projects/CommonMaps/5km_buffer/ALLPacific_Sectors_Islands_5km_buffer.shp")) %>% as("Spatial") #World EEZ v10 0-360
# shp = recenter(shp)

CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(shp) <- CRS.new # proj4string(latlon) <- CRS.new

df$x = ifelse(df$x > 180, df$x - 360, df$x)

latlon = df[,c(1:2)]
coordinates(latlon) = ~x+y
proj4string(latlon) <- CRS.new
area <- over(latlon, shp)
area = as.data.frame(area[,c("Region", "ISLAND_CD", "SEC_NAME")])
colnames(area) = c("Region", "Island", "Sector")
df = cbind(area, df) %>% na.omit()
# crw_region_names = df[, c("Region", "x", "y")]
# df = df[ , -which(names(df) %in% "Region")]

df$x = ifelse(df$x < 0, df$x + 360, df$x)

df$mean = rowMeans(df[,6:dim(df)[2]])

save(df, file = paste0("outputs/CRW_", var, "_5km_coast.RData"))

load("outputs/CRW_DHW_5km_coast.RData")
load("outputs/CRW_BL_5km_coast.RData")

df %>% 
  filter(Region == unique(df$Region)[4]) %>%
  ggplot(aes(x, y, fill = mean)) +  
  geom_point(shape = 21, size = 3, alpha = 0.8) + 
  scale_fill_gradientn(colors = matlab.like(100)) + 
  coord_fixed()

