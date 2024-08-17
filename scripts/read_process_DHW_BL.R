# analyzing how changes in LEHI is related to DHW or Bleaching indices

rm(list = ls())

library(terra)
library(sf)
library(dplyr)
library(data.table)
library(doParallel)
library(raster)
library(ggplot2)
library(colorRamps)
library(ggsci)
library(patchwork)

# Set up parallel processing
cores <- detectCores()
cl <- makeCluster(cores-2)
registerDoParallel(cl)

CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Read 5km shapefile 
isl_5km_buffer <- st_read("N:/GIS/Projects/CommonMaps/5km_buffer/ALLPacific_Sectors_Islands_5km_buffer.shp") %>% as("Spatial") 
isl_5km_buffer <- st_read("/mnt/giscm/5km_buffer/ALLPacific_Sectors_Islands_5km_buffer.shp") %>% as("Spatial") 
proj4string(isl_5km_buffer) <- CRS.new

# Read EEZ shapefile
# https://www.marineregions.org/downloads.php
# shp <- readOGR("G:/GIS/nm/World_12NM_v3_20191118_0_360/eez_12nm_v3_0_360.shp") # World 12 Nautical Miles Zone (Territorial Seas) v2 0-360
eez <- st_read(file.path("G:/GIS/eez/World_EEZ_v10_20180221_HR_0_360/World_EEZ_v10_2018_0_360.shp")) %>% as("Spatial") #World EEZ v10 0-360
eez <- st_read(file.path("/mnt/ktanaka/GIS/eez/World_EEZ_v10_20180221_HR_0_360/World_EEZ_v10_2018_0_360.shp")) %>% as("Spatial") #World EEZ v10 0-360
eez <- eez[eez$Pol_type != "Overlapping claim",]
eez <- eez[eez$Sovereign1 == "United States",]
proj4string(eez) <- CRS.new # proj4string(latlon) <- CRS.new

for (v in c("DHW", "BAA_7daymax", "BAA", "BH")) {
  
  # v = "DHW"
  # v = "BAA_7daymax"
  
  # nc_list_cw <- switch(v,
  #                      "DHW" = list.files(path = "M:/Environmental_Data_Summary/Data_Download/Degree_Heating_Weeks_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE),
  #                      "BAA_7daymax" = list.files(path = "M:/Environmental_Data_Summary/Data_Download/Bleaching_Alert_Area_7daymax_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE),
  #                      "BAA" = list.files(path = "M:/Environmental_Data_Summary/Data_Download/Bleaching_Alert_Area_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE),
  #                      "BH" = list.files(path = "M:/Environmental_Data_Summary/Data_Download/Bleaching_Hotspot_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE))
  
  nc_list_cw <- switch(v,
                       "DHW" = list.files(path = "/mnt/pmos/Data_Download/Degree_Heating_Weeks_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE),
                       "BAA_7daymax" = list.files(path = "/mnt/pmos/Data_Download/Bleaching_Alert_Area_7daymax_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE),
                       "BAA" = list.files(path = "/mnt/pmos/Data_Download/Bleaching_Alert_Area_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE),
                       "BH" = list.files(path = "/mnt/pmos/Data_Download/Bleaching_Hotspot_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE))
  
  nc_list_cw
  
  df <- foreach(i = 1:length(nc_list_cw), .combine = rbind, .packages = c("terra", "dplyr")) %dopar% {
    
    df_i <- terra::rast(nc_list_cw[i])
    time_stamp <- as.character(terra::time(df_i)) %>% substring(1, 10)
    
    df_i <- terra::as.data.frame(df_i, xy = TRUE) %>%
      dplyr::rename_with(~time_stamp, -c(x, y))
    
    df_i
    
  }
  
  # clip to 5km coastal waters
  df$x <- ifelse(df$x > 180, df$x - 360, df$x)
  latlon <- df[, c("x", "y")]
  coordinates(latlon) <- ~x+y
  proj4string(latlon) <- CRS.new
  area <- over(latlon, isl_5km_buffer)
  df <- cbind(as.data.table(area[,c("ISLAND_CD", "Region")]), df) %>% na.omit()
  df$x <- ifelse(df$x < 0, df$x + 360, df$x)
  colnames(df)[1:2] = c("island", "region")
  
  # attach EEZ shapefile
  latlon <- df[, c("x", "y")]
  coordinates(latlon) <- ~x+y
  proj4string(latlon) <- CRS.new
  area <- over(latlon, eez)
  df <- cbind(as.data.table(area[,c("Territory1")]), df) %>% na.omit()
  colnames(df)[1] = "eez"
  
  save(df, file = paste0("outputs/CRW_", v, "_5km_coast.RData"))
  
  df_time <- foreach(i = 6:(ncol(df)), .combine = rbind, .packages = c("data.table")) %dopar% {
    
    df_i <- df[, .(v = mean(as.numeric(.SD[[1]]), na.rm = TRUE)), by = .(region, island, eez), .SDcols = i] %>%
      .[, t := colnames(df)[i]]
    
    df_i
    
  }
  
  df_time <- df_time %>%
    # filter(t != "y") %>%
    mutate(t = as.Date(t),
           year = year(t),
           month = month(t)) %>%
    .[, .(v = mean(v)), by = .(year, month, region, island, eez)]
  
  save(df_time, file = paste0("outputs/CRW_", v, "_5km_coast_time.RData"))
  
}

# Stop parallel processing
stopCluster(cl)
beepr::beep(2)

# load regional DHW and BL values
load("outputs/CRW_BAA_5km_coast.RData"); df1 = df
load("outputs/CRW_BAA_7daymax_5km_coast.RData"); df2 = df
load("outputs/CRW_BH_5km_coast.RData"); df3 = df
load("outputs/CRW_DHW_5km_coast.RData"); df4 = df

common_columns <- intersect(names(df1), names(df2))
common_columns <- intersect(names(df3), common_columns)
common_columns <- intersect(names(df4), common_columns)

df1 <- df1[, ..common_columns]
df2 <- df2[, ..common_columns]
df3 <- df3[, ..common_columns]
df4 <- df4[, ..common_columns]

df1$index = "BAA"
df2$index = "BAA_7daymax"
df3$index = "BH"
df4$index = "DHW"

df_xy = rbind(df1, df2, df3, df4) %>% 
  dplyr::select(last_col(), everything())

df_xy$mean <- rowMeans(df_xy[, -(1:5), with = FALSE])

indexes <- c("BAA", "BAA_7daymax", "BH", "DHW")

plots <- list()

for (i in 1:length(indexes)) {
  
  p <- ggplot() +  
    geom_spatial_point(data = df_xy %>% filter(index == indexes[i]), 
                       aes(x, y, fill = mean), shape = 21, alpha = 0.7, crs = 4326) + 
    scale_fill_gradientn(colors = matlab.like(100), "") +
    annotation_map(map = map_data("world")) + 
    coord_sf(crs = st_crs(4326)) + 
    labs(x = "", y = "") + 
    ggtitle(indexes[i])
  
  plots[[i]] <- p
  
}

plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + 
  plot_layout(ncol = 2)

