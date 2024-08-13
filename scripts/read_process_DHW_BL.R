rm(list = ls())

library(terra)
library(sf)
library(dplyr)
library(data.table)
library(doParallel)
library(raster)
library(ggplot2)
library(colorRamps)

# Set up parallel processing
cores <- detectCores()
cl <- makeCluster(cores-2)
registerDoParallel(cl)

# Read shapefile once before the loop
shp <- st_read("N:/GIS/Projects/CommonMaps/5km_buffer/ALLPacific_Sectors_Islands_5km_buffer.shp") %>% as("Spatial") 
CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(shp) <- CRS.new

for (v in c("DHW", "BL")) {
  
  # v = "DHW"
  # v = "BL"
  
  nc_list_cw <- switch(v,
                       "DHW" = list.files(path = "M:/Environmental_Data_Summary/Data_Download/Degree_Heating_Weeks_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE),
                       "BL" = list.files(path = "M:/Environmental_Data_Summary/Data_Download/Bleaching_Alert_Area_CRW_Daily/Unit_Level_Data/", pattern = "\\.nc$", full.names = TRUE))
  
  nc_list_cw
  
  df <- foreach(i = 1:length(nc_list_cw), .combine = rbind, .packages = c("terra", "dplyr")) %dopar% {
    df_i <- terra::rast(nc_list_cw[i])
    time_stamp <- as.character(terra::time(df_i)) %>% substring(1, 10)
    df_i <- terra::as.data.frame(df_i, xy = TRUE) %>%
      dplyr::rename_with(~time_stamp, -c(x, y))
    df_i
  }
  
  df$x <- ifelse(df$x > 180, df$x - 360, df$x)
  latlon <- df[, c("x", "y")]
  coordinates(latlon) <- ~x+y
  proj4string(latlon) <- CRS.new
  area <- over(latlon, shp)
  df <- cbind(as.data.table(area[,c("Region", "ISLAND_CD")]), df) %>% na.omit()
  df$x <- ifelse(df$x < 0, df$x + 360, df$x)
  colnames(df)[1:2] = c("region", "island")
  
  # Compute mean
  df$mean <- rowMeans(df[, -(1:4), with = FALSE])
  
  save(df, file = paste0("outputs/CRW_", v, "_5km_coast.RData"))
  
  df %>%
    ggplot(aes(x, y, fill = mean)) +  
    geom_point(shape = 21, size = 3, alpha = 0.8) + 
    scale_fill_gradientn(colors = matlab.like(100)) + 
    facet_wrap(~region, scales = "free") + 
    ggtitle(v)
  
  df_time <- foreach(i = 5:(ncol(df)-1), .combine = rbind, .packages = c("data.table")) %dopar% {
    
    df_i <- df[, .(v = mean(as.numeric(.SD[[1]]), na.rm = TRUE)), by = .(region, island), .SDcols = i] %>%
      .[, t := colnames(df)[i]]
    
    df_i
    
  }
  
  df_time <- df_time %>%
    filter(t != "y") %>%
    mutate(t = as.Date(t),
           year = year(t),
           month = month(t)) %>%
    .[, .(v = mean(v)), by = .(year, month, region, island)]
  
  save(df_time, file = paste0("outputs/CRW_", v, "_5km_coast_time.RData"))
  
}

# Stop parallel processing
stopCluster(cl)
beepr::beep(2)

load("outputs/CRW_BL_5km_coast_time.RData")

df %>%
  .[, .(v = mean(v)), by = .(year, region)] %>%
  ggplot(aes(x = year, y = v, fill = v, group = region)) + 
  geom_line() +
  geom_point(shape = 21, size = 5) + 
  labs(x = "Year", y = v, color = "Island") + 
  facet_wrap(~region, scales = "free") + 
  scale_fill_gradientn(colors = matlab.like(100), "")
