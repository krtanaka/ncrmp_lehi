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
library(patchwork)
library(tidyr)
library(ggplot2)

# cores = detectCores()-2
# registerDoParallel(cores = cores)

# I had to download CRW SST files from both Coastwatch and Oceanwatch URLs separately. The Coastwatch URL was missing files for February 2022 and November 2023, so I obtained them from the Oceanwatch URLs. - KRT 2024/01/12

# CoastWatch files
nc_list_cw = list.files(path = "G:/SST/CRW_SST/coastwatch/", pattern = "\\.nc$", full.names = T); nc_list_cw
nc_list_cw = list.files(path = "/mnt/ktanaka/SST/CRW_SST/coastwatch/", pattern = "\\.nc$", full.names = T); nc_list_cw

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

df$x = ifelse(df$x < 0, df$x + 360, df$x)
crw_region_names = df[, c("Region", "x", "y")]
df = df[ , -which(names(df) %in% "Region")]

save(df, file = "outputs/CRW_SST_5km_coast.RData")
save(crw_region_names, file = "outputs/CRW_Region_Names.RData")

#plot
load("outputs/CRW_SST_5km_coast.RData")
load("outputs/CRW_Region_Names.RData")

ggmap::register_google("AIzaSyDpirvA5gB7bmbEbwB1Pk__6jiV4SXAEcY")

df = left_join(crw_region_names, df)
df$mean = rowMeans(df[,3:470])

df_i <- df %>%
  select(x, y, mean) %>% 
  filter(x >= 201.5 & x <= 202.5 & y >= 21.2 & y <= 21.75) %>% 
  mutate(x = ifelse(x > 180, x - 360, x)) %>% 
  st_as_sf(coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

coordinates <- st_coordinates(df_i)
coords_df <- as.data.frame(coordinates)
min_lat <- min(coords_df$Y)
max_lat <- max(coords_df$Y)
min_lon <- min(coords_df$X)
max_lon <- max(coords_df$X)

map = ggmap::get_map(#location = c(mean_lon, mean_lat),
  location =  c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat),
  maptype = "satellite",
  zoom = 10,
  force = T, 
  color = "bw")

(ggmap(map, darken = c(0.1, "black")) + 
    geom_sf(data = df_i, aes(fill = mean), size = 10, shape = 22, inherit.aes = F) + 
    scale_color_gradientn(colors = matlab.like(100), name = "deg C") + 
    scale_fill_gradientn(colors = matlab.like(100), name = "deg C"))

ggsave(last_plot(), file = "outputs/crw_sst_Oahu.png", width = 6, height = 6, bg = "transparent")

plots = list()

third_region <- unique(df$Region)[3]

for (r in unique(df$Region)) {
  
  p =  df %>% 
    filter(Region == r) %>% 
    select(x, y, mean) %>% 
    ggplot(aes(x, y, fill = mean)) + 
    geom_point(shape = 21, show.legend = r == third_region) + 
    scale_color_gradientn(colors = matlab.like(100), name = "deg C", limits = c(23.35, 29.13)) + 
    scale_fill_gradientn(colors = matlab.like(100), name = "deg C", limits = c(23.35, 29.13)) + 
    labs(x = "", y = "") + 
    coord_fixed() + 
    ggtitle(r)
  
  print(p)
  ggsave(last_plot(), file = paste0("outputs/crw_sst_map_", r, ".png"), width = 6)
  
  plots[[r]] = p 
}

plots[[5]] + (plots[[1]] / plots[[2]] / plots[[4]]) + plots[[3]]

ggsave(last_plot(), file = "outputs/crw_sst_map.png", width = 12, height = 6, bg = "transparent")

df = left_join(crw_region_names, df)

# Reshape the data from wide to long format, ensuring only the date columns are selected
date_columns <- grep("^X\\d+", names(df), value = TRUE)

df_long <- df %>%
  pivot_longer(cols = all_of(date_columns), 
               names_to = "Date", 
               values_to = "Value") %>%
  mutate(Date = as.Date(paste0(substr(Date, 2, 7), "01"), format = "%Y%m%d")) %>%
  mutate(Year = format(Date, "%Y"))  # Extract year from Date

# Aggregate by Region and Year
df_aggregated <- df_long %>%
  group_by(Region, Year) %>%
  summarize(MeanValue = mean(Value, na.rm = TRUE)) %>%
  ungroup()

library(grid) # for unit()

# Calculate slopes for each region
slope_df <- df_aggregated %>%
  group_by(Region) %>%
  summarize(slope = coef(lm(MeanValue ~ as.numeric(Year)))[2])

ggplot(df_aggregated, aes(x = as.numeric(Year), y = MeanValue, fill = MeanValue)) +
  geom_smooth(method = "lm", se = T, linetype = "dashed", color = "gray10") +
  geom_line(color = "gray20") +
  geom_point(shape = 21, size = 5, alpha = 0.8) + 
  scale_fill_gradientn(colors = matlab.like(100), name = "") + 
  facet_wrap(~ Region, scales = "free_y", ncol = 3) +
  labs(x = "", y = "SST (°C)") +
  geom_text(data = slope_df, aes(x = -Inf, y = Inf, 
                                 label = paste0(round(slope, 3), " °C year⁻¹")), 
            hjust = -0.1, vjust = 1.2, size = 3, color = "black", inherit.aes = FALSE) + 
  theme(legend.position = "none")

ggsave(last_plot(), file = "outputs/crw_sst_time.png", width = 10, height = 7, bg = "transparent")
