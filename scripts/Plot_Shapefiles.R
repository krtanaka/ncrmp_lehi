library(readr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(sf)

rm(list = ls())

### plot EEZ ###
eez <- readOGR("G:/GIS/eez/World_EEZ_v10_20180221_HR_0_360/World_EEZ_v10_2018_0_360.shp")
eez <- eez[eez$Pol_type != "Overlapping claim",]
eez <- eez[eez$Sovereign1 == "United States",]
# eez = recenter(eez)
CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(shp) <- CRS.new # proj4string(latlon) <- CRS.new
region_list = c("American Samoa", 
                "Guam", 
                "Hawaii", 
                "Howland and Baker islands", 
                "Jarvis Island", 
                "Johnston Atoll", 
                "Northern Mariana Islands",
                "Palmyra Atoll", 
                "Wake Island",
                "NCRMP")
eez <- eez[eez$Territory1 %in% region_list,]
# eez <- rmapshaper::ms_simplify(eez, keep = 0.01, keep_shapes = F)
eez <- eez %>% st_as_sf() 

png("/Users/Kisei/Desktop/EEZ.png", units = "in", res = 100, height = 20, width = 30)

eez %>%
  # subset(Territory1 %in% c("Maldives", "Tanzania", "Liberia")) %>%
  # subset(Territory1 %in% c("Kiribati", "Ecuador", "Peru")) %>%
  ggplot() + 
  geom_sf(aes(group = Territory1, fill = Territory1), color = "NA", show.legend = T) + 
  scale_fill_discrete("") + 
  theme_void() + 
  # geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id), color = "gray60", fill = "gray40", size = 0.001) + 
  guides(fill = guide_legend(nrow = 20), "") + 
  theme(legend.position = "bottom")

dev.off()
