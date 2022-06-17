library(readr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(sf)

rm(list = ls())

world <- rnaturalearth::ne_countries(scale = 'large', returnclass = "sp")
box_cut <- bbox2SP(n = 90, s = -90, w = -20, e = 20, proj4string = world@proj4string)
world_crop <- gDifference(world, box_cut)

sf::sf_use_s2(FALSE)

pacific_crop <- world_crop %>% 
  st_as_sf() %>% # change from sp to sf object/class
  st_shift_longitude() %>% 
  st_crop(y = c(xmin = st_bbox(.)[["xmin"]],
                xmax = st_bbox(.)[["xmax"]],
                ymin = -50,
                ymax = 50))

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

png("outputs/Pacific_EEZ.png", units = "in", res = 500, height = 8, width = 10)

eez %>%
  ggplot() + 
  geom_sf(data = pacific_crop, size = 0, fill = "gray", color = "gray20") +
  geom_sf(fill = "lightblue", color = "lightblue", alpha = 0.3) + 
  geom_sf_label(aes(label = Territory1)) + 
  scale_x_continuous(expand = c(0, 0), "", limits = c(140, 210)) +
  scale_y_continuous(expand = c(0, 0), "", limits = c(-18, 32)) +
  guides(fill = guide_legend(nrow = 5), "") + 
  # theme_map() +
  theme_minimal() + 
  theme(legend.position = c(1, 0),
        legend.justification = c(1, -0.1),
        panel.grid.major = element_line(colour = "gray80", size = 0.2))

dev.off()
