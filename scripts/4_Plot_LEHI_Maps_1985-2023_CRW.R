library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
# library(rgdal)
library(dplyr)
library(maps)
library(ggjoy)
library(rworldmap)
library(ggalt)
library(readr)
library(lwgeom)
library(ggdark)

rm(list = ls())

select = dplyr::select

percentile = 0.96667 #based on 30 years baseline (1984-2014)

period = c("1985-1994", "1995_2004", "2005-2014", "2015-2023")

data = c("CRW")

world <- fortify(getMap())

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

ipcc_col <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
              rgb(178, 24, 43, maxColorValue = 255, alpha = 255),
              rgb(214, 96, 77, maxColorValue = 255, alpha = 255),
              rgb(244, 165, 130, maxColorValue = 255, alpha = 255),
              rgb(253, 219, 199, maxColorValue = 255, alpha = 255),
              rgb(247, 247, 247, maxColorValue = 255, alpha = 255),
              rgb(209, 229, 240, maxColorValue = 255, alpha = 255),
              rgb(146, 197, 222, maxColorValue = 255, alpha = 255),
              rgb(67, 147, 195, maxColorValue = 255, alpha = 255),
              rgb(33, 102, 172, maxColorValue = 255, alpha = 255),
              rgb(5, 48, 97, maxColorValue = 255, alpha = 255))

# coarse shape files, see prep_shapefile.R
load(paste0('data/isl_sf_dataframe_0.001.RData'))

# use ggmap
ggmap::register_google("AIzaSyDpirvA5gB7bmbEbwB1Pk__6jiV4SXAEcY")

map = function(mode){
  
  load("outputs/CRW_0.96667_LEHI_1985-1994.RData"); crw1 = anom; crw1$source = "CRW v1.1"; crw1$period = "1985-1994"
  load("outputs/CRW_0.96667_LEHI_1995-2004.RData"); crw2 = anom; crw2$source = "CRW v1.1"; crw2$period = "1995-2004"
  load("outputs/CRW_0.96667_LEHI_2005-2014.RData"); crw3 = anom; crw3$source = "CRW v1.1"; crw3$period = "2005-2014"
  load("outputs/CRW_0.96667_LEHI_2015-2023.RData"); crw4 = anom; crw4$source = "CRW v1.1"; crw4$period = "2015-2023"
  
  if (mode == "annual") {
    
    anom = rbind(crw1, crw2, crw3, crw4)
    
    anom$lon = anom$x
    anom$lat = anom$y
    anom <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    anom <- st_intersection(anom, st_make_valid(isl))
    anom <- anom %>% as.data.frame()
    
    anom = anom %>% 
      mutate(x = lon,
             y = lat,
             island = ISLAND_CD, 
             region = Region) %>% 
      select(x, y, 
             jan, feb, mar, apr, may, jun, 
             jul, aug, sep, oct, nov, dec, 
             sum, source, period, island, region)
    
    anom %>% 
      filter(period != "2015-2023") %>%
      group_by(period) %>% 
      summarise(mean = mean(sum)/120)
    
    anom %>%
      filter(period == "2015-2023") %>%
      group_by(period) %>%
      summarise(mean = mean(sum)/108)
    
    anom_i_a = anom %>% 
      filter(period != "2015-2023") %>% 
      mutate(x = round(x, 1),
             y = round(y, 1)) %>%
      group_by(x, y, period, island, region) %>% 
      summarise(sum = mean(sum)/120)
    
    anom_i_b = anom %>% 
      filter(period == "2015-2023") %>% 
      mutate(x = round(x, 1),
             y = round(y, 1)) %>%
      group_by(x, y, period, island, region) %>% 
      summarise(sum = mean(sum)/108)
    
    anom_i = rbind(anom_i_a, anom_i_b)
    
    # (p = anom_i %>% 
    #     filter(region == "MHI") %>%
    #     # filter(period %in% c("1985-1994")) %>%
    #     filter(period %in% c("2015-2023")) %>%
    #     # filter(period %in% c("1985-1994", "2015-2023")) %>%
    #     # filter(period %in% c("2005-2014", "2015-2023")) %>%
    #     ggplot() + 
    #     geom_raster(aes(x, y, fill = sum)) +
    #     # geom_tile(aes(x, y, fill = sum), width = 1, height = 1) +
    #     annotation_map(map_data("world"), fill = "gray50", colour = "gray20") +
    #     scale_fill_gradientn(colors = rev(ipcc_col), "") +
    #     # scale_x_continuous(expand = c(-0.005, 15), "", limits = range(anom$x)) +
    #     # scale_y_continuous(expand = c(-0.005, 15), "", limits = range(anom$y)) +
    #     # facet_wrap(~period, nrow = 2) +
    #     # facet_wrap(region~period, scales = "free") +
    #     # facet_grid(~period, scales = "free") +
    #     coord_fixed() +
    #     # coord_sf(xlim = range(anom_i$x), ylim = range(anom_i$y)) +
    #     # coord_map("ortho", orientation = c(0, median(anom$x), 0)) + #normal
    #     # coord_map(projection = "mercator") +
    #     theme(axis.title = element_blank(),
    #           axis.ticks = element_blank(), 
    #           axis.text = element_blank(),
    #           legend.position = "bottom",
    #           legend.justification = c(1,0)))
    
    anom_i <- anom_i %>%
      mutate(x = ifelse(x > 180, x - 360, x)) %>% 
      st_as_sf(coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    # (p = anom_i %>% 
    #     filter(region == "MHI") %>%
    #     filter(period %in% c("2015-2023")) %>% 
    #     ggplot() +
    #     geom_sf(aes(fill = sum), size = 5, shape = 22) + 
    #     scale_fill_gradientn(colors = rev(ipcc_col), ""))
    
    coordinates <- st_coordinates(anom_i %>% filter(region == "MHI") )
    coords_df <- as.data.frame(coordinates)
    mean_lat <- mean(coords_df$Y)
    mean_lon <- mean(coords_df$X)
    min_lat <- min(coords_df$Y)
    max_lat <- max(coords_df$Y)
    min_lon <- min(coords_df$X)
    max_lon <- max(coords_df$X)
    
    map = ggmap::get_map(#location = c(mean_lon, mean_lat),
      location =  c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat),
      maptype = "satellite",
      zoom = 7,
      force = T, 
      color = "bw")
    
    (ggmap(map, darken = c(0.5, "black")) + 
        geom_sf(data = anom_i %>% 
                  filter(region == "MHI") %>%
                  filter(period %in% c("2015-2023")),
                aes(fill = sum), size = 4, shape = 22, inherit.aes = F) + 
        scale_fill_gradientn(colors = rev(ipcc_col), name = "LEHI") +
        scale_y_continuous(limits = c(min_lat, max_lat), "") + 
        scale_x_continuous(limits = c(min_lon, max_lon), "") + 
        theme(legend.position = c(0.08, 0.2),
              legend.background = element_blank(),
              legend.box.background = element_blank(), 
              legend.text = element_text(color = "white", face = "bold", size = 14), 
              legend.title = element_text(color = "white", face = "bold", size = 16)) + 
        annotate("text", x = max_lon, y = max_lat, label = "2015-2023", 
                 hjust = 1, vjust = 1, color = "white", size = 6))
    
    ggsave(last_plot(), file = paste0("outputs/CRW_LEHI_map_annual_", percentile, ".png"), height = 5, width = 8)
    
  }
  
  if (mode == "seasonal_difference") {
    
    anom = rbind(crw1, crw2, crw3, crw4)
    
    anom$lon = anom$x
    anom$lat = anom$y
    anom <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    anom <- st_intersection(anom, st_make_valid(isl))
    anom = anom %>% as.data.frame()
    
    anom = anom %>% 
      filter(period %in% c("2015-2023")) %>% 
      mutate(x = lon,
             y = lat,
             island = ISLAND_CD, 
             region = Region) %>% 
      select(x, y, 
             jan, feb, mar, apr, may, jun, 
             jul, aug, sep, oct, nov, dec, 
             sum, source, period, island, region)
    
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source", "period", "island", "region")]; season_1$season = "Jan_Feb_Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source", "period", "island", "region")]; season_2$season = "Jul_Aug_Sep"
    
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    
    season_1 = season_1 %>% group_by(x, y, island, region) %>% summarise(sum = mean(sum)/30)
    season_2 = season_2 %>% group_by(x, y, island, region) %>% summarise(sum = mean(sum)/30)
    
    seasonal_differnece = season_2$sum - season_1$sum #summer - winter
    seasonal_differnece = cbind(season_1[,1:4], seasonal_differnece)
    colnames(seasonal_differnece)[5] = "diff"
    
    seasonal_differnece = seasonal_differnece %>% 
      mutate(x = round(x, 1),
             y = round(y, 1)) %>% 
      group_by(x, y, island, region) %>% 
      summarise(diff = mean(diff))
    
    # (seasonal_differnece %>% 
    #     filter(region == "MHI") %>%
    #     ggplot() +
    #     geom_raster(aes(x, y, fill = diff)) +
    #     # geom_tile(aes(x, y, fill = diff), width = 0.5, height = 0.5) + 
    #     annotation_map(map_data("world"), fill = "gray50", colour = "gray20", size = 0.5) +
    #     scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c( max(abs(seasonal_differnece$diff))*-1,
    #                                                                  max(abs(seasonal_differnece$diff)))) +
    #     scale_fill_gradientn(colors = rev(ipcc_col), "") +
    #     # scale_x_continuous(expand = c(-0.005, 15), "", limits = range(anom$x)) +
    #     # scale_y_continuous(expand = c(-0.005, 15), "", limits = range(anom$y)) +
    #     # coord_map("ortho", orientation = c(0, 180, 0)) + #normal
    #     theme(axis.title = element_blank(),
    #           axis.text = element_blank(),
    #           axis.ticks = element_blank(),
    #           legend.text = element_text(size = 6),
    #           legend.position = "bottom",
    #           legend.justification = c(1, 0)))
    
    seasonal_differnece <- seasonal_differnece %>%
      mutate(x = ifelse(x > 180, x - 360, x)) %>% 
      st_as_sf(coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    coordinates <- st_coordinates(seasonal_differnece %>% filter(region == "MHI") )
    coords_df <- as.data.frame(coordinates)
    mean_lat <- mean(coords_df$Y)
    mean_lon <- mean(coords_df$X)
    min_lat <- min(coords_df$Y)
    max_lat <- max(coords_df$Y)
    min_lon <- min(coords_df$X)
    max_lon <- max(coords_df$X)
    
    map = ggmap::get_map(#location = c(mean_lon, mean_lat),
      location =  c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat),
      maptype = "satellite",
      zoom = 7,
      force = T,
      color = "bw")
    
    (ggmap(map, darken = c(0.5, "black")) + 
        geom_sf(data = seasonal_differnece %>% 
                  filter(region == "MHI"),
                aes(fill = diff), size = 4, shape = 22, inherit.aes = F) + 
        scale_fill_gradientn(colors = rev(ipcc_col), name = "LEHI\nseasonal difference\n(Jul-Sep)-(Jan-Mar)") +
        scale_y_continuous(limits = c(min_lat, max_lat), "") + 
        scale_x_continuous(limits = c(min_lon, max_lon), "") + 
        theme(legend.position = c(0.18, 0.25),
              legend.background = element_blank(),
              legend.box.background = element_blank(), 
              legend.text = element_text(color = "white", face = "bold", size = 14), 
              legend.title = element_text(color = "white", face = "bold", size = 16)) + 
        annotate("text", x = max_lon, y = max_lat, label = "2015-2023", 
                 hjust = 1, vjust = 1, color = "white", size = 6))
    
    ggsave(last_plot(), file = paste0("outputs/CRW_LEHI_map_seasonal_diff_", percentile, ".png"), height = 5, width = 8)
    
  }
  
  if (mode == "combine") {
    
    #all periods
    annual = rbind(crw1, crw2, crw3, crw4)
    
    annual$lon = annual$x
    annual$lat = annual$y
    annual <- st_as_sf(x = annual, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    annual <- st_intersection(annual, st_make_valid(isl))
    annual <- annual %>% as.data.frame()
    colnames(annual) = tolower(colnames(annual))
    annual$x = annual$lon
    annual$y = annual$lat
    
    annual$sum = annual$sum/120
    annual$season = "Annual"
    annual = annual[, c("x", "y", "sum", "source", "period", "season", "region")]
    
    #seasonals
    anom = rbind(crw1, crw2, crw3, crw4)
    
    anom$lon = anom$x
    anom$lat = anom$y
    anom <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    anom <- st_intersection(anom, st_make_valid(isl))
    anom = anom %>% as.data.frame()
    
    anom = anom %>% 
      mutate(x = lon,
             y = lat,
             island = ISLAND_CD, 
             region = Region) %>% 
      select(x, y, 
             jan, feb, mar, apr, may, jun, 
             jul, aug, sep, oct, nov, dec, 
             sum, source, period, island, region)
    
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source", "period", "region")]; season_1$season = "Jan-Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source", "period", "region")]; season_2$season = "Jul-Sep"
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    season_1 = season_1[,c(1,2, 6:10)]
    season_2 = season_2[,c(1,2, 6:10)]
    season = rbind(season_1, season_2)
    season$sum = season$sum/30
    season = season[, c("x", "y", "sum", "source", "period", "season", "region")]
    
    anom = rbind(annual, season) %>% 
      mutate(x = round(x, 1),
             y = round(y, 1)) %>% 
      group_by(x, y, period, season, region) %>% 
      summarise(sum = mean(sum))
    
    anom <- anom %>%
      mutate(x = ifelse(x > 180, x - 360, x)) %>% 
      st_as_sf(coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    (ggmap(map, darken = c(0.5, "black")) + 
        geom_sf(data = anom %>% 
                  filter(region == "MHI"),
                aes(fill = sum), shape = 22, inherit.aes = F) + 
        scale_fill_gradientn(colors = rev(ipcc_col), name = "LEHI") +
        scale_y_continuous(limits = c(min_lat, max_lat), "") +
        scale_x_continuous(limits = c(min_lon, max_lon), "") +
        facet_grid(season ~ period))
    
    # (anom %>%
    #     filter(region == "MHI") %>%
    #     # filter(region == "MARIAN") %>%
    #     # filter(period %in% c("2015-2023")) %>%
    #     # filter(season %in% c("Jan-Mar", "Jul-Sep")) %>% 
    #     ggplot() +
    #     geom_raster(aes(x, y, fill = sum), color = "gray20") +
    #     scale_fill_gradientn(colors = rev(ipcc_col), "") +
    #     coord_fixed() +
    #     facet_grid(season ~ period) +
    #     theme(
    #       axis.title = element_blank(),
    #       axis.text = element_blank(),
    #       axis.ticks = element_blank(),
    #       legend.justification = c(1, 0),
    #       panel.background = element_rect(fill = "transparent", colour = NA_character_), # Necessary to avoid drawing panel outline
    #       panel.grid.major = element_blank(), # Get rid of major grid
    #       panel.grid.minor = element_blank(), # Get rid of minor grid
    #       plot.background = element_rect(fill = "transparent", colour = NA_character_), # Necessary to avoid drawing plot outline
    #       legend.background = element_rect(fill = "transparent"),
    #       legend.box.background = element_rect(fill = "transparent"),
    #       legend.key = element_rect(fill = "transparent"),
    #       legend.text = element_text(color = "white", face = "bold"), # Set legend text to white and bold
    #       legend.title = element_text(color = "white", face = "bold")  # Set legend title to white and bold
    #     ))
    
    ggsave(last_plot(), filename = "outputs/CRW_LEHI_map_combined.png",  bg = "transparent", width = 12, height = 6)
    
  }
  
}

map("annual")
map("seasonal_difference")
map("combine")
