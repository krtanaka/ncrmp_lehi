library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(ggjoy)
library(rworldmap)
library(ggalt)
library(readr)
library(lwgeom)
library(ggdark)

rm(list = ls())

percentile = 0.96667 #based on 30 years baseline (1955-1984)

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

map = function(mode){
  
  load("outputs/CRW_0.96667_LEHI_1985-1994.RData"); crw1 = anom; crw1$source = "CRW v1.1"; crw1$period = "1985-1994"
  load("outputs/CRW_0.96667_LEHI_1995-2004.RData"); crw2 = anom; crw2$source = "CRW v1.1"; crw2$period = "1995-2004"
  load("outputs/CRW_0.96667_LEHI_2005-2014.RData"); crw3 = anom; crw3$source = "CRW v1.1"; crw3$period = "2005-2014"
  load("outputs/CRW_0.96667_LEHI_2015-2023.RData"); crw4 = anom; crw4$source = "CRW v1.1"; crw4$period = "2015-2023"
  
  if (mode == "annual") {
    
    anom = rbind(crw1, crw2, crw3, crw4)
    
    anom %>% 
      filter(period != "2015-2023") %>%
      group_by(period) %>% 
      summarise(mean = mean(sum)/120)
    
    anom %>%
      filter(period == "2015-2023") %>%
      group_by(period) %>%
      summarise(mean = mean(sum)/70)
    
    anom_i_a = anom %>% 
      filter(period != "2015-2023") %>% 
      mutate(x = round(x, 1),
             y = round(y, 1)) %>% 
      group_by(x, y, period) %>% 
      summarise(sum = mean(sum)/120)
    
    anom_i_b = anom %>% 
      filter(period == "2015-2023") %>% 
      mutate(x = round(x, 1),
             y = round(y, 1)) %>% 
      group_by(x, y, period) %>% 
      summarise(sum = mean(sum)/70)
    
    anom_i = rbind(anom_i_a, anom_i_b); rm(anom_i_a, anom_i_b)
    
    (p = anom_i %>% 
        # filter(period != "2015-2021") %>% 
        ggplot() + 
        geom_raster(aes(x, y, fill = sum)) +
        # geom_tile(aes(x, y, fill = sum), width = 1, height = 1) +
        annotation_map(map_data("world"), fill = "gray50", colour = "gray20") +
        scale_fill_gradientn(colors = rev(ipcc_col), "") +
        # scale_x_continuous(expand = c(-0.005, 15), "", limits = range(anom$x)) +
        # scale_y_continuous(expand = c(-0.005, 15), "", limits = range(anom$y)) +
        facet_wrap(~period, nrow = 2) +
        # coord_fixed() + 
        coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
        # coord_map("ortho", orientation = c(0, median(anom$x), 0)) + #normal
        # coord_map(projection = "mercator") +
        theme(axis.title = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              legend.position = "bottom",
              legend.justification = c(1,0)))
    
    png(paste0("outputs/CRW_annual_map_v1_", percentile, ".png"), height = 6.5, width = 6, units = "in", res = 500)
    print(p)
    dev.off()
    
  }
  
  if (mode == "seasonal_difference") {
    
    anom = rbind(crw1, crw2, crw3, crw4)
    
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source", "period")]; season_1$season = "Jan_Feb_Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source", "period")]; season_2$season = "Jul_Aug_Sep"
    
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    
    season_1 = season_1 %>% group_by(x, y) %>% summarise(sum = mean(sum)/30)
    season_2 = season_2 %>% group_by(x, y) %>% summarise(sum = mean(sum)/30)
    
    seasonal_differnece = season_2$sum - season_1$sum #summer - winter
    seasonal_differnece = cbind(season_1[,1:2], seasonal_differnece)
    colnames(seasonal_differnece)[3] = "diff"
    
    (p = seasonal_differnece %>% 
        ggplot() +
        geom_raster(aes(x, y, fill = diff)) + 
        annotation_map(map_data("world"), fill = "gray50", colour = "gray20", size = 0.5) +
        scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c( max(abs(seasonal_differnece$diff))*-1,
                                                                     max(abs(seasonal_differnece$diff)))) +
        # scale_x_continuous(expand = c(-0.005, 15), "", limits = range(anom$x)) +
        # scale_y_continuous(expand = c(-0.005, 15), "", limits = range(anom$y)) +
        # coord_map("ortho", orientation = c(0, 180, 0)) + #normal
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "bottom",
              legend.justification = c(1,0)))
    
    png(paste0("outputs/CWR_annual_map_v3_", percentile, ".png"), height = 6, width = 6, units = "in", res = 500)
    print(p)
    dev.off()
    
  }
  
  if (mode == "combine") {
    
    #all periods
    annual = rbind(crw1, crw2, crw3, crw4)
    annual$sum = annual$sum/120
    annual$season = "Annual"
    annual = annual[, c("x", "y", "sum", "source", "period", "season")]
    
    #seasonals
    anom = rbind(crw1, crw2, crw3, crw4)
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source", "period")]; season_1$season = "Jan-Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source", "period")]; season_2$season = "Jul-Sep"
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    season_1 = season_1[,c(1,2, 6:9)]
    season_2 = season_2[,c(1,2, 6:9)]
    season = rbind(season_1, season_2)
    season$sum = season$sum/30
    season = season[, c("x", "y", "sum", "source", "period", "season")]
    
    anom = rbind(annual, season) %>% group_by(x, y, period, season) %>% summarise(sum = mean(sum))
    
    (p = ggplot(anom) + 
        geom_tile(aes(x, y, fill = sum), width = 1, height = 1) + 
        annotation_map(map_data("world"), fill = "gray50", colour = "gray20", size = 0.5) +
        scale_fill_gradientn(colors = rev(ipcc_col), "") +
        # scale_x_continuous(expand = c(-0.005, 15), "", limits = range(anom$x)) +
        # scale_y_continuous(expand = c(-0.005, 15), "", limits = range(anom$y)) +
        # coord_fixed() +
        # coord_map("ortho", orientation = c(0, 180, 0)) + #normal
        facet_grid(season ~ period) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "bottom",
              legend.justification = c(1,0)))
    
    png(paste0("outputs/CRW_annual_map_v4_", percentile, ".png"), height = 7, width = 10, units = "in", res = 500)
    print(p)
    dev.off()
    
  }
  
}

map("annual")
map("seasonal_difference")
map("combine")
