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
library(cowplot)

rm(list = ls())

percentile = 0.96667 #based on 30 years baseline (1955-1984)

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2019")

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

region = c("MARIAN", "MHI", "NWHI", "PRIAs", "SAMOA")[5]

map = function(mode){
  
  load(paste0("outputs/OISST_0.96667_", region, "_LEHI_1982-1991.RData")); oisst1 = anom; oisst1$source = "oisst"; oisst1$period = "1980-1989"
  load(paste0("outputs/OISST_0.96667_", region, "_LEHI_1992-2001.RData")); oisst2 = anom; oisst2$source = "oisst"; oisst2$period = "1990-1999"
  load(paste0("outputs/OISST_0.96667_", region, "_LEHI_2002-2011.RData")); oisst3 = anom; oisst3$source = "oisst"; oisst3$period = "2000-2009"
  load(paste0("outputs/OISST_0.96667_", region, "_LEHI_2012-2021.RData")); oisst4 = anom; oisst4$source = "oisst"; oisst4$period = "2010-2019"
  
  if (mode == "annual") {
    
    anom = rbind(oisst1, oisst2, oisst3, oisst4)
    
    anom %>% 
      # mutate(sum = range01(sum)) %>% 
      mutate(sum = sum/120) %>% # instead of rescaling 0-1, this needs to be divided by total number of months in 10 years
      group_by(period) %>% 
      summarise(mean = mean(sum))
    
    (p = anom %>% 
        # mutate(sum = range01(sum)) %>% 
        mutate(sum = sum/120) %>% # instead of rescaling 0-1, this needs to be divided by total number of months in 10 years
        group_by(x, y, period) %>% 
        summarise(sum = mean(sum)) %>% 
        ggplot() + 
        # geom_raster(aes(x, y, fill = sum)) +
        geom_tile(aes(x, y, fill = sum), width = 1, height = 1) +
        annotation_map(map_data("world"), fill = "gray50", colour = "gray20") +
        scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(0,1)) +
        facet_grid(~period) +
        # coord_fixed() + 
        # coord_map(projection = "mercator") +
        coord_map("ortho", orientation = c(0, median(anom$x), 0)) +
        theme_map() +
        scale_x_continuous(expand = c(0.1, 0.1), "") +
        scale_y_continuous(expand = c(0.1, 0.1), ""))
    
    png(paste0("outputs/annual_map_v2_", percentile, ".png"), height = 6.5, width = 12, units = "in", res = 500)
    print(p)
    dev.off()
    
  }
  
  if (mode == "seasonal_difference") {
    
    anom = rbind(oisst1, oisst2, oisst3, oisst4)
    
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
        geom_tile(aes(x, y, fill = diff), width = 1, height = 1) + 
        annotation_map(map_data("world"), fill = "gray50", colour = "gray20", size = 0.5) +
        scale_fill_gradientn(colors = rev(ipcc_col), "") +
        scale_x_continuous(expand = c(0.1, 0.1), "") +
        scale_y_continuous(expand = c(0.1, 0.1), "") +
        coord_map(projection = "mercator"))
    
    png(paste0("outputs/annual_map_v3_", percentile, ".png"), height = 6, width = 6, units = "in", res = 500)
    print(p)
    dev.off()
    
  }
  
  if (mode == "combine") {
    
    #all periods
    annual = rbind(oisst1, oisst2, oisst3, oisst4)
    annual$sum = annual$sum/120
    annual$season = "Annual"
    annual = annual[, c("x", "y", "sum", "source", "period", "season")]
    
    #seasonals
    anom = rbind(oisst1, oisst2, oisst3, oisst4)
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
        scale_x_continuous(expand = c(0.1, 0.1), "") +
        scale_y_continuous(expand = c(0.1, 0.1), "") +
        coord_map("ortho", orientation = c(0, median(anom$x), 0)) +
        facet_grid(season ~ period))
    
    png(paste0("outputs/annual_map_v4_", percentile, ".png"), height = 7, width = 10, units = "in", res = 500)
    print(p)
    dev.off()
    
  }
  
}

map("annual")
map("seasonal_difference")
map("combine")
