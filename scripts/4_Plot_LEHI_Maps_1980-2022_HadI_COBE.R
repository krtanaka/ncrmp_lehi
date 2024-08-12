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

percentile = 0.96667 #based on 30 years baseline (1955-1984)

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2019")

data = c("HadI", "COBE")

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
  
  load("outputs/HadI_0.96667_LEHI_1980-1989.RData"); hadi1 = anom; hadi1$source = "HadISST v1.1"; hadi1$period = "1980-1989"
  load("outputs/HadI_0.96667_LEHI_1990-1999.RData"); hadi2 = anom; hadi2$source = "HadISST v1.1"; hadi2$period = "1990-1999"
  load("outputs/HadI_0.96667_LEHI_2000-2009.RData"); hadi3 = anom; hadi3$source = "HadISST v1.1"; hadi3$period = "2000-2009"
  load("outputs/HadI_0.96667_LEHI_2010-2019.RData"); hadi4 = anom; hadi4$source = "HadISST v1.1"; hadi4$period = "2010-2019"
  
  load("outputs/COBE_0.96667_LEHI_1980-1989.RData"); cobe1 = anom; cobe1$source = "COBE v2"; cobe1$period = "1980-1989"
  load("outputs/COBE_0.96667_LEHI_1990-1999.RData"); cobe2 = anom; cobe2$source = "COBE v2"; cobe2$period = "1990-1999"
  load("outputs/COBE_0.96667_LEHI_2000-2009.RData"); cobe3 = anom; cobe3$source = "COBE v2"; cobe3$period = "2000-2009"
  load("outputs/COBE_0.96667_LEHI_2010-2019.RData"); cobe4 = anom; cobe4$source = "COBE v2"; cobe4$period = "2010-2019"
  
  if (mode == "annual") {
    
    anom = rbind(hadi1, hadi2, hadi3, hadi4, 
                 cobe1, cobe2, cobe3, cobe4)
    
    anom$source = factor(anom$source, levels = c("HadISST v1.1", "COBE v2"))
    
    anom %>% 
      # mutate(sum = range01(sum)) %>%
      group_by(period) %>% 
      summarise(mean = mean(sum)/120)
    
    (p = anom %>% 
        # mutate(sum = range01(sum)) %>% 
        group_by(x, y, period) %>% 
        summarise(sum = mean(sum)/120) %>% 
        ggplot() + 
        # geom_raster(aes(x, y, fill = sum)) +
        geom_tile(aes(x, y, fill = sum), width = 1, height = 1) + 
        annotation_map(map_data("world"), fill = "gray50", colour = "gray20") +
        scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(0, 1), breaks = c(0, 0.5, 1)) +
        scale_x_continuous(expand = c(-0.005, 3), "", limits = range(anom$x)) +
        scale_y_continuous(expand = c(-0.005, 3), "", limits = range(anom$y)) +
        facet_wrap(~period, nrow = 1) +
        # coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
        coord_map("ortho", orientation = c(0, median(anom$x), 0)) +
        # coord_map(projection = "mercator") +
        theme(axis.title = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              legend.position = "bottom",
              legend.justification = c(1,0)))
    
    png(paste0("outputs/HadI_COBE_annual_map_v1_", percentile, ".png"), height = 4, width = 10, units = "in", res = 500)
    print(p)
    dev.off()
    
    (p = anom %>% 
        group_by(x, y, period, source) %>% 
        summarise(sum = median(sum)/120) %>% 
        ggplot() +
        geom_tile(aes(x, y, fill = sum), width = 1, height = 1) + 
        annotation_map(map_data("world"), fill = "gray50", colour = "gray20", size = 0.5) +
        scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(0, 1), breaks = c(0, 0.5, 1)) +
        scale_x_continuous(expand = c(-0.005, 15), "", limits = range(anom$x)) +
        scale_y_continuous(expand = c(-0.005, 15), "", limits = range(anom$y)) +
        facet_grid(source ~ period) +
        coord_fixed() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "bottom",
              legend.justification = c(1,0)))
    
    png(paste0("outputs/HadI_COBE_annual_map_v2_", percentile, ".png"), height = 6.5, width = 12, units = "in", res = 500)
    print(p)
    dev.off()
    
  }
  
  if (mode == "seasonal_difference") {
    
    anom = rbind(hadi1, hadi2, hadi3, hadi4, 
                 cobe1, cobe2, cobe3, cobe4)
    
    anom$source = factor(anom$source, levels = c("HadISST v1.1", "COBE v2"))
    
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
        scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(-1, 1), breaks = c(-1, 0, 1)) +
        scale_x_continuous(expand = c(-0.005, 15), "", limits = range(anom$x)) +
        scale_y_continuous(expand = c(-0.005, 15), "", limits = range(anom$y)) +
        coord_map("ortho", orientation = c(0, 180, 0)) + #normal
        theme(#axis.title = element_blank(),
          #axis.text = element_blank(),
          #axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.justification = c(1,0)))
    
    png(paste0("outputs/HadI_COBE_annual_map_v3_", percentile, ".png"), height = 6, width = 6, units = "in", res = 500)
    print(p)
    dev.off()
    
  }
  
  if (mode == "combine") {
    
    #all periods
    annual = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)
    annual$sum = annual$sum/120
    annual$season = "Annual"
    annual = annual[, c("x", "y", "sum", "source", "period", "season")]
    
    #seasonals
    anom = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)
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
        scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(0,1), breaks = c(0, 0.5, 1)) +
        scale_x_continuous(expand = c(-0.005, 15), "", limits = range(anom$x)) +
        scale_y_continuous(expand = c(-0.005, 15), "", limits = range(anom$y)) +
        # coord_fixed() +
        coord_map("ortho", orientation = c(0, 180, 0)) + #normal
        facet_grid(season ~ period) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "bottom",
              legend.justification = c(1,0)))
    
    png(paste0("outputs/HadI_COBE_annual_map_v4_", percentile, ".png"), height = 7, width = 10, units = "in", res = 500)
    print(p)
    dev.off()
    
  }

}

map("annual")
map("seasonal_difference")
map("combine")
