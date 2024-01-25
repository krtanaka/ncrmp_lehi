rm(list = ls())

library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sp)
library(maptools)
library(sf)
library(sf)
library(dplyr)
library(maps)
library(doParallel)

cores = detectCores()-2
registerDoParallel(cores = cores)

# https://www.marineregions.org/downloads.php
# shp <- readOGR("G:/GIS/nm/World_12NM_v3_20191118_0_360/eez_12nm_v3_0_360.shp") # World 12 Nautical Miles Zone (Territorial Seas) v2 0-360
shp <- st_read(file.path("G:/GIS/eez/World_EEZ_v10_20180221_HR_0_360/World_EEZ_v10_2018_0_360.shp")) %>% as("Spatial") #World EEZ v10 0-360
shp <- st_read(file.path("/mnt/ldrive/ktanaka/GIS/eez/World_EEZ_v10_20180221_HR_0_360/World_EEZ_v10_2018_0_360.shp")) %>% as("Spatial") #World EEZ v10 0-360

shp <- shp[shp$Pol_type != "Overlapping claim",]
shp <- shp[shp$Sovereign1 == "United States",]
# shp = recenter(shp)

CRS.new <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(shp) <- CRS.new # proj4string(latlon) <- CRS.new

sort(unique(shp$Territory1))

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

shp_pacific <- shp[shp$Territory1 %in% region_list,]
plot(shp_pacific, pch = "."); map(add = T); axis(1); axis(2)

calculate_anomalies = function(region){
  
  percentile = 0.96667 #based on 30 years baseline (1955-1984)
  
  # region = "Palmyra Atoll"
  
  shp_i <- shp[shp$Territory1 %in% region,]
  
  load("/mnt/ldrive/ktanaka/CRW_SST/CRW_SST.RData"); monthly_CRW = df
  # load("G:/SST/CRW_SST/CRW_SST.RData"); monthly_CRW = df
  
  # set baseline Jan 1985 - Dec 2014
  Baseline <- monthly_CRW[,1:362]
  names(Baseline)
  # plot(Baseline[,1:2], pch = ".")
  
  if (region != "NCRMP") {
    
    latlon = Baseline[,c(1:2)]
    coordinates(latlon) = ~x+y
    proj4string(latlon) <- CRS.new
    area <- over(latlon, shp_i)
    area = as.data.frame(area[,"Territory1"])
    colnames(area)[1] = "Territory"
    Baseline = cbind(area, Baseline) %>% na.omit()
    Baseline = Baseline[ , !(names(Baseline) %in% "Territory")]
    # plot(Baseline[,1:2], pch = ".")
    
  }
  
  Target <- monthly_CRW[,1:470] #Jan 1985 - Dec 2023
  # plot(Target[,1:2], pch = ".")
  
  if (region != "NCRMP") {
    
    latlon = Target[,c(1:2)]
    coordinates(latlon) = ~x+y
    proj4string(latlon) <- CRS.new
    area <- over(latlon, shp_i)
    area = as.data.frame(area[,"Territory1"])
    colnames(area)[1] = "Territory"
    Target = cbind(area, Target) %>% na.omit()
    Target = Target[ , !(names(Target) %in% "Territory")]
    # plot(Target[,1:2], pch = ".")
    
  }
  
  yy_anom = NULL
  
  for (y in 1:39) { #every year between 1985-2023
    
    # y = 1
    
    interval_year = seq(3, dim(Target)[2], by = 12) 
    
    first_month = interval_year[y]
    last_month = first_month + 11
    
    target = Target[,first_month:last_month]; names(target); dim(target) # target year
    # ll_anom = NULL
    
    # ptime <- system.time({
    
    r <- foreach(ll = 1:dim(target)[1], .combine = rbind) %dopar% {
      
      # ll = 2
      
      monthly_anom = NULL
      
      for (m in 1:12) { # every month
        
        # m = 1
        
        interval_month = seq(m+2, dim(Baseline)[2], by = 12)
        
        baseline = Baseline[ll, c(interval_month)]; names(baseline) #pick corresponding month from every baseline year
        baseline = t(baseline)
        baseline = as.data.frame(baseline)
        baseline = baseline[,1]
        
        q = quantile(baseline, prob = percentile)
        # hist(baseline, breaks = 60, col = matlab.like(60), lty = "blank")
        # abline(v = q)
        
        present = target[ll, m]; present
        sum = ifelse(q < present, 1, 0)
        
        monthly_anom = cbind(monthly_anom, sum)
        
      }
      
      monthly_anom
      
      # ll_anom = rbind(ll_anom, monthly_anom)
      
    }
    
    # })[3]; ptime
    
    r = as.data.frame(r)
    
    colnames(r) = c("jan", "feb", "mar", "apr", "may", "jun",
                    "jul", "aug", "sep", "oct", "nov", "dec")
    
    # year_sum = rowSums(r)
    year_sum = colSums(r) #sum number of grid above monthly baseline 
    year_sum = colSums(r)/dim(Baseline)[1] #divide by total grid cell
    year_sum = as.data.frame(year_sum)
    year_sum$time = paste0(1984+y, "-", seq(1, 12, 1))
    
    yy_anom = rbind(yy_anom, year_sum)
    
    print(1984+y)
    
  }
  
  plot(yy_anom$year_sum, type = "o", axes = F, pch = ".", xlab = "", ylab = "")
  axis(1)
  axis(2, las = 2, at = seq(0, 1, 0.1))
  abline(h = 0.5, lty = 2)
  
  # save(yy_anom, file = paste0("outputs/CRW_timeseries_", percentile, "_", region, ".RData"))
  save(yy_anom, file = paste0("/mnt/ldrive/ktanaka/CRW_SST/CRW_timeseries_", percentile, "_", region, ".RData"))
  
}

calculate_anomalies("American Samoa")
calculate_anomalies("Guam")
calculate_anomalies("Hawaii")
calculate_anomalies("Howland and Baker islands")
calculate_anomalies("Jarvis Island")
calculate_anomalies("Johnston Atoll")
calculate_anomalies("Northern Mariana Islands")
calculate_anomalies("Palmyra Atoll")
calculate_anomalies("Wake Island")
calculate_anomalies("NCRMP")
