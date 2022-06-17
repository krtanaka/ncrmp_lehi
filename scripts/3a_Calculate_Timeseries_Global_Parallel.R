rm(list = ls())

library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sp)
library(maptools)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(doParallel)

cores = detectCores()/2
registerDoParallel(cores = cores)
registerDoParallel(cores = 60)

# https://www.marineregions.org/downloads.php
# shp <- readOGR("G:/GIS/nm/World_12NM_v3_20191118_0_360/eez_12nm_v3_0_360.shp") # World 12 Nautical Miles Zone (Territorial Seas) v2 0-360
shp <- readOGR("G:/GIS/eez/World_EEZ_v10_20180221_HR_0_360/World_EEZ_v10_2018_0_360.shp") #World EEZ v10 0-360
shp <- readOGR("/mnt/ldrive/ktanaka/GIS/eez/World_EEZ_v10_20180221_HR_0_360/World_EEZ_v10_2018_0_360.shp") #World EEZ v10 0-360

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

calculate_anomalies = function(data, region){
  
  percentile = 0.96667 #based on 30 years baseline (1955-1984)
  
  # region = "NCRMP"
  
  shp_i <- shp[shp$Territory1 %in% region,]
  
  # data = c("HadI", "COBE")[2]
  
  load(paste0('data/', data, "_SST.RData"))

  # set baseline Jan 1955 - Dec 1984, 50 years CMIP6 ENSMN historical climate (1955-1984) https://psl.noaa.gov/ipcc/cmip6/ccwp6.html
  Baseline <- df[[1:360]]
  values(Baseline)[values(Baseline) == -1000] = -1.8
  names(Baseline)
  Baseline <- Baseline %>% rasterToPoints() %>% data.frame()
  plot(Baseline[,1:2], pch = 20)
  
  if (region != "NCRMP") {
    
    latlon = Baseline[,c(1:2)]
    coordinates(latlon) = ~x+y
    proj4string(latlon) <- CRS.new
    area <- over(latlon, shp_i)
    area = as.data.frame(area[,"Territory1"])
    colnames(area)[1] = "Territory"
    Baseline = cbind(area, Baseline) %>% na.omit()
    Baseline = Baseline[ , !(names(Baseline) %in% "Territory")]
    plot(Baseline[,1:2], pch = 20)
    
  }
  
  Target <- df[[1:780]] #Jan 1955 - Dec 2019
  values(Target)[values(Target) == -1000] = -1.8 
  names(Baseline)
  Target <- Target %>% rasterToPoints() %>% data.frame()
  plot(Target[,1:2], pch = 20)
  
  if (region != "NCRMP") {
    
    latlon = Target[,c(1:2)]; plot(latlon, pch = ".")
    coordinates(latlon) = ~x+y
    proj4string(latlon) <- CRS.new
    area <- over(latlon, shp_i)
    area = as.data.frame(area[,"Territory1"])
    colnames(area)[1] = "Territory"
    Target = cbind(area, Target) %>% na.omit()
    Target = Target[ , !(names(Target) %in% "Territory")]
    plot(Target[,1:2], pch = 20)
    
  }
  
  yy_anom = NULL
  
  for (y in 1:65) { #every year between 1955-2019
    
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
    year_sum$time = paste0(1954+y, "-", seq(1, 12, 1))
    
    yy_anom = rbind(yy_anom, year_sum)
    
    print(1954+y)
    
  }
  
  plot(yy_anom$year_sum, type = "o", axes = F, pch = ".", xlab = "", ylab = "")
  axis(1)
  axis(2, las = 2, at = seq(0, 1, 0.1))
  abline(h = 0.5, lty = 2)
  
  save(yy_anom, file = paste0("outputs/", data, "_timeseries_", percentile, "_", region, ".RData"))
  
}

calculate_anomalies("HadI", "American Samoa")
calculate_anomalies("HadI", "Guam")
calculate_anomalies("HadI", "Hawaii")
calculate_anomalies("HadI", "Howland and Baker islands")
calculate_anomalies("HadI", "Jarvis Island")
calculate_anomalies("HadI", "Johnston Atoll")
calculate_anomalies("HadI", "Northern Mariana Islands")
calculate_anomalies("HadI", "Palmyra Atoll")
calculate_anomalies("HadI", "Wake Island")
calculate_anomalies("HadI", "NCRMP")

calculate_anomalies("COBE", "American Samoa")
calculate_anomalies("COBE", "Guam")
calculate_anomalies("COBE", "Hawaii")
calculate_anomalies("COBE", "Howland and Baker islands")
calculate_anomalies("COBE", "Jarvis Island")
calculate_anomalies("COBE", "Johnston Atoll")
calculate_anomalies("COBE", "Northern Mariana Islands")
calculate_anomalies("COBE", "Palmyra Atoll")
calculate_anomalies("COBE", "Wake Island")
calculate_anomalies("COBE", "NCRMP")
