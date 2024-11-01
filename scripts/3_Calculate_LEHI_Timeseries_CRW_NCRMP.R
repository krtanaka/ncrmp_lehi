rm(list = ls())

library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sp)
# library(maptools)
library(sf)
library(sf)
library(dplyr)
library(maps)
library(doParallel)

cores = detectCores()/2
registerDoParallel(cores = cores)

load("outputs/CRW_Region_Names.RData")

sort(unique(crw_region_names$Region))

region_list = c("MARIAN", 
                "MHI", 
                "NWHI", 
                "PRIA", 
                "SAMOA",
                "NCRMP")

calculate_anomalies = function(region){
  
  percentile = 0.96667 #based on 30 years baseline (1955-1984)
  
  # region = "MHI"
  
  load("outputs/CRW_SST_5km_coast.RData"); monthly_CRW = df
  
  # set baseline Jan 1985 - Dec 2014
  Baseline <- monthly_CRW[,1:362]
  names(Baseline)
  # plot(Baseline[,1:2], pch = ".")
  
  if (region != "NCRMP") {
    
    Baseline = left_join(crw_region_names, Baseline)
    Baseline = Baseline %>% filter(Region == region)
    Baseline = Baseline[ , !(names(Baseline) %in% "Region")]
    # plot(Baseline[,1:2], pch = ".")
    
  }
  
  Target <- monthly_CRW[,1:470] #Jan 1985 - Dec 2023
  # plot(Target[,1:2], pch = ".")
  
  if (region != "NCRMP") {
    
    Target = left_join(crw_region_names, Target)
    Target = Target %>% filter(Region == region)
    Target = Target[ , !(names(Target) %in% "Region")]
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
  save(yy_anom, file = paste0("outputs/CRW_timeseries_", percentile, "_", region, "_5km.RData"))
  # save(yy_anom, file = paste0("/mnt/ldrive/ktanaka/CRW_SST/CRW_timeseries_", percentile, "_", region, ".RData"))
  
}

calculate_anomalies("MARIAN")
calculate_anomalies("MHI")
calculate_anomalies("NWHI")
calculate_anomalies("PRIA")
calculate_anomalies("SAMOA")
calculate_anomalies("NCRMP")

stopImplicitCluster()
