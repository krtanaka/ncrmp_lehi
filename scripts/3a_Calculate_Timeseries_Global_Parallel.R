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

percentile = 0.96667 #based on 30 years baseline (1955-1984)

calculate_anomalies = function(data){

  # data = c("HadI", "COBE")[2]
  
  df = readRDS(paste0('data/', data, "_SST.rds"))

  # set baseline Jan 1955 - Dec 1984, 50 years CMIP6 ENSMN historical climate (1955-1984) https://psl.noaa.gov/ipcc/cmip6/ccwp6.html
  Baseline <- df[[1:360]]
  values(Baseline)[values(Baseline) == -1000] = -1.8
  
  names(Baseline)
  
  Baseline <- Baseline %>% rasterToPoints() %>% data.frame()
  
  Target <- df[[1:780]] #Jan 1955 - Dec 2019
  values(Target)[values(Target) == -1000] = -1.8 
  
  Target <- Target %>% rasterToPoints() %>% data.frame()
  
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
  axis(2, las = 2, at = seq(0, 0.8, 0.1))
  abline(h = 0.5, lty = 2)
  
  save(yy_anom, file = paste0("outputs/", data, "_timeseries_", percentile, ".RData"))
  
}

calculate_anomalies("HadI")
calculate_anomalies("COBE")
