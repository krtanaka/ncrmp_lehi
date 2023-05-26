library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
# library(rgdal)
library(dplyr)
library(maps)
library(foreach)
library(doParallel)
library(pbapply)

rm(list = ls())

calculate_anomalies = function(period){
  
  # period = "1985-1994"
  
  percentile = 0.96667 # based on 30 years baseline (1985-2014, one of CMIP6)
  
  # load("G:/CRW_SST/CRW_1985-2022.RData"); names(df)
  load("/mnt/ldrive/ktanaka/CRW_SST/CRW_1985-2022.RData"); names(df)

  # set baseline Jan 1985 - Dec 2014, 30 years
  Baseline <-  df[,1:362]
  names(Baseline) #look at time steps
  
  # set target period
  if (period == "1985-1994") Target <- df[, 1:122] #Jan 1985 - Dec 1994
  if (period == "1995-2004") Target <- df[,c(1:2, 123:242)] #Jan 1995 - Dec 2004
  if (period == "2005-2014") Target <- df[,c(1:2, 243:362)] #Jan 2005 - Dec 2014
  if (period == "2015-2021") Target <- df[,c(1:2, 363:446)] #Jan 2015 - Dec 2021
  
  # Define the parallel backend
  cl <- makeCluster(detectCores()-2)
  
  # Register the parallel backend
  registerDoParallel(cl)
  
  # calculate lehi at every lot/lon grid cell
  ll_anom <- foreach(ll = 1:dim(Baseline)[1], .combine = rbind) %dopar% {
    
    # ll = 5
    
    print(ll)
    
    monthly_anom = NULL
    
    for (m in 1:12) { # every month
      
      # m = 12
      
      interval = seq(m + 2, dim(Baseline)[2], by = 12)
      
      baseline = Baseline[ll, c(interval)]
      baseline = t(baseline)
      baseline = as.data.frame(baseline)
      baseline = baseline[,1]
      
      q = quantile(baseline, prob = percentile)
      # hist(baseline, breaks = 100, col = matlab.like(100), lty = "blank")
      # abline(v = q)
      
      interval = seq(m + 2, dim(Target)[2], by = 12)
      
      present = Target[ll, c(interval)]
      present = t(present)
      present = as.data.frame(present)
      present = present[,1]
      sum = sum(q < present)
      
      monthly_anom = cbind(monthly_anom, sum)
      
    }
    
    monthly_anom
    
  }
  
  # Stop the parallel backend
  stopCluster(cl)
  
  ll_anom = as.data.frame(ll_anom)
  
  colnames(ll_anom) = c("jan", "feb", "mar", "apr", "may", "jun",
                        "jul", "aug", "sep", "oct", "nov", "dec")
  
  anom = cbind(Target[1:2], ll_anom)
  
  anom$sum = rowSums(anom[3:14])
  
  save(anom, file = paste0("outputs/CRW_", percentile, "_LEHI_", period, ".RData"))
  
  # beepr::beep(2)
  
}

calculate_anomalies("1985-1994")
calculate_anomalies("1995-2004")
calculate_anomalies("2005-2014")
calculate_anomalies("2015-2021")