library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)

rm(list = ls())

percentile = 0.96667 #based on 30 years baseline (1955-1984)

calculate_anomalies = function(period, data){
  
  # period = "1980-1989"
  # data = "COBE"
  # data = "HadI"
  
  load(paste0('G:/SST/', data, "_SST/", data, "_SST.Rdata"))
  
  # set baseline Jan 1955 - Dec 1984, 50 years CMIP6 ENSMN historical climate (1955-1984) https://psl.noaa.gov/ipcc/cmip6/ccwp6.html
  Baseline <-  df[[1:360]]
  values(Baseline)[values(Baseline) == -1000] = -1.8
  
  names(Baseline)
  
  png(paste0("outputs/", data, "_Climatology_1955-1984.png"), height = 9, width = 5.65, res = 500, units = "in")
  par(mfrow = c(2,1))
  plot(calc(Baseline, mean), col = matlab.like(100), axes = F, main = "Mean")
  map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  plot(calc(Baseline, sd), col = matlab.like(100), axes = F, main = "SD")
  map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  dev.off()
  
  Baseline <- Baseline %>% rasterToPoints() %>% data.frame()
  Baseline$x = ifelse(Baseline$x < 0, Baseline$x + 360, Baseline$x)
  Baseline = Baseline %>% subset(x >140 & x < 207)

  # View(names(df)) #look at time steps
  
  # set target period
  if (period == "1980-1989") Target <- df[[301:420]] #Jan 1980 - Dec 1989
  if (period == "1990-1999") Target <- df[[421:540]] #Jan 1990 - Dec 1999
  if (period == "2000-2009") Target <- df[[541:660]] #Jan 2000 - Dec 2009
  if (period == "2010-2019") Target <- df[[661:780]] #Jan 2010 - Dec 2019
  if (period == "2020-2022") Target <- df[[781:816]] #Jan 2020 - Dec 2022
  
  values(Target)[values(Target) == -1000] = -1.8 
  
  Target <- Target %>% rasterToPoints() %>% data.frame()
  Target$x = ifelse(Target$x < 0, Target$x + 360, Target$x)
  Target = Target %>% subset(x >140 & x < 207)

  ll_anom = NULL
  
  # calculate anomalies at every lot/lon grid cell
  for (ll in 1:dim(Baseline)[1]) { 
    
    # ll = 1000
    
    print(ll)
    
    monthly_anom = NULL
    
    for (m in 1:12) { # every month
      
      # m = 8
      
      interval = seq(m+2, dim(Baseline)[2], by = 12)
      
      baseline = Baseline[ll, c(interval)]
      baseline = t(baseline)
      baseline = as.data.frame(baseline)
      baseline = baseline[,1]
      
      q = quantile(baseline, prob = percentile)
      # hist(baseline, breaks = 100, col = matlab.like(100), lty = "blank")
      # abline(v = q)
      
      interval = seq(m+2, dim(Target)[2], by = 12)
      
      present = Target[ll, c(interval)]
      present = t(present)
      present = as.data.frame(present)
      present = present[,1]
      sum = sum(q < present)
      
      monthly_anom = cbind(monthly_anom, sum)
      
    }
    
    ll_anom = rbind(ll_anom, monthly_anom)
    
  }
  
  colnames(ll_anom) = c("jan", "feb", "mar", "apr", "may", "jun",
                        "jul", "aug", "sep", "oct", "nov", "dec")
  
  anom = cbind(Target[1:2], ll_anom)
  
  anom$sum = rowSums(anom[3:14])
  
  save(anom, file = paste0("outputs/", data, "_", percentile, "_LEHI_", period, ".RData"))

  # beepr::beep(2)
  
}

calculate_anomalies("1980-1989", "HadI")
calculate_anomalies("1990-1999", "HadI")
calculate_anomalies("2000-2009", "HadI")
calculate_anomalies("2010-2019", "HadI")
calculate_anomalies("2020-2022", "HadI")

calculate_anomalies("1980-1989", "COBE")
calculate_anomalies("1990-1999", "COBE")
calculate_anomalies("2000-2009", "COBE")
calculate_anomalies("2010-2019", "COBE")
calculate_anomalies("2020-2022", "COBE")
