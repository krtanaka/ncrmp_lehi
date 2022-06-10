library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)

rm(list = ls())

load("data/BenthicREA_sitedata_TAXONCODE.RData")

df$LONGITUDE = ifelse(df$LONGITUDE < 0, df$LONGITUDE + 360, df$LONGITUDE)

region_box = df %>% 
  as.data.frame() %>% 
  group_by(REGION) %>% 
  summarise(min_lon = min(LONGITUDE, na.rm = T),
            max_lon = max(LONGITUDE),
            min_lat = min(LATITUDE),
            max_lat = max(LATITUDE)) %>% 
  as.data.frame()

region_box

percentile = 0.96667 #based on 30 years baseline (1955-1984)

calculate_anomalies = function(period, region){
  
  # period = "2012-2021"
  # region = "MHI"
  
  region_box_i = region_box %>% subset(REGION == region)
  
  load('data/OISST_1982-1991.RData'); df1 = monthly_OISST; names(df1)
  load('data/OISST_1992-2001.RData'); df2 = monthly_OISST; names(df2)
  load('data/OISST_2002-2011.RData'); df3 = monthly_OISST; names(df3)
  load('data/OISST_2012-2021.RData'); df4 = monthly_OISST; names(df4)
  monthly_OISST = cbind(df1, df2[,3:122], df3[,3:122], df4[,3:122]); names(monthly_OISST)
  rm(df1, df2, df3, df4)
  
  df = monthly_OISST %>% subset(x >= region_box_i$min_lon & 
                                  x <= region_box_i$max_lon & 
                                  y >= region_box_i$min_lat & 
                                  y <= region_box_i$max_lat)
  
  # set baseline Jan 1982 - Dec 2011, 30 years
  Baseline <-  df[,1:362]
  names(Baseline) #look at time steps
  
  # set target period
  if (period == "1982-1991") Target <- df[,1:122] #Jan 1982 - Dec 1991
  if (period == "1992-2001") Target <- df[,c(1:2, 123:242)] #Jan 1992 - Dec 2001
  if (period == "2002-2011") Target <- df[,c(1:2, 243:362)] #Jan 2002 - Dec 2011
  if (period == "2012-2021") Target <- df[,c(1:2, 363:482)] #Jan 2012 - Dec 2021
  
  ll_anom = NULL
  
  # calculate lehi at every lot/lon grid cell
  for (ll in 1:dim(Baseline)[1]) { 
    
    # ll = 5
    
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
  
  save(anom, file = paste0("outputs/OISST_", percentile, "_", region, "_LEHI_", period, ".RData"))
  
  # beepr::beep(2)
  
}

calculate_anomalies("1982-1991", "MHI")
calculate_anomalies("1992-2001", "MHI")
calculate_anomalies("2002-2011", "MHI")
calculate_anomalies("2012-2021", "MHI")

calculate_anomalies("1982-1991", "NWHI")
calculate_anomalies("1992-2001", "NWHI")
calculate_anomalies("2002-2011", "NWHI")
calculate_anomalies("2012-2021", "NWHI")

calculate_anomalies("1982-1991", "PRIAs")
calculate_anomalies("1992-2001", "PRIAs")
calculate_anomalies("2002-2011", "PRIAs")
calculate_anomalies("2012-2021", "PRIAs")

calculate_anomalies("1982-1991", "SAMOA")
calculate_anomalies("1992-2001", "SAMOA")
calculate_anomalies("2002-2011", "SAMOA")
calculate_anomalies("2012-2021", "SAMOA")

calculate_anomalies("1982-1991", "MARIAN")
calculate_anomalies("1992-2001", "MARIAN")
calculate_anomalies("2002-2011", "MARIAN")
calculate_anomalies("2012-2021", "MARIAN")
