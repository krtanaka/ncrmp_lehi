library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(ggplot2)
library(colorRamps)

rm(list = ls())

fit_lm_and_get_slope <- function(column) {
  model <- lm(column ~ seq_along(column))
  coef(model)[2]  # Extract the slope coefficient (the second coefficient)
}

# download "sst.mnmean.nc" from https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2/
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/sst.mnmean.nc"), varname = "sst")
df = raster::rotate(df) #rotate to -180:180
df = df[[86:493]] #1989-2022
assign("df", df, .GlobalEnv)
df = readAll(df)

# set baseline Jan 1989 - Dec 2022
Baseline <- df[[1:408]]

Baseline <- Baseline %>% 
  rasterToPoints() %>%
  data.frame() %>%
  subset(x > -160 & x < -140 & y > 50 & y < 62)

# set target period
Target <- df[[1:408]] #Jan 1989 - Dec 2022

Target <- Target %>% 
  rasterToPoints() %>%
  data.frame() %>%
  subset(x > -160 & x < -140 & y > 50 & y < 62)

ll_anom = NULL

# calculate anomalies at every lot/lon grid cell
for (ll in 1:dim(Baseline)[1]) { 
  
  # ll = 3
  
  print(ll)
  
  monthly_anom = NULL
  
  for (m in 1:12) { # every month
    
    # m = 8
    
    interval = seq(m+2, dim(Baseline)[2], by = 12)
    
    baseline = Baseline[ll, c(interval)]
    baseline = t(baseline)
    baseline = as.data.frame(baseline)
    baseline = baseline[,1]
    baseline = mean(baseline)
    
    interval = seq(m+2, dim(Target)[2], by = 12)
    
    present = Target[ll, c(interval)]
    # present = Target[ll, m+2]
    present = t(present)
    present = as.data.frame(present)
    present = present[,1]
    sum = present - baseline

    monthly_anom = cbind(monthly_anom, sum)
    
  }

  # row.names(monthly_anom) = c(1989:2022)
  # monthly_anom = colMeans(monthly_anom)
  monthly_anom <- apply(monthly_anom, 2, fit_lm_and_get_slope)
  
  # colnames(monthly_anom) = c("jan", "feb", "mar", "apr", "may", "jun",
  #                            "jul", "aug", "sep", "oct", "nov", "dec")
  
  # ll_anom = list(ll_anom, monthly_anom)
  ll_anom = rbind(ll_anom, monthly_anom)
  
}

colnames(ll_anom) = c("jan", "feb", "mar", "apr", "may", "jun",
                      "jul", "aug", "sep", "oct", "nov", "dec")

anom = cbind(Target[1:2], ll_anom)

anom$annual_mean = rowMeans(anom[3:14])

anom %>% 
  ggplot(aes(x, y, fill = annual_mean)) +  
  geom_raster() + 
  scale_fill_gradientn(colors = matlab.like(100), "anomaly_trend") + 
  annotation_map(map = map_data("world")) 
