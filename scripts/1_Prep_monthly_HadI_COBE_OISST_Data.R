library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)

rm(list = ls())

load("data/BenthicREA_sitedata_TAXONCODE.RData")

df$LONGITUDE = ifelse(df$LONGITUDE < 0, df$LONGITUDE + 360, df$LONGITUDE)

df = df %>% 
  group_by(LONGITUDE, LATITUDE) %>% 
  summarise(n = n())

summary(df)

plot(df$LONGITUDE, df$LATITUDE, pch = ".", bty = "n", col = "red")
maps::map(add = T, fill = T)

# OSF for Tanaka and Van Houtan 2022
# https://osf.io/mj8u7/

# HadISST1
# https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html

# COBE SST2
# https://psl.noaa.gov/data/gridded/data.cobe.html

e = extent(143, 207, -16, 30) #just pacific NCRMP region

#COBE
df = stack("G:/SST/COBE_SST/sst.mon.mean.nc", varname = "sst")
df = df[[769:1584]] #trim to Jan 1955 - Dec 2022
# df = raster::rotate(df) #rotate to -180:180
df = crop(df, e); plot(mean(df));maps::map(add = T)
assign("df", df, .GlobalEnv)
df = readAll(df)
save(df, file = "G:/SST/COBE_SST/COBE_SST.Rdata")


#Hadley
df = stack("G:/SST/HadI_SST/HadISST_sst.nc", varname = "sst")
df = df[[1021:1836]] #trim to Jan 1955 - Dec 2022
had_names = names(df)
# Cut in two pieces, change the extent of the western hemisphere, and merge again.
x1 <- crop(df, extent(-180, 0, -90, 90))
x2 <- crop(df, extent(0, 180, -90, 90))   
extent(x1) <- c(180, 360, -90, 90)
df <- merge(x1, x2)
df = crop(df, e); plot(mean(df));maps::map(add = T)
assign("df", df, .GlobalEnv)
df = readAll(df)
names(df) = had_names
save(df, file = "G:/SST/HadI_SST/HadI_SST.Rdata")

#OISST
df = stack("G:/SST/OISST/sst.mnmean.nc", varname = "sst")
df = df[[2:493]] #trim to Jan 1982 - Dec 2022
# df = raster::rotate(df) #rotate to -180:180
df = crop(df, e); plot(mean(df));maps::map(add = T)
assign("df", df, .GlobalEnv)
df = readAll(df)
save(df, file = "G:/SST/OISST/OISST.Rdata")
