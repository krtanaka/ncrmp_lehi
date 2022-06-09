library(colorRamps)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(patchwork)

rm(list = ls())

percentile = 0.96667 #based on 30 years baseline (1955-1984)

########################################################
### load area fraction time series results 1900-2019 ###
########################################################

load("outputs/HadI_timeseries_0.96667.RData"); hadi = yy_anom; hadi$region = "NCRMP"
load("outputs/COBE_timeseries_0.96667.RData"); cobe = yy_anom; cobe$region = "NCRMP"

hadi$data = "HadISST"
cobe$data = "COBESST"

df = rbind(hadi, cobe); rm(cobe, hadi)

df = tidyr::separate(df, time, c("Year", "Month"), sep = "-")
df$Month <- sprintf("%02d", as.numeric(df$Month))
df$Day = 01
df$Day <- sprintf("%02d", as.numeric(df$Day))
df$Time = paste(df$Year, df$Month, df$Day, sep = "-")
df$Time = as.Date(df$Time)

exceeded_times = df %>% 
  group_by(Year, region) %>% 
  summarise(year_sum = mean(year_sum)) %>% 
  mutate(y = ifelse(year_sum > 0.5, 1, 0)) %>% 
  dplyr::select(Year, region, y) %>% 
  subset(y > 0) 

exceeded_times$region = factor(exceeded_times$region, levels = c("NCRMP"))

cols <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
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

exceeded_times  %>% 
  ggplot(aes(Year, y = region, fill = region)) +
  scale_fill_manual(values = cols, "") +
  geom_tile(show.legend = F) +
  labs(x = "", y = "") +
  theme_minimal(I(15)) + 
  geom_hline(yintercept = seq(0.5, 8), color = 'white', size = 2) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

all_year = data.frame(Year = seq(1955, 2019, by = 1))

region_n = c(
  "NCRMP")

for (i in 1:length(region_n)) {
  
  # i = 2
  ponr = subset(exceeded_times, region == region_n[[i]])
  ponr = merge(ponr, all_year, all = T)
  print(ponr)
  
}

rm(all_year, region_n, cols, i, ponr)

############
### plot ###
############

timeseries = df %>% group_by(Year, region) %>% summarise(year_sum = mean(year_sum))
timeseries$Year = as.numeric(timeseries$Year)

ElNino = subset(df, Year %in% c(1905, 1906, 
                                1911, 1912, 1914, 1915, 
                                1940, 1941, 1942, 
                                1965, 1966, 
                                1972, 1973,
                                1982, 1983, 1987, 1988, 
                                1991, 1992, 1997, 1998, 
                                2015, 2016))

# calculate % changes in area fraction above 50% 1900-1959 vs. 1960-2019
t = timeseries %>% mutate(period = ifelse(Year %in% c(1955:1987), "1st", "2nd")) %>% 
  group_by(region, period) %>%
  summarise(sum = mean(year_sum)) %>% 
  mutate(percent = (sum/lag(sum)-1)*100)
t


timeseries %>% 
  ggplot(aes(Year, year_sum, color = year_sum)) +
  geom_point(alpha = 0.85, size = 5)  +
  geom_line() +
  # geom_smooth(method = "lm") + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  labs(x = "", y = "Area fraction (%)")

