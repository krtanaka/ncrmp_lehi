library(colorRamps)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(patchwork)
library(cowplot)

rm(list = ls())

percentile = 0.96667 #based on 30 years baseline (1955-1984)

########################################################
### load area fraction time series results 1900-2019 ###
########################################################

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

lehi_time = NULL

for (y in 1:length(region_list)) {
  
  # y = 1
  
  load(paste0("outputs/HadI_timeseries_0.96667_", region_list[y], ".RData"))
  hadi = yy_anom
  hadi$data = "HadI"
  hadi$region = region_list[y]
  
  load(paste0("outputs/COBE_timeseries_0.96667_", region_list[y], ".RData"))
  cobe = yy_anom
  cobe$data = "COBE"
  cobe$region = region_list[y]
  
  df = rbind(hadi, cobe); rm(cobe, hadi)  
  
  lehi_time = rbind(lehi_time, df)
  
}

df = lehi_time
df = tidyr::separate(df, time, c("Year", "Month"), sep = "-"); head(df)
df$Month <- sprintf("%02d", as.numeric(df$Month)); head(df)
df$Day = 01; head(df)
df$Day <- sprintf("%02d", as.numeric(df$Day)); head(df)
df$Time = paste(df$Year, df$Month, df$Day, sep = "-"); head(df)
df$Time = as.Date(df$Time); head(df)

exceeded_times = df %>% 
  group_by(Year, region) %>% 
  summarise(year_sum = mean(year_sum)) %>% 
  mutate(y = ifelse(year_sum > 0.5, 1, 0)) %>% 
  dplyr::select(Year, region, y) %>% 
  subset(y > 0) 

exceeded_times$region = factor(exceeded_times$region, levels = c("American Samoa", 
                                                                 "Guam", 
                                                                 "Hawaii", 
                                                                 "Howland and Baker islands", 
                                                                 "Jarvis Island", 
                                                                 "Johnston Atoll", 
                                                                 "Northern Mariana Islands",
                                                                 "Palmyra Atoll", 
                                                                 "Wake Island",
                                                                 "NCRMP"))

ipcc_col <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
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
  # scale_fill_manual(values = cols, "") +
  geom_tile(show.legend = F) +
  labs(x = "", y = "") +
  theme_light() + 
  geom_hline(yintercept = seq(0.5, 8), color = 'white', size = 2) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

all_year = data.frame(Year = seq(1955, 2019, by = 1))

region_n = c("American Samoa", 
             "Guam", 
             "Hawaii", 
             "Howland and Baker islands", 
             "Jarvis Island", 
             "Johnston Atoll", 
             "Northern Mariana Islands",
             "Palmyra Atoll", 
             "Wake Island",
             "NCRMP")

for (i in 1:length(region_n)) {
  
  # i = 2
  ponr = subset(exceeded_times, region == region_n[[i]])
  ponr = merge(ponr, all_year, all = T)
  print(ponr)
  
}

rm(all_year, region_n, i, ponr)

############
### plot ###
############

ElNino = c(1905, 1906, 
           1911, 1912, 1914, 1915, 
           1940, 1941, 1942, 
           1965, 1966, 
           1972, 1973,
           1982, 1983, 1987, 1988, 
           1991, 1992, 1997, 1998, 
           2015, 2016)

df$ElNino = "N"
df$ElNino = ifelse(df$Year %in% ElNino, "Y", df$ElNino)

(p = df %>% 
    subset(region == "NCRMP") %>% 
    group_by(Time) %>% 
    summarise(year_sum = mean(year_sum)) %>% 
    as.data.frame() %>% 
    ggplot(aes(Time, year_sum, color = year_sum)) +
    # geom_vline(xintercept = df$Time[df$ElNino == "Y"], color = "red", alpha = 0.1) +
    geom_line(alpha = 0.2) +
    geom_point(alpha = 0.8, size = 5) +
    # scale_color_gradientn(colours = matlab.like(100), "") + 
    scale_color_gradientn(colors = rev(ipcc_col), "") +
    geom_hline(yintercept = 0.5, linetype = "dashed") + 
    theme_cowplot(I(15)) + 
    theme(legend.position = c(0,1),
          legend.justification = c(-0.3,0.8)) + 
    labs(x = "", y = "Area fraction"))

png("outputs/LEHI_Timeseries_v1.png", height = 8, width = 12, units = "in", res = 500)
print(p)
dev.off()

df = df %>% group_by(Year, region) %>% summarise(year_sum = mean(year_sum))
df$Year = as.numeric(df$Year)

df$ElNino = "N"
df$ElNino = ifelse(df$Year %in% ElNino, "Y", df$ElNino)

# calculate % changes in area fraction above 50% 1900-1959 vs. 1960-2019
t = df %>% mutate(period = ifelse(Year %in% c(1955:1987), "1st", "2nd")) %>% 
  group_by(region, period) %>%
  summarise(sum = mean(year_sum)) %>% 
  mutate(percent = (sum/lag(sum)-1)*100)
t

df$region = factor(df$region, levels = c("NCRMP",
                                         "American Samoa", 
                                         "Guam", 
                                         "Hawaii", 
                                         "Howland and Baker islands", 
                                         "Jarvis Island", 
                                         "Johnston Atoll", 
                                         "Northern Mariana Islands",
                                         "Palmyra Atoll", 
                                         "Wake Island"))

(p = df %>% 
    subset(region != "NCRMP") %>%
    # subset(region == "NCRMP") %>%
    ggplot(aes(Year, year_sum, colour = year_sum)) +
    geom_line(color = "gray80") +
    geom_point(alpha = 0.8, size = 5)  +
    scale_color_gradientn(colors = rev(ipcc_col), "") +
    geom_hline(yintercept = 0.5, linetype = "dashed") + 
    theme_cowplot() + 
    facet_wrap(~region, nrow = 2) + 
    scale_x_continuous(breaks = seq(1950, 2020, 10), limits = c(1955, 2019)) + 
    scale_y_continuous(breaks = c(seq(0, 1, by = 0.2)), limits = c(0, max(df$year_sum))) +
    labs(x = "", y = "Area fraction") + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

png("outputs/LEHI_Timeseries_v1.png", height = 8, width = 12, units = "in", res = 500)
print(p)
dev.off()
