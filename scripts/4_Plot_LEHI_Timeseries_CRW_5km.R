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

region_list = c("MARIAN", 
                "MHI", 
                "NWHI", 
                "PRIA", 
                "SAMOA",
                "NCRMP")

lehi_time = NULL

for (y in 1:length(region_list)) {
  
  # y = 1
  
  load(paste0("outputs/CRW_timeseries_0.96667_", region_list[y], "_5km.RData"))
  crw = yy_anom
  crw$data = "CRW"
  crw$region = region_list[y]
  
  df = crw
  
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

exceeded_times$region = factor(exceeded_times$region, levels = region_list)

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
  ggplot(aes(as.numeric(Year), y = region, fill = region)) +
  # scale_fill_manual(values = cols, "") +
  geom_tile(show.legend = F, height = 0.5) +
  labs(x = "", y = "") +
  theme_light() +
  geom_hline(yintercept = seq(0.5, 8), color = 'white', size = 2) + 
  # scale_x_continuous(breaks = seq(1984, 2023, 10), limits = c(1984, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

all_year = data.frame(Year = seq(1985, 2023, by = 1))

region_n = region_list

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

# https://psl.noaa.gov/enso/past_events.html

ElNino = c(1897, 1900,
           1903, 1906,
           1915, 1919, 
           1926, 
           1931, 
           1941, 1942, 
           1958, 
           1966, 
           1973, 1978, 1980,
           1983, 1987, 1988,
           1992, 1995, 1998,
           2003, 2007, 2010,
           2016)


df$ElNino = "N"
df$ElNino = ifelse(df$Year %in% ElNino, "Y", df$ElNino)

LaNina = c(1904, 1909, 1910, 1911, 1917, 1918,
           1925,
           1934, 1939, 
           1943, 1950,
           1951, 1955, 1956,
           1962, 
           1971, 1974, 1976, 
           1989, 1999, 2000,
           2008, 2011, 2012, 
           2021, 2022)

df$LaNina = "N"
df$LaNina = ifelse(df$Year %in% LaNina, "Y", df$LaNina)

(p = df %>% 
    subset(region == "NCRMP") %>% 
    group_by(Time) %>% 
    summarise(year_sum = mean(year_sum)) %>% 
    as.data.frame() %>% 
    ggplot(aes(Time, year_sum, color = year_sum)) +
    # geom_vline(xintercept = df$Time[df$ElNino == "Y"], color = "red", alpha = 0.1) +
    # geom_vline(xintercept = df$Time[df$LaNina == "Y"], color = "blue", alpha = 0.1) +
    geom_line(alpha = 0.2) +
    geom_point(alpha = 0.8, size = 5) +
    # scale_y_continuous(limits = c(0, 1)) + 
    scale_color_gradientn(colors = rev(ipcc_col), "") +
    geom_hline(yintercept = 0.5, linetype = "dashed") + 
    theme_cowplot(I(15)) + 
    theme(legend.position = c(0,1),
          legend.justification = c(-0.3,0.8)) + 
    labs(x = "", y = "Area fraction"))

ggsave(last_plot(), file = "outputs/LEHI_Timeseries_v1_5km.png", height = 8, width = 12)

df = df %>% group_by(Year, region) %>% summarise(year_sum = mean(year_sum))
df$Year = as.numeric(df$Year)

df$ElNino = "N"
df$ElNino = ifelse(df$Year %in% ElNino, "Y", df$ElNino)

# calculate % changes in area fraction above 50% 1985:2004 vs. 2005:2023
df %>% 
  mutate(period = ifelse(Year %in% c(1985:2004), "1st", "2nd")) %>% 
  group_by(region, period) %>%
  summarise(sum = mean(year_sum)) %>% 
  mutate(percent = (sum/lag(sum)-1)*100)

# df$region = ifelse(df$region == "NCRMP", "Pacific NCRMP", df$region)

df$region = factor(df$region, levels = region_list)

(p = df %>% 
    # subset(region != "NCRMP") %>%
    ggplot(aes(Year, year_sum, colour = year_sum)) +
    geom_line(color = "gray80") +
    geom_point(alpha = 0.8, size = 5)  +
    scale_color_gradientn(colors = rev(ipcc_col), "") +
    geom_hline(yintercept = 0.5, linetype = "dashed") + 
    theme_cowplot() + 
    facet_wrap(~region, nrow = 2) + 
    scale_x_continuous(breaks = seq(1985, 2023, 10), limits = c(1985, 2023)) + 
    scale_y_continuous(breaks = c(seq(0, 1, by = 0.2)), limits = c(0, max(df$year_sum))) +
    labs(x = "", y = "Area fraction") + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

ggsave(last_plot(), file = "outputs/LEHI_Timeseries_v2_5km.png", height = 8, width = 12)
