library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
# library(rgdal)
library(dplyr)
library(maps)
library(ggjoy)
library(rworldmap)
library(ggalt)
library(readr)
library(lwgeom)
library(patchwork)

rm(list = ls())

select = dplyr::select

isl_names <- read_csv("data/island_name_code.csv") %>%
  select(Island_Code, Island) %>% 
  setNames(c("UNIT", "Island")) %>%
  as.data.frame()

percentile = 0.96667 # based on 30 years baseline (1985-2014)

period = c("1985-1994", "1995-2004", "2005-2014", "2015-2023")

# rescale function
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# coarse shape files, see prep_shapefile.R
load(paste0('data/isl_sf_dataframe_0.001.RData'))

#IPCC - Temperature -
ipcc_temp <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
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

ipcc_temp_4_cols <- c(rgb(153, 0, 2, maxColorValue = 255, alpha = 255),
                      rgb(196, 121, 0, maxColorValue = 255, alpha = 255),
                      rgb(112, 160, 205, maxColorValue = 255, alpha = 255),
                      rgb(0, 52, 102, maxColorValue = 255, alpha = 255))

ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))

rank_joy = function(region){
  
  # region = "island"
  # region = "region"
  
  shape = isl
  
  if (region == "region") shape$UNIT = shape$Region
  if (region == "island") shape$UNIT = shape$ISLAND_CD
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 4
    
    if (period[[i]] == "2015-2023") {
      
      max = 108 # 12 * 9 
      
    }else{
      
      max = 120 # 12 * 10
      
    }
    
    load(paste0("outputs/CRW_", percentile, "_LEHI_", period[[i]], ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    summary(tas)
    # crw <- st_intersection(tas, shape)
    crw <- st_intersection(tas, st_make_valid(shape))
    # crw <- st_intersection(tas, st_buffer(shape, 0))
    # crw$sum = range01(crw$sum)
    crw$sum = ((crw$sum-0)/(max-0))
    prov_levels <- crw %>% # Reorder levels by mean risk by privince 
      as.data.frame() %>% 
      dplyr::select(sum, UNIT) %>%
      group_by(UNIT) %>%
      mutate(unit_median = median(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$unit_median)])
    crw$UNIT <- factor(crw$UNIT, levels = levels, ordered = TRUE)
    df = table(crw$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    crw = merge(crw, df)
    crw$source = "crwSST v1.1"; crw$period = period[[i]]
    
    tas = crw
    
    tas_combined = rbind(tas_combined, tas)
    
  }
  
  # # pick large units
  # pdf(paste0("~/Desktop/joy_", region, "_selected_", percentile, ".pdf"), height = 10, width = 10)
  # 
  # big_units = tas_combined %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 100)
  # big_units = as.data.frame(big_units)
  # big_units = big_units[, 1, drop = FALSE]
  # tas_combined_sub = subset(tas_combined, UNIT %in% dplyr::pull(big_units))
  # 
  # p = tas_combined_sub %>%
  #   mutate(location_id = as.character(geometry)) %>%
  #   group_by(UNIT, period, location_id) %>%
  #   summarise(sum = mean(sum)) %>%
  #   ggplot() +
  #   geom_density(aes(x = sum, fill = period), alpha = 0.8, size = 0.05) +
  #   scale_x_continuous(
  #     limits = c(0, 0.5),
  #     expand = c(0.05, 0.01),
  #     breaks = c(0, 0.25, 0.5)) +
  #   scale_fill_manual(values = rev(ipcc_temp_4_cols), "") +
  #   facet_wrap( ~ UNIT, scales = "fixed") +
  #   # coord_fixed(ratio = 0.03) +
  #   ylab(NULL) + xlab(NULL) +
  #   theme(
  #     axis.text.y = element_blank(),
  #     axis.ticks = element_blank(),
  #     legend.position = "top")
  # 
  # print(p)
  # 
  # dev.off()
  
  
  # remove disputed EEZs (see FML 2019 to find out why)
  exclude_list = c("Area en controversia (disputed - Peruvian point of view)", 
                   "Area of overlap Australia/Indonesia", 
                   "Conflict zone China/Japan/Taiwan", 
                   "Conflict zone Japan/Russia",
                   "Conflict zone Japan/South Korea",
                   "Disputed Barbados/Trinidad & Tobago",
                   "Disputed Kenya/Somalia",
                   "Disputed Western Sahara/Mauritania",
                   "Joint development area Australia/East Timor",
                   "Joint regime Colombia/Jamaica",
                   "Joint regime Japan/Korea",
                   "Joint regime Nigeria/Sao Tome and Principe",
                   "Protected zone Australia/Papua New Guinea", 
                   "Spratly Islands", 
                   "Paracel Islands",
                   "Antarctica", 
                   "Gaza Strip")
  
  tas_combined = tas_combined[ ! tas_combined$UNIT %in% exclude_list, ]
  
  tas_combined$UNIT = gsub("&", "and", tas_combined$UNIT)
  tas_combined$UNIT = gsub(" Is.", " Islands", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub(" I.", " Island", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Congo, DRC", "DR Congo", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Bonaire, Sint-Eustasius, Saba", "Netherlands", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("St. ", "Saint ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Svalbard", "Norway", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Jan Mayen", "Norway", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("United States Minor Outlying Islands", "United States", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("United States Virgin Islands", "United States", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("French Southern and Antarctic Lands", "France", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Reunion", "France", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Virgin Islands, British", "United Kingdom", tas_combined$UNIT, fixed = T)
  
  ### just keep HadISST and COBESST. discard unecessary columns ###
  # tas_combined = subset(tas_combined, source %in% c("HadISST v1.1", "COBE v2")) # remove ERSSTv5
  tas_combined = tas_combined %>% as.data.frame() %>% select(UNIT, period, source, sum, geometry)
  tas_combined = tas_combined[!is.na(tas_combined$UNIT),]
  
  # count changes in number of unit between 1980-1989 and 2010-2019
  n1 = tas_combined %>% 
    subset(period %in% c("1980-1989")) %>%
    group_by(UNIT) %>% 
    summarise(sum = round(median(sum), 2)) %>% 
    subset(sum >= 0.66) #use upper tercile
  
  n2 = tas_combined %>% 
    subset(period %in% c("2010-2019")) %>%
    group_by(UNIT) %>% 
    summarise(sum = round(median(sum), 2)) %>% 
    subset(sum >= 0.66) #use upper tercile
  
  ##########################
  ### expand IPCC colors ###
  ##########################
  ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))
  ipcc_temp_expand = ipcc_temp_expand(length(unique(tas_combined$UNIT)))
  
  #############################
  ### save full list as csv ###
  #############################
  summary = tas_combined %>%
    group_by(UNIT, period) %>%
    summarise_each(funs(mean, sd, se = sd(.)/sqrt(n())), sum)
  
  summary = as.data.frame(summary)
  summary = summary[,c('UNIT', 'period', 'mean', 'sd', 'se')]
  summary$UNIT = as.character(summary$UNIT)
  summary <- summary[order(summary$UNIT),]
  summary[,3:5] = round(summary[,3:5], 2)
  summary$UNIT[duplicated(summary$UNIT)] <- ""
  colnames(summary) = c("Unit", "Period", "Mean", "SD", "SE")
  
  s1 = summary %>% subset(Period == "1985-1994")
  s2 = summary %>% subset(Period == "1995-2004")
  s3 = summary %>% subset(Period == "2005-2014")
  s4 = summary %>% subset(Period == "2015-2023")
  
  summary = cbind(s1, s2, s3, s4)
  summary =  summary[!is.na(summary$Unit),]
  
  write_csv(summary, paste0("outputs/", region, "_", percentile, ".csv"))
  
  
  ################
  ### plot joy ###
  ################
  
  all_unit = tas_combined %>%
    mutate(location_id = as.character(geometry)) %>%
    select(UNIT, period, location_id, sum) %>%
    group_by(UNIT, period, location_id) %>%
    summarise(sum = median(sum, na.rm = T))
  
  library(ggridges)
  
  diff = all_unit %>%
    subset(period == c("1985-1994", "2015-2023")) %>%
    group_by(period, UNIT) %>% 
    summarise(sum = mean(sum)) %>% 
    ungroup() %>% 
    group_by(UNIT) %>%
    tidyr::spread(key = period, value = sum) %>%
    mutate(difference = `2015-2023` - `1985-1994`) %>% 
    select(UNIT, difference)  
  
  p = all_unit %>%
    subset(period == c("1985-1994", "2015-2023")) %>%
    group_by(period, UNIT) %>% 
    summarise(sum = mean(sum)) %>% 
    left_join(diff) %>% 
    ggplot(aes(x = sum, y = reorder(UNIT, difference), fill = period)) +
    geom_line(aes(group = UNIT, color = difference), size = 2) + 
    geom_point(shape = 21, size = 3) +
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    # coord_fixed(ratio = 0.01) +
    theme(axis.text.y = element_text(size = 10),
          # panel.background = element_blank(),
          # panel.grid.major.x = element_blank(),
          # panel.grid.minor.x = element_blank(),
          # panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
    )
  
  print(p)
  if (region == "region") ggsave(last_plot(), file = paste0("outputs/joy_", region, "_", percentile, ".png"), height = 5, width = 6, units = "in")
  if (region == "island") ggsave(last_plot(), file = paste0("outputs/joy_", region, "_", percentile, ".png"), height = 6, width = 8, units = "in")
  
  # pdf(paste0("outputs/joy_", region, "_", percentile, ".pdf"), height = 20, width = 10)
  # print(p)
  # dev.off()
  
  if (region == "island") {
    tas_combined = tas_combined %>% mutate(UNIT = tolower(UNIT))
    tas_combined = merge(tas_combined, isl_names)
    tas_combined$UNIT = tas_combined$Island
  }
  
  return(tas_combined)
  
}

ncrmp = rank_joy("island")
ncrmp = rank_joy("region")

#########################################
### Reorder units by 2015-2023 median ###
#########################################

ncrmp %>%
  subset(period == "2015-2023") %>%
  # subset(period == "1985-1994") %>%
  # subset(period %in% c("1985-1994", "2015-2023")) %>%
  group_by(UNIT) %>% 
  mutate(med_sum = median(sum)) %>% 
  ggplot(aes(x = `sum`, y = reorder(UNIT, med_sum), fill = stat(x))) +
  geom_density_ridges_gradient(
    bandwidth = 0.005,
    alpha = 0.8,
    color = "black",
    scale = 3,
    jittered_points = T,
    position = position_points_jitter(width = 0.05, height = 0),
    # point_shape = "|",
    point_size = 0.5,
    point_alpha = 0.5,
    # quantile_lines = T,
    # vline_color = c("green"),
    # quantile_fun = median
  ) +
  # geom_density_ridges_gradient(
  #   bandwidth = 0.01,
  #   scale = 3,
  #   color = NA,
  #   # quantile_lines = T,
  #   # vline_color = c("blue"),
  #   fill = NA,
  #   quantile_fun = mean
  # ) +
  scale_fill_gradientn(colors = rev(ipcc_col), "") +
  ylab(NULL) + xlab(NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +    
  theme(axis.text.y = element_text(size = 10),
        panel.background = element_blank(),
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor.x = element_blank(),
        # panel.grid.major.y = element_blank(),
        # panel.grid.minor.y = element_blank(),
        legend.position = "none")

ggsave(last_plot(), file = "outputs/fig_crw_joy_5km_2015-2023.png", height = 9, width = 6, bg = "transparent")

set.seed(2024)
ncrmp %>%
  # subset(period %in% c("1985-1994", "2015-2023")) %>%
  ggplot(aes(x = `sum`, y = factor(UNIT, levels = sort(unique(UNIT), decreasing = T)), fill = period)) +
  geom_density_ridges_gradient(
    bandwidth = 0.005,
    color = "black",
  ) +
  scale_fill_viridis_d("") +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.y = element_text(size = 10),
        panel.background = element_blank(),
        legend.position = "top")

ggsave(last_plot(), file = "outputs/fig_s_ncrmp_a.png", height = 8, width = 10, bg = "transparent")

ncrmp = ncrmp %>% 
  subset(period %in% c("2015-2023")) %>% 
  mutate(
    location_id = as.character(geometry),
    UNIT = tolower(gsub("_&_", "_", UNIT))  # Apply gsub to replace spaces with underscores in UNIT
  ) %>%
  group_by(UNIT, location_id) %>%
  summarise(sum = median(sum, na.rm = T)) %>% 
  as.data.frame()

#you have to repeaet ranking because some units are ranked at same spots
df1 = ncrmp %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 3) %>% top_n(15, m); df1 = df1 %>% top_n(15)
df2 = ncrmp %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 3) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$UNIT); sub #see top 15 bottom 15
ncrmp_sub = subset(ncrmp, UNIT %in% sub) #subset
ncrmp_sub = ncrmp_sub %>% group_by(UNIT) %>% mutate(m = median(sum)) %>% arrange(UNIT, m)
ncrmp_sub = ncrmp_sub[,c("UNIT", "sum")]; ncrmp_sub = as.data.frame(ncrmp_sub); ncrmp_sub = ncrmp_sub[1:2]; ncrmp_sub$class = "ncrmp"

ncrmp_sub %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>%
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 3, alpha = 0.8, show.legend = F) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(-0.1, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(ncrmp_sub$UNIT)))) +
  ylab(NULL) + xlab(NULL) +
  # coord_fixed(ratio = 0.05) + 
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")
# labs(tag = "(c) Exclusive Economic Zone"))
ggsave(last_plot(), file = "outputs/fig_crw_joy_5km_2015-2023.png", height = 6, width = 6, bg = "transparent")
