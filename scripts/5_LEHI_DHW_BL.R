library(colorRamps)
library(ggpubr)
library(dplyr)
library(patchwork)
library(cowplot)
library(terra)
library(sf)
library(data.table)
library(doParallel)
library(raster)
library(ggplot2)
library(tidyr)
library(lmtest)
library(broom)

rm(list = ls())

select = dplyr::select

load("outputs/CRW_BAA_5km_coast_time.RData"); df1 = df_time
load("outputs/CRW_BAA_7daymax_5km_coast_time.RData"); df2 = df_time
load("outputs/CRW_BH_5km_coast_time.RData"); df3 = df_time
load("outputs/CRW_DHW_5km_coast_time.RData"); df4 = df_time

df1$index = "Bleaching_Area_Alert"
df2$index = "Bleaching_Area_Alert_7daymax"
df3$index = "Bleaching_Hotspot"
df4$index = "Degree_Heating_Week"

clim = rbind(df1, df2, df3, df4) 

clim %>%
  .[, .(v = mean(v)), by = .(year, region, index)] %>%
  subset(index %in% c("Bleaching_Area_Alert", "Degree_Heating_Week")) %>%
  ggplot(aes(x = year, y = v, fill = region, group = region)) +
  geom_line(alpha = 0.5) +
  geom_point(shape = 21, size = 5, alpha = 0.8) +
  scale_fill_viridis_d("") + 
  facet_wrap(~index, scales = "free") + 
  theme_cowplot() + 
  theme(axis.title = element_blank())

ggsave(last_plot(), file = "outputs/climate_indices.png", width = 10, height = 5)

rm(df1, df2, df3, df4, df_time)

# load LEHI results
percentile = 0.96667 #based on 30 years baseline (1985-2014)

##############################################
### load area fraction time series results ###
##############################################

region_list = c("NCRMP",
                "NWHI", 
                "MHI", 
                "PRIA", 
                "MARIAN", 
                "SAMOA")

lehi_time <- vector("list", length(region_list))

# Load and process data for each region
for (y in seq_along(region_list)) {
  
  load(paste0("outputs/CRW_timeseries_0.96667_", region_list[y], "_5km.RData"))
  
  crw <- yy_anom %>%
    mutate(data = "CRW", region = region_list[y])
  
  lehi_time[[y]] <- crw
}

df <- do.call(rbind, lehi_time)

lehi <- df %>%
  tidyr::separate(time, c("Year", "Month"), sep = "-") %>%
  mutate(Month = sprintf("%02d", as.numeric(Month)),
         Day = "01",
         Time = as.Date(paste(Year, Month, Day, sep = "-")))

rownames(lehi) <- NULL

rm(crw, df, lehi_time, percentile, y, yy_anom)

table(clim$region)
table(lehi$region)

colnames(clim) = tolower(colnames(clim))
colnames(lehi) = tolower(colnames(lehi))

indices = c("Bleaching_Area_Alert", "Bleaching_Area_Alert_7daymax", "Bleaching_Hotspot", "Degree_Heating_Week")

plot_ncrmp = list()
plot_region = list()

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

detrend = T

for (i in 1:length(indices)) {
  
  # i = 1
  
  ccf = NULL
  
  # plots = list()
  
  # par(mfrow = c(3, 2))
  
  for (r in 1:length(region_list)) {
    
    # r = 1
    
    if (region_list[r] == "NCRMP") {
      
      clim_i = clim %>% 
        subset(index == indices[i]) %>%
        group_by(year) %>%
        filter(n_distinct(month) == 12) %>%
        summarise(clim = mean(v))
      
      lehi_i = lehi %>% 
        # group_by(year, month) %>%
        group_by(year) %>%
        filter(n_distinct(month) == 12) %>%
        summarise(lehi = mean(year_sum))
      
    } else {
      
      clim_i = clim %>% 
        subset(index == indices[i]) %>%
        subset(region == region_list[r]) %>%
        # group_by(year, month) %>%
        group_by(year) %>%
        filter(n_distinct(month) == 12) %>%
        summarise(clim = mean(v))
      
      lehi_i = lehi %>% 
        subset(region == region_list[r]) %>%
        # group_by(year, month) %>%
        group_by(year) %>%
        filter(n_distinct(month) == 12) %>%
        summarise(lehi = mean(year_sum))
      
    }
    
    # Convert t1 and t2 to date format
    clim_i <- clim_i %>%
      # mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      mutate(date = as.Date(paste(year, "01", "01", sep = "-"))) %>%
      select(date, clim)
    
    lehi_i<- lehi_i %>%
      # mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      mutate(date = as.Date(paste(year, "01", "01", sep = "-"))) %>%
      select(date, lehi)
    
    # Merge the datasets
    merged_data <- inner_join(clim_i, lehi_i, by = "date")
    
    # Calculate CCF
    # ccf(merged_data$clim, merged_data$lehi, lag.max = 10, plot = T, main = region_list[r])
    
    n <- length(merged_data$clim)
    threshold <- 2 / sqrt(n)
    
    if (detrend == T) {
      
      merged_data$clim <- resid(lm(clim ~ date, data = merged_data))
      merged_data$lehi <- resid(lm(lehi ~ date, data = merged_data))
      
    }
    
    ccf_data <- ccf(merged_data$clim, merged_data$lehi, lag.max = 10, plot = F)
    
    ccf_df <- data.frame(
      lag = ccf_data$lag,
      correlation = ccf_data$acf,
      region = region_list[r],
      significance_threshold = threshold
    )
    
    # Optionally, add a column indicating whether the correlation is significant
    ccf_df$significant <- abs(ccf_df$correlation) > ccf_df$significance_threshold
    
    ccf = rbind(ccf, ccf_df)
    
    # Use the Granger Causality Test to test for predictive causality.
    
    # Purpose: To test if one time series can predict another.
    grangertest(clim ~ lehi, order = 1, data = merged_data)
    grangertest(lehi ~ clim, order = 1, data = merged_data)
    
    # Calculate F-statistics for multiple lags
    max_lag <- 10
    
    f_stats <- sapply(1:max_lag, function(lag) {
      test_result <- grangertest(clim ~ lehi, order = lag, data = merged_data)
      test_result$F[2]
    })
    
    # Create data frame
    f_stats_df <- data.frame(
      lag = 1:max_lag,
      f_stat = f_stats
    )
    
    p1 = ggplot(f_stats_df, aes(x = lag, y = f_stat, fill = f_stat)) +
      geom_line() +
      geom_point(size = 5, shape = 21) +
      labs(title = region_list[r],
           x = "Lag",
           y = "F-statistic")+
      scale_x_continuous(breaks = 1:max_lag)
    
    # Calculate p-values for multiple lags
    p_values <- sapply(1:max_lag, function(lag) {
      test_result <- grangertest(clim ~ lehi, order = lag, data = merged_data)
      broom::tidy(test_result)$p.value[2]
    })
    
    # Create data frame
    p_values_df <- data.frame(
      lag = 1:max_lag,
      p_value = p_values
    )
    
    p2 = ggplot(p_values_df, aes(x = lag, y = p_value, fill = p_value)) +
      geom_line() +
      geom_point(size = 5, shape = 21) +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
      labs(title = "",
           x = "Lag",
           y = "p-value") +
      scale_x_continuous(breaks = 1:max_lag)
    
    # p = p1 + p2
    # 
    # plots[[r]] <- p
    
  }
  
  # Define the title based on the detrend condition
  plot_title <- if (detrend) {
    paste0(indices[i], " (Detrended)")
  } else {
    indices[i]
  }
  
  ccf$region <- factor(ccf$region, levels = region_list)
  
  ccf_filtered <- ccf %>% filter(region == "NCRMP")
  
  # Add the plot with asterisks for significant correlations
  p = ggplot(ccf_filtered, aes(x = lag, y = correlation, fill = correlation)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-2 / sqrt(nrow(merged_data)), 2 / sqrt(nrow(merged_data))), 
               linetype = "dashed", color = "red") +
    geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
    scale_fill_gradientn(colors = rev(ipcc_col), "") +
    ylim(-0.5, 1) + 
    ggtitle(plot_title) + 
    labs(x = "lag (years)") + 
    facet_wrap(~region, ncol = 1) + 
    theme_minimal() +
    geom_text(data = subset(ccf_filtered, significant), 
              aes(label = "*"), 
              size = 10,
              vjust = -0.1, color = "red")
  
  plot_ncrmp[[i]] <- p
  
  ccf_filtered <- ccf %>% filter(region != "NCRMP")
  
  # Add the plot with asterisks for significant correlations
  p = ggplot(ccf_filtered, aes(x = lag, y = correlation, fill = correlation)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-2 / sqrt(nrow(merged_data)), 2 / sqrt(nrow(merged_data))), 
               linetype = "dashed", color = "red") +
    geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
    scale_fill_gradientn(colors = rev(ipcc_col), "") +
    ylim(-0.5, 1) + 
    ggtitle(plot_title) + 
    labs(x = "lag (years)") + 
    facet_wrap(~region, ncol = 1) + 
    theme_minimal() +
    geom_text(data = subset(ccf_filtered, significant), 
              aes(label = "*"), 
              size = 10,
              vjust = -0.1, color = "red")
  
  plot_region[[i]] <- p
  
}

# Define a suffix based on the detrend condition
suffix <- if (detrend) "_detrend" else ""

# Save plots with conditional filenames
plot_ncrmp[[1]] + plot_ncrmp[[4]] 
ggsave(last_plot(), file = paste0("outputs/ccf_BA_DHW_NCRMP", suffix, ".png"), width = 10, height = 6)

plot_ncrmp[[2]] + plot_ncrmp[[4]] 
ggsave(last_plot(), file = paste0("outputs/ccf_BA7days_DHW_NCRMP", suffix, ".png"), width = 10, height = 6)

plot_ncrmp[[3]] + plot_ncrmp[[4]] 
ggsave(last_plot(), file = paste0("outputs/ccf_BAHS_DHW_NCRMP", suffix, ".png"), width = 10, height = 6)

plot_region[[1]] + plot_region[[4]] 
ggsave(last_plot(), file = paste0("outputs/ccf_BA_DHW_Region", suffix, ".png"), width = 10, height = 12)
