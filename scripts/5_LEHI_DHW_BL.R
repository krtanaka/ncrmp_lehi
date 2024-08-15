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

rm(list = ls())

select = dplyr::select

# load regional DHW and BL values
load("outputs/CRW_BAA_5km_coast.RData"); df1 = df
load("outputs/CRW_BAA_7daymax_5km_coast.RData"); df2 = df
load("outputs/CRW_BH_5km_coast.RData"); df3 = df
load("outputs/CRW_DHW_5km_coast.RData"); df4 = df

common_columns <- intersect(names(df1), names(df4))

df1 <- df1[, ..common_columns]
df2 <- df2[, ..common_columns]
df3 <- df3[, ..common_columns]
df4 <- df4[, ..common_columns]

df1$index = "BAA"
df2$index = "BAA_7daymax"
df3$index = "BH"
df4$index = "DHW"

df_xy = rbind(df1, df2, df3, df4) %>% 
  dplyr::select(last_col(), everything())

df_xy$mean <- rowMeans(df_xy[, -(1:5), with = FALSE])

df_xy %>%
  filter(region == "Hawaii") %>% 
  filter(island == "HAW") %>% 
  ggplot(aes(x, y, fill = mean)) +  
  geom_point(shape = 21, size = 5, alpha = 0.8) + 
  scale_fill_gradientn(colors = matlab.like(100)) + 
  facet_grid(~index, scales = "free")

load("outputs/CRW_BAA_5km_coast_time.RData"); df1 = df_time
load("outputs/CRW_BAA_7daymax_5km_coast_time.RData"); df2 = df_time
load("outputs/CRW_BH_5km_coast_time.RData"); df3 = df_time
load("outputs/CRW_DHW_5km_coast_time.RData"); df4 = df_time

df1$index = "BAA"
df2$index = "BAA_7daymax"
df3$index = "BH"
df4$index = "DHW"

df_time = rbind(df1, df2, df3, df4) 

df_time %>%
  # filter(index == "BAA_7daymax") %>% 
  .[, .(v = mean(v)), by = .(year, region, index)] %>%
  ggplot(aes(x = year, y = v, fill = index, group = index)) + 
  geom_line() +
  geom_point(shape = 21, size = 5) + 
  facet_wrap(~region, scales = "free")

rm(common_columns, df, df1, df2)

# load LEHI results

percentile = 0.96667 #based on 30 years baseline (1985-2014)

##############################################
### load area fraction time series results ###
##############################################

region_list = c("MARIAN", 
                "MHI", 
                "NWHI", 
                "PRIA", 
                "SAMOA",
                "NCRMP")

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

rm(crw, df, lehi_time, percentile, region_list, y, yy_anom)

table(df_time$region)
table(lehi$region)

t1 = df_time %>% 
  subset(region == "Guam") %>% 
  # subset(index == "DHW") %>% 
  subset(index == "BH") %>% 
  group_by(year, month) %>% 
  summarise(v = mean(v))

t2 = lehi %>% 
  subset(region == "Guam") %>% 
  group_by(Year, Month) %>% 
  summarise(v = mean(year_sum))

# Step 1: Ensure the data is in Date format and merge
library(dplyr)
library(tidyr)

# Convert t1 and t2 to date format
t1 <- t1 %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  select(date, v)

t2 <- t2 %>%
  mutate(date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  select(date, v)

# Step 2: Merge the datasets
merged_data <- inner_join(t1, t2, by = "date", suffix = c("_t1", "_t2"))

# Step 3: Create lagged versions of t2
merged_data <- merged_data %>%
  mutate(lag1_t2 = lag(v_t2, 1),
         lag2_t2 = lag(v_t2, 2),
         lag3_t2 = lag(v_t2, 3))  # Add more lags as needed

# Step 4: Correlation analysis
correlations <- cor(merged_data$v_t1, merged_data[, c("lag1_t2", "lag2_t2", "lag3_t2")], use = "complete.obs")

print(correlations)

# Calculate CCF
ccf_data <- ccf(merged_data$v_t1, merged_data$v_t2, lag.max = 10, plot = FALSE)

# Create a data frame for ggplot
ccf_df <- data.frame(
  lag = ccf_data$lag,
  correlation = ccf_data$acf
)

# Plot using ggplot2
ggplot(ccf_df, aes(x = lag, y = correlation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(-2/sqrt(nrow(merged_data)), 2/sqrt(nrow(merged_data))), 
             linetype = "dashed", color = "red") +
  labs(title = "Cross-Correlation Function (CCF)",
       x = "Lag",
       y = "Correlation")

# Step 5: Cross-Correlation Function (CCF)
ccf(merged_data$v_t1, merged_data$v_t2, lag.max = 12, plot = F)

# Use the Granger Causality Test to test for predictive causality.
library(lmtest)
# Purpose: To test if one time series can predict another.
grangertest(v_t1 ~ v_t2, order = 1, data = merged_data)
grangertest(v_t2 ~ v_t1, order = 1, data = merged_data)

library(lmtest)
library(broom)

# Calculate F-statistics for multiple lags
max_lag <- 12
f_stats <- sapply(1:max_lag, function(lag) {
  test_result <- grangertest(v_t1 ~ v_t2, order = lag, data = merged_data)
  test_result$F[2]
})

# Create data frame
f_stats_df <- data.frame(
  lag = 1:max_lag,
  f_stat = f_stats
)

# Plot using ggplot2
ggplot(f_stats_df, aes(x = lag, y = f_stat, color = f_stat)) +
  geom_line() +
  geom_point(size = 5) +
  labs(title = "Granger Causality F-statistics by Lag",
       x = "Lag",
       y = "F-statistic")+ 
  scale_x_continuous(breaks = 1:max_lag) 

library(lmtest)
library(ggplot2)

# Calculate p-values for multiple lags
max_lag <- 12
p_values <- sapply(1:max_lag, function(lag) {
  test_result <- grangertest(v_t1 ~ v_t2, order = lag, data = merged_data)
  broom::tidy(test_result)$p.value[2]
})

# Create data frame
p_values_df <- data.frame(
  lag = 1:max_lag,
  p_value = p_values
)

# Plot using ggplot2
ggplot(p_values_df, aes(x = lag, y = p_value)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkblue") +
  # geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(title = "Granger Causality p-values by Lag",
       x = "Lag",
       y = "p-value") + 
  scale_x_continuous(breaks = 1:max_lag)


