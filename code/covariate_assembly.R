### Covariate Dataset Assembly
### May 28, 2024
### Heili Lowman

#### README ####

# The following script will assemble covariates for analysis with
# the HBEF aquatic insect emergence data.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(patchwork)
library(ggcorrplot)

# Load necessary datasets.

# First, the response variables.
# Date and magnitude of leaf season peak.
leaf_peak_dat <- readRDS("data_working/peak_emerge_dates_052824.rds")

# Date and magnitude of later leaf/cooling season peak.
later_peak_dat <- readRDS("data_working/peak_emerge_dates_cooling_052824.rds")

# Cumulative annual magnitude of insect emergence.
sum_emerge_dat <- readRDS("data_working/sum_emerge_052824.rds")

# Next, the covariates.
# Snowmelt data from Danielle.
melt_data <- read_csv("data_raw/HB_spring_melt_duration_emerge_051724.csv")

# Chemistry and algal data from HBWater.
ws_data <- read_csv("data_raw/HBEFdata_Current_2024-05-28.csv")

# Edited seasonal dates based on phenology.
season_data <-  read_csv("data_working/season_dates_pheno_wpeak_052824_trimmed.csv")

#### Tidy Emergence Indices ####

# Trim leaf peak emergence dataset to columns of interest.
leaf_peak_dat <- leaf_peak_dat %>%
  select(watershed, Year, total_count, jday) %>%
  rename(leaf_peak_count = total_count,
         leaf_peak_jday = jday)

# Trim later peak emergence dataset to columns of interest.
later_peak_dat <- later_peak_dat %>%
  select(watershed, Year, total_count, jday) %>%
  rename(later_peak_count = total_count,
         later_peak_jday = jday)

# Join with cumulative emergence.
resp_var <- full_join(sum_emerge_dat, leaf_peak_dat)
resp_var <- full_join(resp_var, later_peak_dat)

#### Generate Covariates ####

##### Previous Year's Emergence #####

# Calculate lagged cumulative emergence values.
resp_var <- resp_var %>%
  group_by(watershed) %>%
  mutate(annual_count_lag1 = lag(annual_count)) %>%
  ungroup()

##### Snowmelt #####

# Trim down to include max SWE and snowmelt duration.
melt_trim <- melt_data %>%
  mutate(watershed = case_when(site_id == "W1" ~ "1",
                               site_id == "W2" ~ "2",
                               site_id == "W3" ~ "3",
                               site_id == "W4" ~ "4",
                               site_id == "W5" ~ "5",
                               site_id == "W6" ~ "6",
                               site_id == "W9" ~ "9",
                               site_id == "HBK" ~ "HBK")) %>%
  select(watershed, Yr, swe_max_mm, duration_days) %>%
  rename(Year = Yr,
         melt_duration_days = duration_days)

##### Algal chl a ##### 

# Need to tidy the HB data some prior to aggregating.
ws_trim <- ws_data %>%
  select(site, date, chla_M, chla_T, chla_MT, chla_WM) %>%
  # and filter only for watersheds of interest
  filter(site %in% c("W1", "W2", "W3", "W4", "W5", "W6", "W9", "HBK")) %>%
  # as well as dates of interest (no data for 2017)
  filter(date > mdy(as.character("1/1/18"))) %>%
  mutate(Year = year(date))

# And need to format dates for proper filtering.
warm_season_data <- season_data %>%
  mutate(start_date = mdy(Start_Date),
         end_date = mdy(End_Date)) %>%
  # and I only need warming & leaf dates for now
  filter(Season %in% c("Leaf", "Warming"))

# And now assign seasons to the chl a data so we can
# group by season (since these differ by watershed).
ws_trim <- ws_trim %>%
  mutate(season = case_when(date >= ymd("2018-04-26") & date < "2018-10-25" |
                              date >= ymd("2019-05-06") & date < "2019-11-04" |
                              date >= ymd("2020-05-17") & date < "2020-11-16" |
                              date >= ymd("2021-05-02") & date < "2021-10-13" |
                              date >= ymd("2022-05-14") & date < "2022-09-19" |
                              date >= ymd("2023-04-30") & date < "2023-10-24" ~ "Leaf",
                            TRUE ~ NA))

# Appears there are very few records during the warming season,
# so I will only aggregate algal records for the leaf season
# for the time being.
algal_leaf <- ws_trim %>%
  filter(season == "Leaf") %>%
  group_by(site, Year, season) %>%
  summarize(mean_chla_T = mean(chla_T, na.rm = TRUE)) %>%
  ungroup()

# And properly format for joining below.
algal_trim <- algal_leaf %>%
  mutate(watershed = case_when(site == "W1" ~ "1",
                               site == "W2" ~ "2",
                               site == "W3" ~ "3",
                               site == "W4" ~ "4",
                               site == "W5" ~ "5",
                               site == "W6" ~ "6",
                               site == "W9" ~ "9",
                               site == "HBK" ~ "HBK")) %>%
  select(watershed, Year, mean_chla_T) %>%
  rename(mean_chla_T_leaf = mean_chla_T) %>%
  mutate(mean_chla_T_leaf_lag1 = lag(mean_chla_T_leaf))

# Also, going to create a separate dataset that aggregates chl a
# leading up to only peak emergence in each watershed.
leaf_peak_dat$peak_Date <- as.Date(leaf_peak_dat$leaf_peak_jday-1, 
                              origin = paste0(leaf_peak_dat$Year, 
                                              "-01-01"))
leaf_peak_trim <- leaf_peak_dat %>%
  select(watershed, Year, peak_Date)

ws_formatted <- ws_trim %>%
  mutate(watershed = case_when(site == "W1" ~ "1",
                               site == "W2" ~ "2",
                               site == "W3" ~ "3",
                               site == "W4" ~ "4",
                               site == "W5" ~ "5",
                               site == "W6" ~ "6",
                               site == "W9" ~ "9",
                               site == "HBK" ~ "HBK"))

algal_pre_peak <- full_join(ws_formatted, leaf_peak_trim, 
                            by = c("watershed", "Year")) %>%
  group_by(watershed, Year) %>%
  filter(date <= peak_Date) %>%
  summarize(mean_chla_T_to_peak = mean(chla_T, na.rm = TRUE)) %>%
  ungroup()

#### Join ####

# Join with snowmelt data.
all_var <- full_join(resp_var, melt_trim)

# Join with algal data.
all_var <- full_join(all_var, algal_trim)
all_var <- full_join(all_var, algal_pre_peak)

#### Visualize ####

# Examine some of the correlation plots.
plot(all_var$annual_count_lag1, all_var$annual_count) # no trend
plot(all_var$melt_duration_days, all_var$annual_count) # weakly negative
plot(all_var$swe_max_mm, all_var$annual_count) # weakly positive
plot(all_var$melt_duration_days, all_var$leaf_peak_count) # no trend
plot(all_var$melt_duration_days, all_var$leaf_peak_jday) # no trend
plot(all_var$mean_chla_T_leaf, all_var$annual_count) # neg correlated
plot(all_var$mean_chla_T_leaf_lag1, all_var$annual_count) # even more negative

# And create more official plots of the above.

##### Annual Magnitude #####

(fig1_count <- ggplot(all_var, aes(x = annual_count_lag1,
                                  y = annual_count)) +
    geom_point(color = "#E29244", size = 3) +
    labs(x = "Previous Year's Count (Individuals)",
         y = "Annual Count (Individuals)") +
    theme_bw())

(fig2_count <- ggplot(all_var, aes(x = mean_chla_T_leaf,
                                   y = annual_count)) +
    geom_point(color = "#F19E1F", size = 3) +
    labs(x = "Mean Leaf Season Chl a",
         y = "Annual Count (Individuals)") +
    theme_bw())

(fig3_count <- ggplot(all_var, aes(x = mean_chla_T_leaf_lag1,
                                   y = annual_count)) +
    geom_point(color = "#FCA601", size = 3) +
    labs(x = "Previous Year's Mean Leaf Season Chl a",
         y = "Annual Count (individuals)") +
    theme_bw())

(fig4_count <- ggplot(all_var, aes(x = melt_duration_days,
                                   y = annual_count)) +
    geom_point(color = "#E58609", size = 3) +
    labs(x = "Snowmelt Duration (days)",
         y = "Annual Count (individuals)") +
    theme_bw())

(fig5_count <- ggplot(all_var, aes(x = swe_max_mm,
                                   y = annual_count)) +
    geom_point(color = "#C17622", size = 3) +
    labs(x = "Max. Snow Water Equivalent (mm)",
         y = "Annual Count (individuals)") +
    theme_bw())

(fig_count_all <- fig1_count + fig2_count + fig3_count + fig4_count + fig5_count)

# ggsave(plot = fig_count_all,
#        filename = "figures/annual_covar_060424.jpg",
#        width = 30,
#        height = 20,
#        units = "cm")

##### Peak Magnitude #####

(fig1_peak <- ggplot(all_var, aes(x = annual_count_lag1,
                                  y = leaf_peak_count)) +
    geom_point(color = "#79926E", size = 3) +
    labs(x = "Previous Year's Annual Count (individuals)",
         y = "Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig2_peak <- ggplot(all_var, aes(x = annual_count,
                                  y = leaf_peak_count)) +
    geom_point(color = "#51A8B0", size = 3) +
    labs(x = "Annual Count (individuals)",
         y = "Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig3_peak <- ggplot(all_var, aes(x = melt_duration_days,
                                  y = leaf_peak_count)) +
    geom_point(color = "#61B3E1", size = 3) +
    labs(x = "Snowmelt duration (days)",
         y = "Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig4_peak <- ggplot(all_var, aes(x = swe_max_mm,
                                  y = leaf_peak_count)) +
    geom_point(color = "#64B3F9", size = 3) +
    labs(x = "Max. Snow Water Equivalent (mm)",
         y = "Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig5_peak <- ggplot(all_var, aes(x = mean_chla_T_to_peak,
                                  y = leaf_peak_count)) +
    geom_point(color = "#64B3F9", size = 3) +
    labs(x = "Mean chl a Before Peak",
         y = "Peak Emergence Magnitude (individuals)") +
    scale_x_log10() +
    theme_bw())

(fig_peak_all <- fig1_peak + fig2_peak + fig3_peak + fig4_peak + fig5_peak)
 
# ggsave(plot = fig_peak_all,
#        filename = "figures/peak_covar_060424.jpg",
#        width = 30,
#        height = 20,
#        units = "cm")

##### Peak Timing #####

(fig1_peak_time <- ggplot(all_var, aes(x = leaf_peak_count,
                                       y = leaf_peak_jday)) +
    geom_point(color = "#5CA7F8", size = 3) +
    labs(x = "Peak Emergence Magnitude (individuals)",
         y = "Peak Emergence DOY") +
    theme_bw())

(fig2_peak_time <- ggplot(all_var, aes(x = annual_count,
                                       y = leaf_peak_jday)) +
    geom_point(color = "#549CF7", size = 3) +
    labs(x = "Annual Count (individuals)",
         y = "Peak Emergence DOY") +
    theme_bw())

(fig3_peak_time <- ggplot(all_var, aes(x = melt_duration_days,
                                       y = leaf_peak_jday)) +
    geom_point(color = "#4C91F7", size = 3) +
    labs(x = "Snowmelt duration (days)",
         y = "Peak Emergence DOY") +
    theme_bw())

(fig4_peak_time <- ggplot(all_var, aes(x = swe_max_mm,
                                       y = leaf_peak_jday)) +
    geom_point(color = "#5188E5", size = 3) +
    labs(x = "Max. Snow Water Equivalent (mm)",
         y = "Peak Emergence DOY") +
    theme_bw())

(fig_peaktime_all <- fig1_peak_time + fig2_peak_time + 
    fig3_peak_time + fig4_peak_time)

# ggsave(plot = fig_peaktime_all,
#        filename = "figures/peaktime_covar_060424.jpg",
#        width = 20,
#        height = 20,
#        units = "cm")

##### Late Peak #####

(fig1_latepeak <- ggplot(all_var, aes(x = leaf_peak_count,
                                      y = later_peak_count)) +
    geom_point(color = "#597FCD", size = 3) +
    scale_y_log10() +
    labs(x = "Peak Emergence Magnitude (individuals)",
         y = "Later Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig2_latepeak <- ggplot(all_var, aes(x = annual_count,
                                      y = later_peak_count)) +
    geom_point(color = "#6176B6", size = 3) +
    scale_y_log10() +
    labs(x = "Annual Count (individuals)",
         y = "Later Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig3_latepeak <- ggplot(all_var, aes(x = mean_chla_T_leaf,
                                      y = later_peak_count)) +
    geom_point(color = "#6B6D9F", size = 3) +
    scale_y_log10() +
    labs(x = "Mean Leaf Season Chl a",
         y = "Later Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig_latepeak_all <- fig1_latepeak + fig2_latepeak + fig3_latepeak)

# ggsave(plot = fig_latepeak_all,
#        filename = "figures/latepeak_covar_060424.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

# And finally generate a summary correlation plot for comparison.

# Select only variables of interest.
all_var_trim <- all_var %>%
  select(annual_count:mean_chla_T_to_peak) %>%
  # remove years for which we have no data
  drop_na(annual_count) %>%
  # and replace NaN values
  mutate_all(~ifelse(is.nan(.), NA, .))

# Calculate correlations.
corr_var <- cor(all_var_trim, use = "complete.obs")

ggcorrplot(corr_var,
           type = "lower",
           lab = TRUE)

# Appears the only one not explore above is the fall peak magnitude's
# correlation with the previous summer's mean chl a concentration.

# End of script.
