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

#### Join ####

# Join with snowmelt data.
all_var <- full_join(resp_var, melt_trim)

# Join with algal data.
all_var <- full_join(all_var, algal_trim)

#### Visualize ####

# Examine some of the correlation plots.
plot(all_var$annual_count_lag1, all_var$annual_count) # no trend

plot(all_var$melt_duration_days, all_var$annual_count) # weakly negative
plot(all_var$melt_duration_days, all_var$leaf_peak_count) # no trend
plot(all_var$melt_duration_days, all_var$leaf_peak_jday) # no trend

plot(all_var$mean_chla_T_leaf, all_var$annual_count) # negatively correlated
plot(all_var$mean_chla_T_leaf_lag1, all_var$annual_count) # even more negative

# End of script.
