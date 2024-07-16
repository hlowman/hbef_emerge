### Covariate Dataset Assembly
### May 28, 2024
### Heili Lowman

#### README ####

# The following script will assemble covariates for analysis with
# the HBEF aquatic insect emergence data.

# After multiple changes, this assembly was reviewed on July 2, 2024.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(patchwork)
library(ggcorrplot)
library(viridis)

# Load necessary datasets.

##### Response Variables #####

# Date and magnitude of leaf season peak (all taxa).
leaf_peak_dat <- readRDS("data_working/peak_emerge_dates_062824.rds")
# Removing years for which data is not complete
# W4 2020
# W5 2022, 2023
leaf_peak_dat <- leaf_peak_dat[-c(12, 17, 18),]
# 26 unique total watershed/year combinations

# And for stoneflies & caddisflies only (also removing incomplete years).
leaf_peak_dat_sf <- readRDS("data_working/peak_emerge_sf_dates_062824.rds")
leaf_peak_dat_sf <- leaf_peak_dat_sf[-c(12, 17, 18),]
leaf_peak_dat_cf <- readRDS("data_working/peak_emerge_cf_dates_062824.rds")
leaf_peak_dat_cf <- leaf_peak_dat_cf[-c(12, 17, 18),]

# Magnitude of 3 weeks (peak emergence +/-1 week for all taxa).
leaf_peak_3wk_dat <- readRDS("data_working/peak_emerge_3wk_062824.rds")
leaf_peak_3wk_dat <- leaf_peak_3wk_dat[-c(12, 17, 18),]

# Date and magnitude of later leaf/cooling season peak (all taxa).
later_peak_dat <- readRDS("data_working/peak_emerge_dates_cooling_062824.rds")
later_peak_dat <- later_peak_dat[-c(12, 17),]

# Cumulative annual magnitude of insect emergence.
sum_emerge_dat <- readRDS("data_working/sum_emerge_062824.rds")
sum_emerge_dat <- sum_emerge_dat[-c(12, 17, 18),]

# And for stoneflies and caddisflies only.
sum_emerge_dat_sf <- readRDS("data_working/sum_emerge_sf_062824.rds")
sum_emerge_dat_sf <- sum_emerge_dat_sf[-c(12, 17, 18),]
sum_emerge_dat_cf <- readRDS("data_working/sum_emerge_cf_062824.rds")
sum_emerge_dat_cf <- sum_emerge_dat_cf[-c(12, 17, 18),]

##### Covariates #####

# Not going to worry about trimming these down since they'll be joined to
# the already-trimmed response variable data.

# Snowmelt data from Danielle.
melt_data <- read_csv("data_raw/HB_spring_melt_duration_emerge_051724.csv")

# Weekly chemistry and algal data from HBWater.
ws_data <- read_csv("data_raw/HBEFdata_Current_2024-05-28.csv")

# Edited seasonal dates based on phenology.
season_data <-  read_csv("data_working/season_dates_pheno_wpeak_052824_trimmed.csv")
season_data <- season_data[-c(135, 140, 141, 144),] # removing W1 2021 data
# since all other years for which data is incomplete are reflected herein

# Cumulative degree days & rate of change.
# Note, no available temperature data for HBK and poor coverage for some others.
ddays_data <- readRDS("data_working/sum_degreedays_070224.rds")

# And flow/temperature summary metrics, including 2yr and 10yr flood recurrences.
qt_data <- read_csv("data_raw/QT_15min_EmergeSeasons_SummaryStats_v2.csv")

#### Tidy Emergence Indices ####

# Trim leaf peak emergence dataset to columns of interest.
leaf_peak_dat <- leaf_peak_dat %>%
  select(watershed, Year, total_count, jday) %>%
  rename(leaf_peak_count = total_count,
         leaf_peak_jday = jday)

leaf_peak_dat_sf <- leaf_peak_dat_sf %>%
  select(watershed, Year, total_count, jday) %>%
  rename(leaf_peak_sf_count = total_count,
         leaf_peak_sf_jday = jday)

leaf_peak_dat_cf <- leaf_peak_dat_cf %>%
  select(watershed, Year, total_count, jday) %>%
  rename(leaf_peak_cf_count = total_count,
         leaf_peak_cf_jday = jday)

leaf_peak_3wk_dat <- leaf_peak_3wk_dat %>%
  rename(leaf_peak_3wk_count = sum_total_3wk)

# Trim later peak emergence dataset to columns of interest.
later_peak_dat <- later_peak_dat %>%
  select(watershed, Year, total_count, jday) %>%
  rename(later_peak_count = total_count,
         later_peak_jday = jday)

# And rename the cumulative column in the stonefly
# and caddisfly data.
sum_emerge_dat_sf <- sum_emerge_dat_sf %>%
  rename(annual_count_sf = annual_count)
sum_emerge_dat_cf <- sum_emerge_dat_cf %>%
  rename(annual_count_cf = annual_count)

# Join with cumulative emergence.
resp_var <- full_join(sum_emerge_dat, sum_emerge_dat_sf)
resp_var <- full_join(resp_var, sum_emerge_dat_cf)
resp_var <- full_join(resp_var, leaf_peak_dat)
resp_var <- full_join(resp_var, leaf_peak_dat_sf)
resp_var <- full_join(resp_var, leaf_peak_dat_cf)
resp_var <- full_join(resp_var, leaf_peak_3wk_dat)
resp_var <- full_join(resp_var, later_peak_dat)

#### Generate Covariates ####

##### Previous Year's Emergence #####

# Calculate lagged cumulative annual emergence values.
resp_var <- resp_var %>%
  group_by(watershed) %>%
  mutate(annual_count_lag1 = lag(annual_count),
         annual_count_cf_lag1 = lag(annual_count_cf),
         annual_count_sf_lag1 = lag(annual_count_sf)) %>%
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

# Note, this corresponds to the same year in every case,
# since max SWE is typically achieved in Feb/March prior to the
# onset of snowmelt.

##### Algal chl a ##### 

# Note, no data for HBK, but otherwise a fairly complete
# record for other sites 2018-2022 (5 years).

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
# group by season.

# There are very few records during the warming season,
# so I will only aggregate algal records for the leaf season
# for the time being (which are all the same due to remote
# sensing data resolution).
ws_trim <- ws_trim %>%
  mutate(season = case_when(date >= ymd("2018-04-26") & date < ymd("2018-10-25") |
                              date >= ymd("2019-05-06") & date < ymd("2019-11-04") |
                              date >= ymd("2020-05-17") & date < ymd("2020-11-16") |
                              date >= ymd("2021-05-02") & date < ymd("2021-10-13") |
                              date >= ymd("2022-05-14") & date < ymd("2022-09-19") |
                              date >= ymd("2023-04-30") & date < ymd("2023-10-24") ~ "Leaf",
                            TRUE ~ NA))

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
  # And add a column for the prior year's algal productivity
  group_by(watershed) %>%
  mutate(mean_chla_T_leaf_lag1 = lag(mean_chla_T_leaf)) %>%
  ungroup() %>%
  # note all resulting NANs as NAs
  mutate_all(~ifelse(is.nan(.), NA, .))

# Also, going to create a separate dataset that aggregates chl a
# leading up to only peak emergence in each watershed.

# Create a column of peak dates.
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

# Also going to create separate May, June, and combined chl a measures.
algal_monthly <- ws_trim %>%
  mutate(month = month(date)) %>%
  group_by(site, Year, month) %>%
  summarize(mean_chla_T_monthly = mean(chla_T, na.rm = TRUE)) %>%
  ungroup()

# and filter only for May/June months
algal_mj <- algal_monthly %>%
  filter(month %in% c(5,6)) %>%
  # and replace NaN values
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  # and pivot wider
  pivot_wider(names_from = month, values_from = mean_chla_T_monthly) %>%
  rename(mean_chla_T_may = `5`,
         mean_chla_T_june = `6`) %>%
  mutate(watershed = case_when(site == "W1" ~ "1",
                               site == "W2" ~ "2",
                               site == "W3" ~ "3",
                               site == "W4" ~ "4",
                               site == "W5" ~ "5",
                               site == "W6" ~ "6",
                               site == "W9" ~ "9",
                               site == "HBK" ~ "HBK")) %>%
  select(watershed, Year, mean_chla_T_may, mean_chla_T_june)

##### Season Durations #####

# Next, I'll be adding in the length of the preceding seasons, using the tidied dataset loaded above.

seasons4 <- season_data %>%
  filter(Season %in% c("Warming", "Leaf", 
                       "Cooling", "Snow")) %>%
  mutate(start_date = mdy(Start_Date),
         end_date = mdy(End_Date))

# and calculate the difference in dates
seasons4 <- seasons4 %>%
  mutate(duration = as.numeric(end_date - start_date))

# and trim down/pivot to columns of interest
seasons4_trim <- seasons4 %>%
  select(Site_ID, Year, Season, duration) %>%
  pivot_wider(names_from = Season, values_from = duration)

# also need re-format and calculate appropriately lagged seasons
seasons4_trim <- seasons4_trim %>%
  rename(Leaf_duration = Leaf,
         Cooling_duration = Cooling,
         Snow_duration = Snow,
         Warming_duration = Warming) %>%
  group_by(Site_ID) %>%
  # need to include:
  # same year leaf, warming
  # prior year snow, cooling, leaf
  mutate(Snow_duration_lag1 = lag(Snow_duration),
         Cooling_duration_lag1 = lag(Cooling_duration),
         Leaf_duration_lag1 = lag(Leaf_duration)) %>%
  ungroup()

# and finally trim it down and add proper WS column
seasons_trim <- seasons4_trim %>%
  mutate(watershed = case_when(Site_ID == "W1" ~ "1",
                               Site_ID == "W2" ~ "2",
                               Site_ID == "W3" ~ "3",
                               Site_ID == "W4" ~ "4",
                               Site_ID == "W5" ~ "5",
                               Site_ID == "W6" ~ "6",
                               Site_ID == "W9" ~ "9",
                               Site_ID == "HBK" ~ "HBK")) %>%
  select(watershed, Year, 
         # working backwards
         Leaf_duration, Warming_duration,
         Snow_duration_lag1, Cooling_duration_lag1,
         Leaf_duration_lag1)

##### Q & T metrics #####

# Properly format degree day dataset.
ddays_data <- ddays_data %>%
  mutate(watershed = case_when(Site_ID == "W1" ~ "1",
                               Site_ID == "W2" ~ "2",
                               Site_ID == "W3" ~ "3",
                               Site_ID == "W4" ~ "4",
                               Site_ID == "W5" ~ "5",
                               Site_ID == "W6" ~ "6",
                               Site_ID == "W9" ~ "9",
                               Site_ID == "HBK" ~ "HBK")) %>%
  select(watershed, Year, cum_degree_day_1,
         cum_degree_day_2, delta_temp)

# Keep only the metrics of interest for now.
qt_data_trim <- qt_data %>%
  mutate(watershed = case_when(Site_ID == "W1" ~ "1",
                               Site_ID == "W2" ~ "2",
                               Site_ID == "W3" ~ "3",
                               Site_ID == "W4" ~ "4",
                               Site_ID == "W5" ~ "5",
                               Site_ID == "W6" ~ "6",
                               Site_ID == "W9" ~ "9",
                               Site_ID == "HBK" ~ "HBK")) %>%
  select(watershed, Year, Season,
         mean_Q, medi_Q, max_Q, min_Q,
         std_Q, CV_Q, mean_RBI,
         mean_T, medi_T, max_T, min_T,
         std_T, CV_T, 
         Exceed_2y_perc, Exceed_10y_perc)

# Must pivot to accommodate annual-level site data.
qt_data_pivot <- qt_data_trim %>%
  pivot_wider(values_from = mean_Q:Exceed_10y_perc, 
              names_from = Season)

# Similar to seasonal durations considered above, we are
# most interested in (working backwards):
# same year leaf, warming
# prior year snow, cooling, leaf

# So, let's first create the lagged snow/cooling/leaf data
qt_data_pivot <- qt_data_pivot %>%
  group_by(watershed) %>%
  # previous snow season metrics first
  mutate(mean_Q_Snow_lag1 = lag(mean_Q_Snow),
         medi_Q_Snow_lag1 = lag(medi_Q_Snow),
         max_Q_Snow_lag1 = lag(max_Q_Snow),
         min_Q_Snow_lag1 = lag(min_Q_Snow),
         std_Q_Snow_lag1 = lag(std_Q_Snow),
         CV_Q_Snow_lag1 = lag(CV_Q_Snow),
         mean_RBI_Snow_lag1 = lag(mean_RBI_Snow),
         mean_T_Snow_lag1 = lag(mean_T_Snow),
         medi_T_Snow_lag1 = lag(medi_T_Snow),
         max_T_Snow_lag1 = lag(max_T_Snow),
         min_T_Snow_lag1 = lag(min_T_Snow),
         std_T_Snow_lag1 = lag(std_T_Snow),
         CV_T_Snow_lag1 = lag(CV_T_Snow),
         Exceed_2y_perc_Snow_lag1 = lag(Exceed_2y_perc_Snow),
         Exceed_10y_perc_Snow_lag1 = lag(Exceed_10y_perc_Snow),
         # then previous cooling season metrics
         mean_Q_Cooling_lag1 = lag(mean_Q_Cooling),
         medi_Q_Cooling_lag1 = lag(medi_Q_Cooling),
         max_Q_Cooling_lag1 = lag(max_Q_Cooling),
         min_Q_Cooling_lag1 = lag(min_Q_Cooling),
         std_Q_Cooling_lag1 = lag(std_Q_Cooling),
         CV_Q_Cooling_lag1 = lag(CV_Q_Cooling),
         mean_RBI_Cooling_lag1 = lag(mean_RBI_Cooling),
         mean_T_Cooling_lag1 = lag(mean_T_Cooling),
         medi_T_Cooling_lag1 = lag(medi_T_Cooling),
         max_T_Cooling_lag1 = lag(max_T_Cooling),
         min_T_Cooling_lag1 = lag(min_T_Cooling),
         std_T_Cooling_lag1 = lag(std_T_Cooling),
         CV_T_Cooling_lag1 = lag(CV_T_Cooling),
         Exceed_2y_perc_Cooling_lag1 = lag(Exceed_2y_perc_Cooling),
         Exceed_10y_perc_Cooling_lag1 = lag(Exceed_10y_perc_Cooling),
         # and finally previous leaf season metrics
         mean_Q_Leaf_lag1 = lag(mean_Q_Leaf),
         medi_Q_Leaf_lag1 = lag(medi_Q_Leaf),
         max_Q_Leaf_lag1 = lag(max_Q_Leaf),
         min_Q_Leaf_lag1 = lag(min_Q_Leaf),
         std_Q_Leaf_lag1 = lag(std_Q_Leaf),
         CV_Q_Leaf_lag1 = lag(CV_Q_Leaf),
         mean_RBI_Leaf_lag1 = lag(mean_RBI_Leaf),
         mean_T_Leaf_lag1 = lag(mean_T_Leaf),
         medi_T_Leaf_lag1 = lag(medi_T_Leaf),
         max_T_Leaf_lag1 = lag(max_T_Leaf),
         min_T_Leaf_lag1 = lag(min_T_Leaf),
         std_T_Leaf_lag1 = lag(std_T_Leaf),
         CV_T_Leaf_lag1 = lag(CV_T_Leaf),
         Exceed_2y_perc_Leaf_lag1 = lag(Exceed_2y_perc_Leaf),
         Exceed_10y_perc_Leaf_lag1 = lag(Exceed_10y_perc_Leaf))

# And once again trim down only to the columns of interest.
qt_data_pivot_trim <- qt_data_pivot %>%
  select(watershed, Year,
         # same year leaf
         mean_Q_Leaf, medi_Q_Leaf, max_Q_Leaf, 
         min_Q_Leaf, std_Q_Leaf, CV_Q_Leaf, 
         mean_RBI_Leaf, mean_T_Leaf, medi_T_Leaf,
         max_T_Leaf, min_T_Leaf, std_T_Leaf,
         CV_T_Leaf, Exceed_2y_perc_Leaf, Exceed_10y_perc_Leaf,
         # same year warming
         mean_Q_Warming, medi_Q_Warming, max_Q_Warming, 
         min_Q_Warming, std_Q_Warming, CV_Q_Warming, 
         mean_RBI_Warming, mean_T_Warming, medi_T_Warming,
         max_T_Warming, min_T_Warming, std_T_Warming,
         CV_T_Warming, Exceed_2y_perc_Warming, Exceed_10y_perc_Warming,
         # same year warming to peak (shifts in timing)
         `mean_Q_Warming to Peak`, `medi_Q_Warming to Peak`,
         `max_Q_Warming to Peak`, `min_Q_Warming to Peak`,
         `std_Q_Warming to Peak`, `CV_Q_Warming to Peak`, 
         `mean_RBI_Warming to Peak`, `mean_T_Warming to Peak`,
         `medi_T_Warming to Peak`, `max_T_Warming to Peak`,
         `min_T_Warming to Peak`, `std_T_Warming to Peak`,
         `CV_T_Warming to Peak`, `Exceed_2y_perc_Warming to Peak`,
         `Exceed_10y_perc_Warming to Peak`,
         # prior year snow
         mean_Q_Snow_lag1, medi_Q_Snow_lag1, max_Q_Snow_lag1, 
         min_Q_Snow_lag1, std_Q_Snow_lag1, CV_Q_Snow_lag1, 
         mean_RBI_Snow_lag1, mean_T_Snow_lag1, medi_T_Snow_lag1,
         max_T_Snow_lag1, min_T_Snow_lag1, std_T_Snow_lag1,
         CV_T_Snow_lag1, Exceed_2y_perc_Snow_lag1, Exceed_10y_perc_Snow_lag1,
         # prior year cooling
         mean_Q_Cooling_lag1, medi_Q_Cooling_lag1, max_Q_Cooling_lag1, 
         min_Q_Cooling_lag1, std_Q_Cooling_lag1, CV_Q_Cooling_lag1, 
         mean_RBI_Cooling_lag1, mean_T_Cooling_lag1, medi_T_Cooling_lag1,
         max_T_Cooling_lag1, min_T_Cooling_lag1, std_T_Cooling_lag1,
         CV_T_Cooling_lag1, Exceed_2y_perc_Cooling_lag1,
         Exceed_10y_perc_Cooling_lag1,
         # prior year leaf
         mean_Q_Leaf_lag1, medi_Q_Leaf_lag1, max_Q_Leaf_lag1, 
         min_Q_Leaf_lag1, std_Q_Leaf_lag1, CV_Q_Leaf_lag1, 
         mean_RBI_Leaf_lag1, mean_T_Leaf_lag1, medi_T_Leaf_lag1,
         max_T_Leaf_lag1, min_T_Leaf_lag1, std_T_Leaf_lag1,
         CV_T_Leaf_lag1, Exceed_2y_perc_Leaf_lag1, Exceed_10y_perc_Leaf_lag1)

#### Join ####

# Join with snowmelt data.
all_var <- left_join(resp_var, melt_trim)

# Join with algal data.
all_var <- left_join(all_var, algal_trim)
all_var <- left_join(all_var, algal_pre_peak)
all_var <- left_join(all_var, algal_mj)

# Join with seasonal durations.
all_var <- left_join(all_var, seasons_trim)

# Join with Q and T 
all_var <- left_join(all_var, ddays_data)
all_var <- left_join(all_var, qt_data_pivot_trim)

# Export for future use.
# saveRDS(all_var, "data_working/resp_and_cov_070224.rds")

#### Visualize ####

# Note, these have been revised following the data QAQC above.
all_var <- readRDS("data_working/resp_and_cov_070224.rds")
# Also load in watershed sizes for normalization
size <- read_csv("data_raw/ws_sizes.csv")

# Join size with full dataset.
all_var <- full_join(all_var, size, by = c("watershed" = "Watershed"))

# Examine some of the correlation plots.
plot(all_var$annual_count_lag1, all_var$annual_count) # no trend
plot(all_var$annual_count_cf_lag1, all_var$annual_count_cf) # positive
plot(all_var$annual_count_sf_lag1, all_var$annual_count_sf) # positive
plot(all_var$melt_duration_days, all_var$annual_count) # weakly negative
plot(all_var$swe_max_mm, all_var$annual_count) # weakly positive
plot(all_var$melt_duration_days, all_var$leaf_peak_count) # no trend
plot(all_var$melt_duration_days, all_var$leaf_peak_jday) # weakly positive
plot(all_var$mean_chla_T_leaf, all_var$annual_count) # negative
plot(all_var$mean_chla_T_leaf_lag1, all_var$annual_count) # negative
plot(all_var$delta_temp, all_var$leaf_peak_jday) # no trend

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
#        filename = "figures/annual_covar_070224.jpg",
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

(fig3_peak <- ggplot(all_var, aes(x = annual_count_cf,
                                  y = leaf_peak_cf_count)) +
    geom_point(color = "#51A8B0", size = 3) +
    labs(x = "Annual Caddisfly Count (individuals)",
         y = "Caddisfly Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig4_peak <- ggplot(all_var, aes(x = annual_count_sf,
                                  y = leaf_peak_sf_count)) +
    geom_point(color = "#51A8B0", size = 3) +
    labs(x = "Annual Stonefly Count (individuals)",
         y = "Stonefly Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig_peak_all <- fig1_peak + fig2_peak + fig3_peak + fig4_peak)
 
# ggsave(plot = fig_peak_all,
#        filename = "figures/peak_alltaxa_070224.jpg",
#        width = 20,
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

(fig3_peak_time <- ggplot(all_var, aes(x = leaf_peak_cf_count,
                                       y = leaf_peak_cf_jday)) +
    geom_point(color = "#5CA7F8", size = 3) +
    labs(x = "Caddisfly Peak Emergence Magnitude (individuals)",
         y = "Caddisfly Peak Emergence DOY") +
    theme_bw())

(fig4_peak_time <- ggplot(all_var, aes(x = leaf_peak_sf_count,
                                       y = leaf_peak_sf_jday)) +
    geom_point(color = "#5CA7F8", size = 3) +
    labs(x = "Stonefly Peak Emergence Magnitude (individuals)",
         y = "Stonefly Peak Emergence DOY") +
    theme_bw())

(fig_peaktime_all <- fig1_peak_time + fig2_peak_time + 
    fig3_peak_time + fig4_peak_time)

# ggsave(plot = fig_peaktime_all,
#        filename = "figures/peaktime_alltaxa_070224.jpg",
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
#        filename = "figures/latepeak_covar_070224.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

##### Q Variability #####

(fig1_q_count <- ggplot(all_var, aes(x = mean_RBI_Leaf,
                               y = annual_count)) +
   geom_point(color = "#85ADCC", size = 3) +
   labs(x = "RBI - Leaf Season",
        y = "Annual Count (Individuals)") +
   theme_bw())

(fig2_q_count <- ggplot(all_var, aes(x = mean_RBI_Warming,
                               y = annual_count)) +
    geom_point(color = "#85ADCC", size = 3) +
    labs(x = "RBI - Warming Season",
         y = "Annual Count (Individuals)") +
    theme_bw())

(fig3_q_count <- ggplot(all_var, aes(x = mean_RBI_Snow_lag1,
                               y = annual_count)) +
    geom_point(color = "#85ADCC", size = 3) +
    labs(x = "RBI - Previous Snow Season",
         y = "Annual Count (Individuals)") +
    theme_bw())

(fig4_q_count <- ggplot(all_var, aes(x = mean_RBI_Cooling_lag1,
                               y = annual_count)) +
    geom_point(color = "#85ADCC", size = 3) +
    labs(x = "RBI - Previous Cooling Season",
         y = "Annual Count (Individuals)") +
    theme_bw())

(fig_q_count <- fig1_q_count +
    fig2_q_count +
    fig3_q_count +
    fig4_q_count)

# ggsave(plot = fig_q_count,
#        filename = "figures/cumulative_q_070224.jpg",
#        width = 15,
#        height = 15,
#        units = "cm")

(fig1_q_peak <- ggplot(all_var, aes(x = mean_RBI_Warming,
                                  y = leaf_peak_count)) +
    geom_point(color = "#5E6B7B", size = 3) +
    labs(x = "RBI - Warming Season",
         y = "Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig2_q_peak <- ggplot(all_var, aes(x = `mean_RBI_Warming to Peak`,
                                    y = leaf_peak_count)) +
    geom_point(color = "#5E6B7B", size = 3) +
    labs(x = "RBI - Warming Season until Peak",
         y = "Peak Emergence Magnitude (individuals)") +
    theme_bw())

(fig_q_peak <- fig1_q_peak +
    fig2_q_peak)

# ggsave(plot = fig_q_peak,
#        filename = "figures/peak_q_070224.jpg",
#        width = 15,
#        height = 8,
#        units = "cm")

(fig1_q_peak_time <- ggplot(all_var, aes(x = mean_RBI_Warming,
                                       y = leaf_peak_jday)) +
    geom_point(color = "#233D3F", size = 3) +
    labs(x = "RBI - Warming Season",
         y = "Peak Emergence DOY") +
    theme_bw())

(fig2_q_peak_time <- ggplot(all_var, aes(x = mean_RBI_Leaf,
                                         y = later_peak_jday)) +
    geom_point(color = "#233D3F", size = 3) +
    labs(x = "RBI - Leaf Season",
         y = "Later Peak Emergence DOY") +
    theme_bw())

(fig_q_peak_time <- fig1_q_peak_time +
    fig2_q_peak_time)

# ggsave(plot = fig_q_peak_time,
#        filename = "figures/peak_time_q_070224.jpg",
#        width = 15,
#        height = 8,
#        units = "cm")

##### Degree Days #####

(fig1_deg_peak <- ggplot(all_var, aes(x = cum_degree_day_1,
                                       y = leaf_peak_jday)) +
   geom_point(color = "#D95204", size = 3) +
   labs(x = "Cumulative Degree Days (Average)",
        y = "Peak Emergence DOY") +
   theme_bw())

(fig2_deg_peak <- ggplot(all_var, aes(x = cum_degree_day_2,
                                       y = leaf_peak_jday)) +
    geom_point(color = "#D95204", size = 3) +
    labs(x = "Cumulative Degree Days (Max/Min)",
         y = "Peak Emergence DOY") +
    theme_bw())

(fig_degpeak_all <- fig1_deg_peak + fig2_deg_peak)

# ggsave(plot = fig_degpeak_all,
#        filename = "figures/peaktime_degdays_070224.jpg",
#        width = 15,
#        height = 8,
#        units = "cm")

##### HB Presentation Figures #####

# Choosing a few select relationships to plot ahead of the HB LTER Meeting.

(fig1_present <- ggplot(all_var, aes(x = leaf_peak_count,
                                     y = annual_count,
                                     fill = factor(watershed))) +
   geom_point(size = 4, shape = 21) +
   scale_fill_viridis(discrete = TRUE, option = "magma") +
   labs(x = "Peak Emergence Magnitude (Individuals)",
        y = "Cumulative Annual Emergence (Individuals)",
        fill = "Watershed") +
   theme_bw())

(fig2_present <- ggplot(all_var, aes(x = mean_Q_Warming,
                                     y = annual_count,
                                     fill = factor(watershed))) +
    geom_point(size = 4, shape = 21) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    labs(x = "Warming Season Mean Discharge (cfs)",
         y = "Cumulative Annual Total Emergence (Individuals)",
         fill = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

(fig3_present <- ggplot(all_var, aes(x = mean_Q_Warming,
                                     y = annual_count_cf,
                                     fill = factor(watershed))) +
    geom_point(size = 4, shape = 21) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    labs(x = "Warming Season Mean Discharge (cfs)",
         y = "Cumulative Annual Caddisfly Emergence (Individuals)",
         fill = "Watershed") +
    theme_bw())

(fig3a_present <- ggplot(all_var, aes(x = mean_Q_Warming,
                                     y = annual_count_cf/Area_ha,
                                     fill = factor(watershed))) +
    geom_point(size = 4, shape = 21) +
    scale_x_log10() +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    labs(x = "Warming Season Mean Discharge (cfs) / Watershed Size (ha)",
         y = "Cumulative Annual Caddisfly Emergence (Individuals)",
         fill = "Watershed") +
    theme_bw())

(fig3b_present <- ggplot(all_var, aes(x = mean_Q_Warming,
                                      y = annual_count_sf/Area_ha,
                                      fill = factor(watershed))) +
    geom_point(size = 4, shape = 21) +
    scale_x_log10() +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    labs(x = "Warming Season Mean Discharge (cfs) / Watershed Size (ha)",
         y = "Cumulative Annual Stonefly Emergence (Individuals)",
         fill = "Watershed") +
    theme_bw())

fig3a_present + fig3b_present

(fig4_present <- ggplot(all_var, aes(x = mean_Q_Cooling_lag1,
                                     y = annual_count,
                                     fill = factor(watershed))) +
    geom_point(size = 4, shape = 21) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    labs(x = "Cooling Season Mean Discharge (cfs)",
         y = "Cumulative Annual Total Emergence (Individuals)",
         fill = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

(fig5_present <- ggplot(all_var, aes(x = mean_Q_Cooling_lag1,
                                     y = annual_count_cf,
                                     fill = factor(watershed))) +
    geom_point(size = 4, shape = 21) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    labs(x = "Cooling Season Mean Discharge (cfs)",
         y = "Cumulative Annual Caddisfly Emergence (Individuals)",
         fill = "Watershed") +
    theme_bw())

(fig_q <- fig2_present + fig3_present)

# ggsave(plot = fig_q,
#        filename = "figures/warming_q_total_cf_070224.jpg",
#        width = 28,
#        height = 11,
#        units = "cm")

# Greater minimum leaf season (summer) temperatures are correlated with later fall peak emergence (of all taxa).**

(fig_t_present <- ggplot(all_var, aes(x = min_T_Leaf,
                                      y = later_peak_jday,
                                      fill = factor(watershed))) +
    geom_point(size = 4, shape = 21) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    labs(x = "Leaf Season Minimum Temperature (Celsius)",
         y = "Peak Fall Emergence Date (Day of year)",
         fill = "Watershed") +
    theme_bw())

# ggsave(plot = fig_t_present,
#        filename = "figures/leaf_t_later_peak_070224.jpg",
#        width = 14,
#        height = 11,
#        units = "cm")

(fig_t2_present <- ggplot(all_var, aes(x = CV_T_Cooling_lag1,
                                      y = later_peak_jday,
                                      fill = factor(watershed))) +
    geom_point(size = 4, shape = 21) +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    labs(x = "Prior Cooling Season C.V. Temperature (Celsius)",
         y = "Peak Fall Emergence Date (Day of year)",
         fill = "Watershed") +
    theme_bw())

# ggsave(plot = fig_t2_present,
#        filename = "figures/leaf_t2_later_peak_070224.jpg",
#        width = 14,
#        height = 11,
#        units = "cm")

#### Correlations ####

# And finally generate summary correlation plots.

# Select only variables of interest.
all_var_trim <- all_var %>%
  select(annual_count:Exceed_10y_perc_Leaf_lag1) %>%
  # and replace NaN values
  mutate_all(~ifelse(is.nan(.), NA, .))

# Calculate correlations.
corr_var <- cor(all_var_trim, 
                method = "pearson", 
                # compute correlation using all complete
                # pairs of obs on two variables in question
                use = "pairwise.complete.obs")

ggcorrplot(corr_var,
           type = "lower",
           lab = TRUE) # eek! way too big to see

# Exporting for safekeeping.
saveRDS(corr_var, "data_working/correlations_070224.rds")

# Trimming things down to variables of interest
corr_ed <- corr_var

# see code here: https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57
# assign NAs along the diagonal for bottom half     
corr_ed[lower.tri(corr_ed, diag=TRUE)] <- NA 

# assign perfect correlations to be NA (unrealistic)
corr_ed[corr_ed == 1] <- NA 

# turn into a 3-column table, with each compared
# variable in two different columns, and their
# corresponding correlation value in a third
corr_ed <- as.data.frame(as.table(corr_ed))

# remove the NA values from new table
corr_ed <- na.omit(corr_ed) 

# filter for values above 0.6 or below -0.6
corr_ed <- subset(corr_ed, abs(Freq) >= 0.6) 

# and filter for comparisons only to response variables
corr_ed <- corr_ed %>%
  filter(Var1 %in% c("annual_count", "annual_count_sf", "annual_count_cf",
                     "leaf_peak_count", "leaf_peak_jday", "leaf_peak_sf_count",
                     "leaf_peak_sf_jday", "leaf_peak_cf_count", "leaf_peak_cf_jday",
                     "leaf_peak_3wk_count", "later_peak_count", "later_peak_jday"))

# sort by highest correlation
corr_ed <- corr_ed[order(-abs(corr_ed$Freq)),] 

# turn corr back into matrix in order to plot with corrplot
mtx_corr_ed <- reshape2::acast(corr_ed, Var1~Var2, value.var="Freq")

# plot correlations visually
fig_corr <- ggcorrplot(mtx_corr_ed,
                       lab = TRUE) #woot!! MUCH betters

# export for safekeeping.
# ggsave(plot = fig_corr,
#        filename = "figures/sig_corr_070224.jpg",
#        width = 20,
#        height = 40,
#        units = "cm")

# included stonefly and caddisfly indices in the above to
# avoid duplicate analyses on same data

# End of script.
