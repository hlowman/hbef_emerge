### Temperature Data QAQC
### July 1, 2024
### Heili Lowman

#### README ####

# The following script will tidy the temperature data collected
# by Bill McDowell & Jody Potter for use as covariate/degree day data.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load data.
temp_dat_w3 <- read_csv("data_raw/HBEF_W9_UNHWQAL_WaterTemp.csv")
temp_dat_w9 <- read_csv("data_raw/HBEF_W3_UNHWQAL_WaterTemp.csv")

# And load seasonal dates.
seas_dates <- read_csv("data_working/season_dates_pheno_wpeak_052824_trimmed.csv")

#### QAQC ####

# First, examine both datasets.
# Format the date columns appropriately to do so.
temp_dat_w3 <- temp_dat_w3 %>%
  mutate(date = mdy_hm(Date))

ggplot(temp_dat_w3, aes(x = date, y = TempC)) +
  geom_point() +
  theme_bw()

# Appear to be missing some spring data in 2019, otherwise goes from
# July 2018 through October 2021. But no major outliers, so going to
# wait on the QAQC until I've had a chance to speak with Danielle and
# copy her protocol.

temp_dat_w9 <- temp_dat_w9 %>%
  mutate(date = mdy_hm(Date))

ggplot(temp_dat_w9, aes(x = date, y = TempC)) +
  geom_point() +
  theme_bw()

# Appear to have a complete record for the time period I'm interested
# in, and it spans (overall) May 2013 through November 2021.

#### Aggregate ####

# Using the seasonal dates, will aggregate both datasets.
# First, for W3:
seas_dates_w3 <- seas_dates %>%
  filter(Site_ID == "W3")

# And need to trim down to seasons/years for which data is complete.
seas_dates_w3 <- seas_dates_w3[11:17,]

# Now, to add columns for aggregation to the original dataset.
temp_dat_w3 <- temp_dat_w3 %>%
  mutate(Year = year(date)) %>%
  mutate(season_yr = case_when(date >= ymd("2019-05-06") &
                              date < ymd("2019-11-04") ~ "Leaf19",
                            date >= ymd("2019-11-04") &
                              date < ymd("2019-12-03") ~ "Cooling19",
                            date >= ymd("2019-12-03") &
                              date < ymd("2020-03-03") ~ "Snow19",
                            date >= ymd("2020-03-03") &
                              date < ymd("2020-05-17") ~ "Warming20",
                            date >= ymd("2020-05-17") &
                              date < ymd("2020-11-16") ~ "Leaf20",
                            date >= ymd("2020-11-16") &
                              date < ymd("2020-12-07") ~ "Cooling20",
                            TRUE ~ NA))

# And aggregate by those columns.
temp_agg_w3 <- temp_dat_w3 %>%
  group_by(Year, season_yr) %>%
  summarize(mean_T = mean(TempC, na.rm = TRUE),
            medi_T = median(TempC, na.rm = TRUE),
            min_T = min(TempC, na.rm = TRUE),
            max_T = max(TempC, na.rm = TRUE),
            std_T = sd(TempC, na.rm = TRUE),
            CV_T = (sd(TempC, na.rm = TRUE))/(mean(TempC, na.rm = TRUE))) %>%
  ungroup()

# also need to add the single warming to peak season.
temp_dat_w3_wtp <- temp_dat_w3 %>%
  mutate(wtp = case_when(date >= ymd("2020-03-03") &
                           date < ymd("2020-05-26") ~ "WarmingToPeak20",
                         TRUE ~ NA))

temp_agg_wtp_w3 <- temp_dat_w3_wtp %>%
  group_by(wtp) %>%
  summarize(mean_T = mean(TempC, na.rm = TRUE),
            medi_T = median(TempC, na.rm = TRUE),
            min_T = min(TempC, na.rm = TRUE),
            max_T = max(TempC, na.rm = TRUE),
            std_T = sd(TempC, na.rm = TRUE),
            CV_T = (sd(TempC, na.rm = TRUE))/(mean(TempC, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(Year = 2020) %>%
  rename(season_yr = wtp) %>%
  select(Year, season_yr, mean_T, medi_T,
         min_T, max_T, std_T, CV_T)

# Join the two and drop the NAs.
temp_agg_w3 <- rbind(temp_agg_w3, temp_agg_wtp_w3) %>%
  drop_na(season_yr)

# And now for W9:
seas_dates_w9 <- seas_dates %>%
  filter(Site_ID == "W9")

# Now, to add columns for aggregation to the original dataset.
temp_dat_w9 <- temp_dat_w9 %>%
  mutate(Year = year(date)) %>%
  mutate(season_yr = case_when(date >= ymd("2017-05-04") &
                                 date < ymd("2017-10-11") ~ "Leaf17",
                               date >= ymd("2017-10-11") &
                                 date < ymd("2017-12-11") ~ "Cooling17",
                               date >= ymd("2017-12-11") &
                                 date < ymd("2018-03-03") ~ "Snow17",
                               date >= ymd("2018-03-03") &
                                 date < ymd("2018-04-26") ~ "Warming18",
                               date >= ymd("2018-04-26") &
                                 date < ymd("2018-10-25") ~ "Leaf18",
                               date >= ymd("2018-10-25") &
                                 date < ymd("2018-11-19") ~ "Cooling18",
                               date >= ymd("2018-11-19") &
                                 date < ymd("2019-02-10") ~ "Snow18",
                               date >= ymd("2019-02-10") &
                                 date < ymd("2019-05-06") ~ "Warming19",
                               date >= ymd("2019-05-06") &
                                 date < ymd("2019-11-04") ~ "Leaf19",
                               date >= ymd("2019-11-04") &
                                 date < ymd("2019-12-03") ~ "Cooling19",
                               date >= ymd("2019-12-03") &
                                 date < ymd("2020-03-08") ~ "Snow19",
                               date >= ymd("2020-03-08") &
                                 date < ymd("2020-05-17") ~ "Warming20",
                               date >= ymd("2020-05-17") &
                                 date < ymd("2020-11-16") ~ "Leaf20",
                               date >= ymd("2020-11-16") &
                                 date < ymd("2020-12-07") ~ "Cooling20",
                               TRUE ~ NA))

# And aggregate by those columns.
temp_agg_w9 <- temp_dat_w9 %>%
  group_by(Year, season_yr) %>%
  summarize(mean_T = mean(TempC, na.rm = TRUE),
            medi_T = median(TempC, na.rm = TRUE),
            min_T = min(TempC, na.rm = TRUE),
            max_T = max(TempC, na.rm = TRUE),
            std_T = sd(TempC, na.rm = TRUE),
            CV_T = (sd(TempC, na.rm = TRUE))/(mean(TempC, na.rm = TRUE))) %>%
  ungroup()

# also need to add the warming to peak seasons.
temp_dat_w9_wtp <- temp_dat_w9 %>%
  mutate(wtp = case_when(date >= ymd("2018-03-03") &
                           date < ymd("2018-06-18") ~ "WarmingToPeak18",
                         date >= ymd("2019-02-10") &
                           date < ymd("2019-06-24") ~ "WarmingToPeak19",
                         date >= ymd("2020-03-08") &
                           date < ymd("2020-06-22") ~ "WarmingToPeak20",
                         TRUE ~ NA))

temp_agg_wtp_w9 <- temp_dat_w9_wtp %>%
  group_by(wtp) %>%
  summarize(mean_T = mean(TempC, na.rm = TRUE),
            medi_T = median(TempC, na.rm = TRUE),
            min_T = min(TempC, na.rm = TRUE),
            max_T = max(TempC, na.rm = TRUE),
            std_T = sd(TempC, na.rm = TRUE),
            CV_T = (sd(TempC, na.rm = TRUE))/(mean(TempC, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(Year = 2020) %>%
  rename(season_yr = wtp) %>%
  select(Year, season_yr, mean_T, medi_T,
         min_T, max_T, std_T, CV_T)

# Join the two and drop the NAs.
temp_agg_w9 <- rbind(temp_agg_w9, temp_agg_wtp_w9) %>%
  drop_na(season_yr)

#### Export ####

# Add watershed identifier columns to both datasets.
temp_agg_w3$Site_ID <- "W3"
temp_agg_w9$Site_ID <- "W9"

# Combine the two datasets.
temp_agg_both <- rbind(temp_agg_w3, temp_agg_w9)

# And tidy the season column.
temp_agg_both <- temp_agg_both %>%
  mutate(Season = case_when(season_yr %in% c("Leaf17", "Leaf18", "Leaf19", "Leaf20") ~ "Leaf",
                            season_yr %in% c("Cooling17", "Cooling18", "Cooling19", "Cooling20") ~ "Cooling",
                            season_yr %in% c("Snow17", "Snow18", "Snow19", "Snow20") ~ "Snow",
                            season_yr %in% c("Warming17", "Warming18", "Warming19", "Warming20") ~ "Warming",
                            season_yr %in% c("WarmingToPeak17", "WarmingToPeak18", 
                                             "WarmingToPeak19", "WarmingToPeak20") ~ "Warming to Peak",
                            TRUE ~ NA)) %>%
  select(Site_ID, Year, Season, mean_T, medi_T, min_T, max_T, std_T, CV_T)

# Export.
saveRDS(temp_agg_both, "data_working/Temp_HBEF_SummaryStats_070124.rds")

# End of script.
