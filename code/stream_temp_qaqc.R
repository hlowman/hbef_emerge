### Stream Temperature Data
### June 3, 2025
### Heili Lowman

#### README ####

# Unlike the previous temperature script, this script will combine temperature 
# data collected by Bill McDowell & Jody Potter (at W9) and Nina Lany & the U.S.
# Forest Service for calculating degree days.

# I will be using functions developed by Danielle Hare to examine water-air
# temperature relationships to remove bad data.

# Finally, I will fill in missing days to help calculate degree days in the 
# next script in this workflow.

#### TO-DOs ####

# (1) Add data from W6 weird pond for 2023/2024.

# (2) Apply correction for W3 from W4 for 2020.

# (3) Check about time zones from instruments for both parties.
# Note - I have plotted things out, and they look alright timing-wise,
# but need to be sure.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(targets)
library(magrittr)
library(data.table)

# Load necessary functions.
source("code/PASTA_dielAnalysisFunctions.R")

# Load stream temperature data.
temp_dat_w16 <- read_csv("data_raw/HBEF_streamtemp_roughlyCleaned.csv")
temp_dat_w9 <- read_csv("data_raw/HBEF_W9_UNHWQAL_WaterTemp.csv")

# Load air temperature data.
air_temp_dat <- lapply(list.files(path = "data_raw/airT_15", 
                                  pattern = "*.csv", 
                                  full.names = TRUE), read.csv) %>%
  do.call("rbind", .)

#### Data Prep ####

# The following steps will trim only to watersheds of interest,
# and data post-2018.

# Tidy USFS data.
temp_dat_w16_wide <- temp_dat_w16 %>%
  pivot_longer(cols = starts_with("Stream"),
               names_pattern = "Streamtemp_(.*)",
               names_to = c("watershed"),
               values_to = "tempC_sw")%>%
  mutate(aspect = ifelse(watershed%in%c("W7", "W8", "W9"),"North", "South"),
         DateTime = mdy_hm(TIMESTAMP, tz = "UTC"))%>%
  drop_na(tempC_sw) %>%
  filter(watershed %in% c("W1", "W2", "W3",
                          "W4", "W5", "W6")) %>%
  mutate(DateTime_r = DateTime,
         Y = year(DateTime)) %>%
  filter(Y > 2017) %>%
  select(-Y)
# Data already at sub-15 minute intervals.

# Reformat UNH data.
temp_dat_w9_wide <- temp_dat_w9 %>%
  select(-Site) %>%
  mutate(watershed = "W9",
         aspect = "North") %>%
  rename(TIMESTAMP = Date,
         tempC_sw = TempC) %>%
  mutate(DateTime = mdy_hm(TIMESTAMP, tz = "UTC"),
         Y = year(DateTime)) %>%
  filter(Y > 2017) %>%
  select(-Y) %>%
  # need to round to nearest 15 min to match air temp data
  mutate(DateTime_r = round_date(DateTime, "15 min"))

# Assign weather stations to ws aspect.
SouthFacing_RG <- c("STA_1", "STA_6")
NorthFacing_RG <- c("STA14", "STA17", "STA23")

# Summarize air temperature measurements across sites to match
# with watersheds.
air_temp_summ <- air_temp_dat %>%
  filter(STA != "STA_HQ") %>% 
  mutate(aspect = ifelse(STA %in% SouthFacing_RG, "South", "North")) %>%
  group_by(aspect, DateTime) %>%
  summarize(airTemp = mean(airTemp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(DateTime_r = ymd_hms(DateTime, tz = "UTC"))

# Trim and join water and air temperature datasets.
temp_dat_w16_trim <- temp_dat_w16_wide %>%
  select(DateTime_r, aspect, watershed, tempC_sw)

temp_dat_w9_trim <- temp_dat_w9_wide %>%
  select(DateTime_r, aspect, watershed, tempC_sw)

temp_dat_sw <- rbind(temp_dat_w16_trim,
                      temp_dat_w9_trim)

air_temp_summ_trim <- air_temp_summ %>%
  select(DateTime_r, aspect, airTemp)
  
temp_air_sw <- left_join(temp_dat_sw, air_temp_summ, by = c("aspect", "DateTime_r")) %>%
  # and drop rows where air or stream temp is unavailable
  drop_na(airTemp, tempC_sw)

# Trim full dataset for processing with sinusoidal function.
temp_dat_prepped <- temp_air_sw %>%
  select(watershed, DateTime_r, tempC_sw, airTemp)

#### Diel Fit ####

# Fit sinusoidal models to air and water temperatures.
temp_fit <- TMd_output(temp_dat_prepped)

# Calculate amplitude ratios and phase lags.
fit_metrics <- temp_fit %>%
  group_by(site_id, date) %>% 
  # last is water first is air 
  summarize(AmpRatio = round(last(amplitude_C)/first(amplitude_C),2),
            PhaseLag_h = round(last(phase_h) - first(phase_h),2),
            Ratio_Mean = round(last(YInt) / first(YInt),2),
            site_id = first(site_id))

# Plot results
ggplot(fit_metrics, aes(x = date, y = AmpRatio)) +
  geom_point() +
  facet_wrap(.~site_id) +
  theme_bw() # ranges from 0-8 (some outliers)

ggplot(fit_metrics, aes(x = date, y = PhaseLag_h)) +
  geom_point() +
  facet_wrap(.~site_id) +
  theme_bw() # ranges from -10 to 10 (fairly even)

ggplot(fit_metrics, aes(x = date, y = Ratio_Mean)) +
  geom_point() +
  facet_wrap(.~site_id) +
  theme_bw() # ranges from -100 to 400 (very few outliers)

#### Final Temp Data ####

# Filter temperature data based on fit above.
good_site_dates <- fit_metrics %>%
  filter(AmpRatio < 2.5) %>% # removes 16 days
  filter(Ratio_Mean < 100 & Ratio_Mean > -100) %>% # removes 10 days
  rename(watershed = site_id) %>%
  mutate(date = as_date(date)) %>%
  select(watershed, date)

# Going back to stream dataset, with sub-15 minute data in some cases.
# Need to first bring all data back in (not time-rounded).
temp_dat_w16_trim2 <- temp_dat_w16_trim %>%
  rename(DateTime = DateTime_r)

temp_dat_w9_trim2 <- temp_dat_w9_wide %>%
  select(DateTime, aspect, watershed, tempC_sw)

temp_dat_sw2 <- rbind(temp_dat_w16_trim2,
                     temp_dat_w9_trim2)

# Need to also add a "date" column to join by.
temp_dat_sw2 <- temp_dat_sw2 %>%
  mutate(date = as_date(DateTime))

# Join to filter the available stream temperature data
# to the sites that pass the fit metrics above.
temp_dat_sw_good <- left_join(good_site_dates, temp_dat_sw2,
                              by = c("watershed", "date"))

# Average by day.
temp_sw_daily_avg <- temp_dat_sw_good %>%
  group_by(watershed, date) %>%
  summarize(mean_tempC_sw = mean(tempC_sw, na.rm = TRUE)) %>%
  ungroup()

# But also realizing that I need to make the gaps exist,
# so adding full date list to all sites.
trap_dates <- seq(as.Date("2018-01-01"), as.Date("2023-12-31"), 
                  by = "day")

all_dates <- rep(trap_dates, 7)

all_sites <- c(rep("W1", 2191), 
               rep("W2", 2191),
               rep("W3", 2191), 
               rep("W4", 2191),
               rep("W5", 2191), 
               rep("W6", 2191),
               rep("W9", 2191))

main_frame <- data.frame(all_sites, all_dates) %>%
  rename(watershed = all_sites,
         date = all_dates)

# Filter down to site/years that are of interest given
# the insect record and *available* per USFS data having
# joined with the full list so gaps persist.
temp_sw_daily_w_gaps <- left_join(main_frame, temp_sw_daily_avg) %>%
  filter(watershed == "W1" & date > "2019-12-31" & date < "2022-01-01" |
           watershed == "W2" & date > "2017-12-31" & date < "2021-01-01" |
           watershed == "W3" & date > "2017-12-31" & date < "2021-01-01" |
           watershed == "W4" & date > "2017-12-31" & date < "2020-01-01" |
           watershed == "W5" & date > "2017-12-31" & date < "2022-08-13" |
           watershed == "W6" & date > "2017-12-31" & date < "2022-08-13" |
           watershed == "W9" & date > "2019-12-31" & date < "2021-01-01")

# Quick gut check plot to see coverage.
ggplot(temp_sw_daily_w_gaps, aes(x = date, y = mean_tempC_sw)) +
  geom_line() +
  theme_bw() +
  facet_grid(watershed~.) #  21 site-years total

# W1 is missing data in spring 2019, so I've removed that year
# W5 & W6 missing data in late 2022, but that's ok for our purposes.
# W9 is missing data in spring 2019, so I've removed that year

# Since most missingness appears to occur outside of the spring warming
# window I'll be considering, I'm going to export this as is.
saveRDS(temp_sw_daily_w_gaps, "data_working/stream_temps_w1234569.rds")

# End of script.
