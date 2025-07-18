### Annual Climate and Stream Data Summary
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will aggregate existing records to
# describe annual statistics regarding climate and streamflow.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)
library(lubridate)

# Load data.
q_dat <- read_csv("data_raw/HBEF_DailyStreamflow_1956-2024.csv") # Streamflow is in mm/day.
ppt_dat <- read_csv("data_raw/dailyWatershedPrecip1956-2025.csv") # Precipitation is in mm.

#### Tidy ####

##### Q metrics #####

# Calculate Q percentiles for the full record.
q_perc <- q_dat %>%
  group_by(WS) %>%
  summarize(perc10 = quantile(Streamflow,
                              probs = 0.1,
                              na.rm = TRUE),
            perc90 = quantile(Streamflow,
                              probs = 0.9,
                              na.rm = TRUE)) %>%
  ungroup()

# And use to assign to dataset of interest.
q_metrics <- q_dat %>%
  left_join(q_perc) %>%
  mutate(month = month(DATE),
         year = year(DATE),
         water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year),
         low_flow_day = case_when(Streamflow <= perc10 ~ 1,
                                  TRUE ~ 0),
         high_flow_day = case_when(Streamflow >= perc90 ~ 1,
                                   TRUE ~ 0)) %>%
  group_by(WS, water_year) %>%
  summarize(low_flow_days = sum(low_flow_day),
            high_flow_days = sum(high_flow_day),
            mean_q = mean(Streamflow, na.rm = TRUE),
            cv_q = sd(Streamflow, na.rm = TRUE)/
              mean(Streamflow, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(low_flow_perc = percent_rank(low_flow_days),
         high_flow_perc = percent_rank(high_flow_days),
         cv_q_perc = percent_rank(cv_q))

# Export dataset.
# saveRDS(q_dat_low_high_flow_days,
#         "data_working/low_high_flow_days_cvQ.rds")

# And trim down to sites and years of interest.
q_metrics_trim <- q_metrics %>%
  filter(WS %in% c(5,6)) %>%
  filter(water_year %in% c(2017, 2018, 2019,
                           2020, 2021, 2022,
                           2023, 2024))

##### Precip Metrics #####

# Create similarly aggregated precipitation dataset.
ppt_metrics <- ppt_dat %>%
  mutate(month = month(DATE),
         year = year(DATE),
         water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
  group_by(watershed, water_year) %>%
  summarize(sum_ppt = sum(Precip, na.rm = TRUE)) %>%
  ungroup()

# And trim down to sites and years of interest.
ppt_metrics_trim <- ppt_metrics %>%
  filter(watershed %in% c("W5", "W6")) %>%
  filter(water_year %in% c(2017, 2018, 2019,
                           2020, 2021, 2022,
                           2023, 2024))

# End of script.

