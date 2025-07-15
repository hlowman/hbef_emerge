### Discharge Analysis (revised)
### July 1, 2025
### Heili Lowman

#### README ####

# The following script will estimate discharge metrics
# to assist with explanation of annual black fly emergence magnitude.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load discharge data.
q_dat <- read_csv("data_raw/HBEF_DailyStreamflow_1956-2024.csv")
# Streamflow is in mm/day.

#### Q metrics ####

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
q_dat_low_high_flow_days <- q_dat %>%
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
            high_flow_days = sum(high_flow_day)) %>%
  ungroup() %>%
  mutate(low_flow_perc = percent_rank(low_flow_days),
         high_flow_perc = percent_rank(high_flow_days))

# Export dataset.
# saveRDS(q_dat_low_high_flow_days,
#         "data_working/low_high_flow_days.rds")

# End of script.