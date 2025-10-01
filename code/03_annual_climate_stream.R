### Annual Climate, Chemistry, and Stream Data Summary
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will aggregate existing records to
# describe statistics regarding climate, chemistry, and streamflow.

# It will also generate an aggregated dataset for stream climate.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)
library(lubridate)

# Load data.
q_dat <- read_csv("data_raw/HBEF_DailyStreamflow_1956-2024.csv") # Streamflow is in mm/day.
ppt_dat <- read_csv("data_raw/dailyWatershedPrecip1956-2025.csv") # Precipitation is in mm.
chem_dat <- read_csv("data_raw/HBEFdata_Current_2025-07-18.csv") # Weekly chemistry data starting in 2013.

#### Tidy ####

##### Q metrics #####

# Calculate Q percentiles for the full record.
# Dates when sites came online
# W1 - 1956
# W2 - Oct 1957
# W3 - Oct 1957
# W4 - July 1960
# W5 - 1962
# W6 - 1963
# W9 - 1995
q_perc <- q_dat %>%
  group_by(WS) %>%
  summarize(perc10 = quantile(Streamflow,
                              probs = 0.1,
                              na.rm = TRUE),
            perc50 = quantile(Streamflow,
                              probs = 0.5,
                              na.rm = TRUE),
            perc90 = quantile(Streamflow,
                              probs = 0.9,
                              na.rm = TRUE),
            perc99 = quantile(Streamflow,
                              probs = 0.99,
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
         high_flow_day = case_when(Streamflow >= perc99 ~ 1,
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
# saveRDS(q_metrics,
#         "data_working/low_high_flow_days_cvQ.rds")

# And trim down to sites and years of interest.
q_metrics_trim <- q_metrics %>%
  filter(WS %in% c(5,6)) %>%
  filter(water_year %in% c(2017, 2018, 2019,
                           2020, 2021, 2022,
                           2023, 2024))

# Also calculate peak and cumulative April flows.
q_april <- q_dat %>%
  mutate(month = month(DATE),
         year = year(DATE)) %>%
  mutate(water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
  filter(month == 4) %>%
  group_by(WS, water_year) %>%
  summarize(max_apr_q = max(Streamflow, na.rm = TRUE),
            sum_apr_q = sum(Streamflow, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(max_apr_q_perc = percent_rank(max_apr_q),
         sum_apr_q_perc = percent_rank(sum_apr_q))

# Export dataset.
# saveRDS(q_april,
#         "data_working/april_flows.rds")

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

##### Chem Metrics #####

# Create aggregated chemistry dataset (reported in Table 1)
chem_metrics_17onward <- chem_dat %>%
  mutate(month = month(date),
         year = year(date),
         water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
  filter(water_year > 2017) %>%
  filter(site %in% c("W1", "W2", "W3", "W4", 
                     "W5", "W6", "W9", "HBK")) %>%
  group_by(site) %>%
  summarize(pH_avg = mean(pH, na.rm = TRUE),
            ph_2.5 = quantile(pH, probs = 0.025, na.rm = TRUE),
            ph_97.5 = quantile(pH, probs = 0.975, na.rm = TRUE),
            spCond_avg = mean(spCond, na.rm = TRUE),
            spCond_2.5 = quantile(spCond, probs = 0.025, na.rm = TRUE),
            spCond_97.5 = quantile(spCond, probs = 0.975, na.rm = TRUE),
            temp_avg = mean(temp, na.rm = TRUE),
            temp_2.5 = quantile(temp, probs = 0.025, na.rm = TRUE),
            temp_97.5 = quantile(temp, probs = 0.975, na.rm = TRUE),
            NO3_N_avg = mean(NO3_N, na.rm = TRUE),
            NO3_N_2.5 = quantile(NO3_N, probs = 0.025, na.rm = TRUE),
            NO3_N_97.5 = quantile(NO3_N, probs = 0.975, na.rm = TRUE),
            PO4_avg = mean(PO4, na.rm = TRUE),
            PO4_2.5 = quantile(PO4, probs = 0.025, na.rm = TRUE),
            PO4_97.5 = quantile(PO4, probs = 0.975, na.rm = TRUE),
            DOC_avg = mean(DOC, na.rm = TRUE),
            DOC_2.5 = quantile(DOC, probs = 0.025, na.rm = TRUE),
            DOC_97.5 = quantile(DOC, probs = 0.975, na.rm = TRUE),
            chla_avg = mean(chla_T, na.rm = TRUE),
            chla_2.5 = quantile(chla_T, probs = 0.025, na.rm = TRUE),
            chla_97.5 = quantile(chla_T, probs = 0.975, na.rm = TRUE),) %>%
  ungroup()

# Create unaggregated chemistry dataset for use in figures.
# First, need calculate average and sd pH values for sites.
site_pH <- chem_dat %>%
  select(site, date, pH) %>%
  filter(date > "2017-05-31") %>%
  group_by(site) %>%
  summarize(mean_pH = mean(pH, na.rm = TRUE),
            sd_pH = sd(pH, na.rm = TRUE))

# And append this data to calculate instances of acidic conditions.
low_pH_events <- chem_dat %>%
    mutate(month = month(date),
           year = year(date)) %>%
    mutate(water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                  month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
    filter(water_year > 2016) %>%
    filter(site %in% c("W1", "W2", "W3", "W4", 
                       "W5", "W6", "W9", "HBK")) %>%
    group_by(site, date) %>%
    summarize(daily_pH = mean(pH, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(site_pH) %>%
    mutate(low_pH = case_when(daily_pH < (mean_pH - sd_pH) ~ 1,
                            TRUE ~ 0)) %>%
    mutate(month = month(date),
           year = year(date)) %>%
    mutate(water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                  month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
    group_by(site, water_year) %>%
    summarize(low_pH_days = sum(low_pH)) %>%
    ungroup()

chem_metrics <- chem_dat %>%
  mutate(month = month(date),
         year = year(date),
         water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
  filter(water_year > 2016) %>%
  filter(site %in% c("W1", "W2", "W3", "W4", 
                     "W5", "W6", "W9", "HBK")) %>%
  group_by(site, water_year) %>%
  summarize(mean_NO3 = mean(NO3_N, na.rm = TRUE),
            mean_PO4 = mean(PO4, na.rm = TRUE),
            mean_chla = mean(chla_T, na.rm = TRUE)) %>%
  ungroup() %>%
  full_join(low_pH_events)

# Export dataset.
# saveRDS(chem_metrics,
#         "data_working/nuts_chla_pH.rds")

#### Join ####

# Trim Q dataset to years and sites of interest.
q_2017 <- q_dat %>%
  filter(DATE > "2016-12-31") %>%
  filter(WS %in% c("1","2","3","4","5","6","9","HBK")) %>%
  mutate(WS = as.character(WS)) %>%
  select(DATE, WS, Streamflow)

# Trim chemistry dataset to years of interest.
chem_2017 <- chem_dat %>%
  filter(date > "2016-12-31") %>%
  rename(DATE = date) %>%
  filter(site %in% c("W1","W2","W3","W4","W5","W6","W9","HBK")) %>%
  mutate(WS = case_when(site == "W1" ~ "1",
                        site == "W2" ~ "2",
                        site == "W3" ~ "3",
                        site == "W4" ~ "4",
                        site == "W5" ~ "5",
                        site == "W6" ~ "6",
                        site == "W9" ~ "9",
                        site == "HBK" ~ site,
                        TRUE ~ NA))

# Combine the two datasets.
data_2017 <- left_join(q_2017, chem_2017,
                       by = join_by(DATE, WS))

# Export for use in other scripts.
# saveRDS(data_2017, "data_working/stream_climate_qchem.rds")

# End of script.
