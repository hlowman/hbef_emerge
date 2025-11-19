### SI Figures
### September 16, 2025
### Heili Lowman

#### README ####

# The following script will create supplementary figures
# as part of the insect emergence manuscript.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)

# Load datasets.
q_dat <- read_csv("data_raw/HBEF_DailyStreamflow_1956-2024.csv") # Streamflow is in mm/day.
q_stats <- readRDS("data_working/low_high_flow_days_cvQ.rds") # With low and high flow counts populated.
ppt_dat <- read_csv("data_raw/dailyWatershedPrecip1956-2025.csv") # Precipitation is in mm.
temp_dat <- read_csv("data_raw/HBEF_air_temp_daily.csv") # Daily air temperature in Celsius.
chem_dat <- read_csv("data_raw/HubbardBrook_weekly_stream_chemistry_1963-2024.csv") # Weekly chem data.
insect_dat <- readRDS("data_working/aquatic_counts_complete_yrs_081425.rds") # Insect count data.
stream_dat <- readRDS("data_working/stream_climate_qchem.rds") # Summarized chemistry data.

weir1_dat <- read_csv("data_raw/sediment_weir_1.csv")
weir5_dat <- read_csv("data_raw/sediment_weir_5.csv")
weir6_dat <- read_csv("data_raw/sediment_weir_6.csv")

#### Tidy ####

# Edits to flow and chemistry data for context plot below.
# Will only present data at one site so as not to bias outputs.
q_dat <- q_stats %>%
  filter(WS == 6)

q_dat_present <- q_dat %>%
  mutate(keep = case_when(WS == 6 & water_year %in% c(2017, 2018, 2019, 2020,
                                                2021, 2022, 2023, 2024) ~ "Yes",
                          TRUE ~ NA)) %>%
  filter(keep == "Yes")

temp_trim <- temp_dat %>%
  filter(STA == "STA1") %>%
  mutate(month = month(date),
         year = year(date),
         water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
  group_by(water_year) %>%
  summarize(mean_air_temp = mean(AVE, na.rm = TRUE)) %>%
  ungroup()

temp_dat_present <- temp_trim %>%
  mutate(keep = case_when(water_year %in% c(2017, 2018, 2019, 2020,
                                            2021, 2022, 2023, 2024) ~ "Yes",
                          TRUE ~ NA)) %>%
  filter(keep == "Yes")

chem_dat <- chem_dat %>%
  filter(site %in% c("W1", "W2", "W3",
                     "W4", "W5", "W6",
                     "W9"))

chem_dat_present <- chem_dat %>% 
  mutate(year = year(date)) %>%
  mutate(keep = case_when(site == "W1" & year %in% c(2018,2019,2020) |
                          site == "W2" & year %in% c(2018,2019,2020) |
                          site == "W3" & year %in% c(2018,2019,2020) |
                          site == "W4" & year %in% c(2018,2019,2020) |
                          site == "W5" & year %in% c(2018,2019, 2020,
                                                  2021, 2022, 2023, 2024) |
                          site == "W6" & year %in% c(2018,2019, 2020,
                                                  2021, 2022, 2023, 2024) |
                          site == "W9" & year %in% c(2018,2019,2020) ~ "Yes",
                          TRUE ~ NA)) %>%
  filter(keep == "Yes")

# Filtering/joining insect and temperature data.
# Trim only to black flies and summarize by week.
# Summarize by order.
dat_dipt <- insect_dat %>%
  filter(Order == "dipteran") %>%
  group_by(watershed, year, Date) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = month(Date))

# The stream data doesn't always perfectly align with the
# insect collections, so using the previous measurement of
# temperature here.
stream_dat_filled <- stream_dat %>%
  fill(temp)

# Join with stream data.
dat_all <- full_join(dat_dipt, stream_dat_filled,
                     by = join_by("watershed" == "WS",
                                  "Date" == "DATE"))

# Trimming weir data to the dates of interest.
weir1_dat_recent <- weir1_dat %>%
  mutate(year = year(mdy(`end date`))) %>%
  filter(year > 2017 & year < 2023) %>%
  mutate(watershed = "W1")

# Replace text in organic column with NA.
weir1_dat_recent[3,3] <- NA

weir5_dat_recent <- weir5_dat %>%
  mutate(year = year(mdy(`end date`))) %>%
  filter(year > 2017 & year < 2023) %>%
  mutate(watershed = "W5")

# Replace text in organic column with NA.
weir5_dat_recent[3,3] <- NA

weir6_dat_recent <- weir6_dat %>%
  mutate(year = year(mdy(`end date`))) %>%
  filter(year > 2017 & year < 2023) %>%
  mutate(watershed = "W6")

# Replace text in organic column with NA.
weir6_dat_recent[3,3] <- NA

# And join the above together.
dat_weirs <- full_join(weir1_dat_recent, weir5_dat_recent)
dat_weirs <- full_join(dat_weirs, weir6_dat_recent)

# And make in long format for easier stacked bar plotting.
dat_weirs_long <- dat_weirs %>%
  mutate(organic = as.numeric(organic)) %>%
  select(`start date`:total, year, watershed) %>%
  pivot_longer(cols = organic:mineral,
               names_to = "Fraction")

#### Figures ####

##### Temp vs. Emergence #####

# Including only W5 & W6.
(fig_about_peak <- ggplot(dat_all %>% 
                            mutate(keep = case_when(duplicate == "Dup" ~ "No",
                                                    watershed == 5 & year %in% 
                                                      c(2018,2019, 2020,
                                                        2021, 2022, 2023, 2024) |
                                                    watershed == 6 & year %in% 
                                                      c(2018,2019, 2020,
                                                        2021, 2022, 2023, 2024) ~ "Yes",
                                                    TRUE ~ NA)) %>%
                            filter(keep == "Yes"),
                          aes(x = temp, y = total_count,
                              shape = watershed)) +
   geom_point(size = 5, stroke = 2, alpha = 0.5) +
   scale_shape_manual(values = c(16, 21)) +
   labs(x = "Daytime Temperature (°C)",
        y = "Weekly Aquatic Diptera Emergence",
        shape = "Watershed") +
   theme_bw() +
   theme(text = element_text(size = 20)))

# Export figure.
# ggsave(plot = fig_about_peak,
#        filename = "figures/temp_about_peak_W5W6_111825.jpg",
#        width = 24,
#        height = 14,
#        units = "cm")

##### Weir Ponds #####

# First, a figure of the weir basin contents.
(weir_barplot <- ggplot(dat_weirs_long, 
                        aes(x = year, 
                            y = value,
                            fill = Fraction)) +
    geom_bar(position="stack", stat="identity") +
    labs(x = "Year", y = "Dried Material (kg/ha)") +
    theme_bw() +
    # theme(text = element_text(size = 20),
    #       legend.position = "top") +
    scale_fill_manual(labels = c("Mineral", "Mixed", "Organic"),
                      values = c("#DED4C8", "#AD6F4F", "#AEC96F")) +
    facet_grid(watershed~.))

# Export figure.
# ggsave(plot = weir_barplot,
#        filename = "figures/weir_recent_091625.jpg",
#        width = 20,
#        height = 15,
#        units = "cm")

##### Historical #####

# Historical context figures for temperature, flow, and nutrients.
# And filtering to overlap a similar historical period (>1964) as
# some of the other analyses.

# Discharge panels
(fig_q_low <- ggplot(q_dat, 
                 aes(x = low_flow_days)) +
   geom_histogram(color = "black", fill = "white",
                  bins = 20) +
   geom_histogram(data = q_dat_present,
                  color = "black", fill = "gray40",
                  bins = 20) +
   scale_x_log10() +
   labs(x = "Low Flow Days",
        y = "Water Years") +
   theme_bw())

(fig_q_high <- ggplot(q_dat, 
                     aes(x = high_flow_days)) +
    geom_histogram(color = "black", fill = "white",
                   bins = 10) +
    geom_histogram(data = q_dat_present,
                   color = "black", fill = "gray40",
                   bins = 10) +
    scale_x_continuous(breaks = c(0,2,4,6,8)) +
    labs(x = "High Flow Days",
         y = "Water Years") +
    theme_bw())

(fig_cvq <- ggplot(q_dat, 
                      aes(x = cv_q)) +
    geom_histogram(color = "black", fill = "white",
                   bins = 20) +
    geom_histogram(data = q_dat_present,
                   color = "black", fill = "gray40",
                   bins = 20) +
    labs(x = expression(CV[Q]),
         y = "Water Years") +
    theme_bw())

# Air temperature panel
(fig_air_temp <- ggplot(temp_trim %>%
                          filter(water_year > 1955), 
                     aes(x = mean_air_temp)) +
    geom_histogram(color = "black", fill = "white",
                   bins = 20) +
    geom_histogram(data = temp_dat_present,
                   color = "black", fill = "gray40",
                   bins = 20) +
    #scale_x_log10() +
    labs(x = "Mean Annual Air Temperature (°C)",
         y = "Water Years") +
    theme_bw())

# Combine into a single figure.
(fig_historic <- (fig_q_low + fig_q_high) / (fig_cvq + fig_air_temp) +
    plot_annotation(tag_levels = "A"))

# And export.
# ggsave(plot = fig_historic,
#        filename = "figures/conditions_historic_v_present_111825.jpg",
#        width = 20,
#        height = 18,
#        units = "cm")

# End of script.
