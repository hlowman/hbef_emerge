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
ppt_dat <- read_csv("data_raw/dailyWatershedPrecip1956-2025.csv") # Precipitation is in mm.
chem_dat <- read_csv("data_raw/HubbardBrook_weekly_stream_chemistry_1963-2024.csv") # Weekly chem data.

weir1_dat <- read_csv("data_raw/sediment_weir_1.csv")
weir5_dat <- read_csv("data_raw/sediment_weir_5.csv")
weir6_dat <- read_csv("data_raw/sediment_weir_6.csv")

#### Tidy ####

# Edits to flow and chemistry data for context plot below.
q_dat <- q_dat %>%
  filter(WS %in% c(1,2,3,4,5,6,9))

q_dat_present <- q_dat %>%
  mutate(year = year(DATE)) %>%
  mutate(keep = case_when(WS == 1 & year %in% c(2018,2019,2020) |
                          WS == 2 & year %in% c(2018,2019,2020) |
                          WS == 3 & year %in% c(2018,2019,2020) |
                          WS == 4 & year %in% c(2018,2019,2020) |
                          WS == 5 & year %in% c(2018,2019, 2020,
                                                         2021, 2022, 2023, 2024) |
                          WS == 6 & year %in% c(2018,2019, 2020,
                                                         2021, 2022, 2023, 2024) |
                          WS == 9 & year %in% c(2018,2019,2020) ~ "Yes",
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

#### Figures ####

# Historical context figures for temperature, flow, and nutrients.
# And filtering to overlap a similar historical period (>1964) as
# some of the other analyses.

# Discharge
(fig_q <- ggplot(q_dat %>%
                   filter(DATE > as.Date("1964-12-31")), 
                 aes(x = Streamflow)) +
   geom_histogram(color = "black", fill = "white") +
   geom_histogram(data = q_dat_present,
                  color = "black", fill = "gray40") +
   scale_x_log10() +
   scale_y_log10() +
   labs(x = "Discharge (mm/day)",
        y = "Records") +
   theme_bw())

# Temperature
(fig_temp <- ggplot(chem_dat %>%
                    filter(date > as.Date("1964-12-31")), 
                 aes(x = temp)) +
    geom_histogram(color = "black", fill = "white") +
    geom_histogram(data = chem_dat_present,
                   color = "black", fill = "gray40") +
    scale_y_log10() +
    labs(x = "Temperature (°C)",
         y = "Records") +
    theme_bw())

# pH
(fig_pH <- ggplot(chem_dat %>%
                      filter(date > as.Date("1964-12-31")), 
                    aes(x = pH)) +
    geom_histogram(color = "black", fill = "white") +
    geom_histogram(data = chem_dat_present,
                   color = "black", fill = "gray40") +
    scale_y_log10() +
    labs(x = "pH",
         y = "Records") +
    theme_bw())

# NO3
(fig_NO3 <- ggplot(chem_dat %>%
                    filter(date > as.Date("1964-12-31")), 
                  aes(x = NO3)) +
    geom_histogram(color = "black", fill = "white") +
    geom_histogram(data = chem_dat_present,
                   color = "black", fill = "gray40") +
    scale_x_log10() +
    scale_y_log10() +
    labs(x = "Nitrate (mg/L)",
         y = "Records") +
    theme_bw())

# Combine into a single figure.
(fig_historic <- (fig_q + fig_temp) / (fig_pH + fig_NO3) +
    plot_annotation(tag_levels = "A"))

# And export.
ggsave(plot = fig_historic,
       filename = "figures/conditions_historic_v_present_091625.jpg",
       width = 22,
       height = 20,
       units = "cm")

# End of script.
