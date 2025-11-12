### Time series figures
### November 11, 2025
### Heili Lowman

#### README ####

# The following script will create figures to be added to
# the long-term trends figure in the Discussion section.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)

# Load necessary data.
# Using high-frequency data provided by Mark
q <- read_csv("data_raw/W3_high_freq_flow.csv")

# Using low flow data provided by Kevin
low <- read_csv("data_raw/W3_min7day.csv")

# Historical weekly stream chemistry dataset from EDI.
chem <- read_csv("data_raw/HubbardBrook_weekly_stream_chemistry_1963-2024.csv")

# Weekly chemistry data starting in 2013 with ecology data.
chem_2013 <- read_csv("data_raw/HBEFdata_Current_2025-07-18.csv") 

#### Tidy ####

# Tidy flow data.
q <- q %>%
  mutate(date = date(ts),
         month = month(ts),
         year = year(ts)) %>%
  mutate(water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
  # removing 2023 since it is not a complete year
  filter(water_year < 2023)

# Tidy low flow data.
low <- low %>%
  filter(GroupCount > 364)

# Tidy chemistry data.
chem <- chem %>%
  mutate(month = month(date),
         year = year(date)) %>%
  mutate(water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year))

chem_2013 <- chem_2013 %>%
  mutate(month = month(date),
         year = year(date)) %>%
  mutate(water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year))

#### Plot ####

##### High flow #####

# Calculate maximum annual recorded flow.
max_q <- q %>%
  group_by(water_year) %>%
  summarize(max_q = max(q_Ls, na.rm = TRUE)) %>%
  ungroup() %>%
  # remove two years that do not exist
  filter(water_year > 1956)

(fig_max <- ggplot(max_q, aes(x = water_year,
                              y = max_q)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(1970, 1990, 2010)) +
    labs(y = "Maximum Flow (L/s)",
         x = "Water Year") +
    theme_bw())

ggsave(plot = fig_max,
       filename = "figures/max_flows_111125.jpg",
       width = 8,
       height = 6,
       units = "cm")

##### Low flow #####

(fig_min <- ggplot(low, aes(x = WY,
                            y = Min_Q7)) +
   geom_line() +
   geom_point(size = 1) +
   scale_x_continuous(breaks = c(1970, 1990, 2010)) +
   labs(y = "Minimum 7-day Flow (L/s)",
        x = "Water Year") +
   theme_bw())

ggsave(plot = fig_min,
       filename = "figures/min_flows_111125.jpg",
       width = 8,
       height = 6,
       units = "cm")

##### Temperature #####

# Calculate mean annual temperature.
w3_temp <- chem %>%
  filter(site == "W3") %>%
  group_by(water_year) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(water_year > 1964)

(fig_temp <- ggplot(w3_temp, aes(x = water_year,
                                 y = mean_temp)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(1970, 1990, 2010)) +
    labs(y = "Mean Temperature (°C)",
         x = "Water Year") +
    theme_bw())

ggsave(plot = fig_temp,
       filename = "figures/mean_temp_111125.jpg",
       width = 8,
       height = 6,
       units = "cm")

# also plot the remaining sites for added context
w_temp <- chem %>%
  group_by(site, water_year) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(water_year > 1964)

(fig_temps <- ggplot(w_temp, aes(x = water_year,
                                 y = mean_temp)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(1970, 1990, 2010)) +
    labs(y = "Mean Temperature (°C)",
         x = "Water Year") +
    facet_wrap(site~.) +
    theme_bw()) # yep they're warming...
# albeit in a kind of parabola-looking shape

# also, out of curiosity, plotting max spring temps
# per Bernhardt et al. 2005

w3_march_max_temp <- chem %>%
  filter(site == "W3") %>%
  filter(month == 3) %>%
  group_by(water_year) %>%
  summarize(max_March_temp = max(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(water_year > 1964)

(fig_March_temp <- ggplot(w3_march_max_temp, aes(x = water_year,
                                 y = max_March_temp)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(1970, 1990, 2010)) +
    labs(y = "Max. March Temperature (°C)",
         x = "Water Year") +
    theme_bw()) # slight increase

w3_apr_max_temp <- chem %>%
  filter(site == "W3") %>%
  filter(month == 4) %>%
  group_by(water_year) %>%
  summarize(max_Apr_temp = max(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(water_year > 1964)

(fig_Apr_temp <- ggplot(w3_apr_max_temp, aes(x = water_year,
                                                 y = max_Apr_temp)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(1970, 1990, 2010)) +
    labs(y = "Max. April Temperature (°C)",
         x = "Water Year") +
    theme_bw()) # huh, that's interesting - decreasing variability
# maybe due to lack of snow?

w3_may_max_temp <- chem %>%
  filter(site == "W3") %>%
  filter(month == 5) %>%
  group_by(water_year) %>%
  summarize(max_May_temp = max(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(water_year > 1964)

(fig_May_temp <- ggplot(w3_may_max_temp, aes(x = water_year,
                                               y = max_May_temp)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(1970, 1990, 2010)) +
    labs(y = "Max. May Temperature (°C)",
         x = "Water Year") +
    theme_bw()) # no change

##### pH #####

# Calculate maximum annual recorded flow.
w3_pH <- chem %>%
  filter(site == "W3") %>%
  group_by(water_year) %>%
  summarize(mean_pH = mean(pH, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(water_year > 1964)

(fig_pH <- ggplot(w3_pH, aes(x = water_year,
                             y = mean_pH)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(1970, 1990, 2010)) +
    labs(y = "Mean pH",
         x = "Water Year") +
    theme_bw())

ggsave(plot = fig_pH,
       filename = "figures/mean_pH_111125.jpg",
       width = 8,
       height = 6,
       units = "cm")

##### Algae #####

# Calculate mean annual algae.
w3_algae <- chem_2013 %>%
  filter(site == "W3") %>%
  group_by(water_year) %>%
  summarize(mean_chla = mean(chla_M, na.rm = TRUE)) %>%
  ungroup()

(fig_algae <- ggplot(w3_algae, aes(x = water_year,
                                   y = mean_chla)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(2010, 2015, 2020)) +
    labs(y = "Mean chl a (mg/L)",
         x = "Water Year") +
    theme_bw())

ggsave(plot = fig_algae,
       filename = "figures/mean_algae_111125.jpg",
       width = 8,
       height = 6,
       units = "cm")

# End of script.
