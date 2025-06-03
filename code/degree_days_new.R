### Degree Days Calculation (revised)
### June 3, 2025
### Heili Lowman

#### README ####

# The following script will calculate cumulative degree days
# leading up to peak emergence for every counted year.

#### TO-DOs ####

# (1) Figure out start other than January 1st for each year.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load temperature data.
temp_dat <- readRDS("data_working/stream_temps_w1234569.rds")

# Load emergence data.
peak_dat <- readRDS("data_working/warm_peak_emerge_dates_051325.rds")
emerge_dat <- readRDS("data_working/aquatic_counts_long_051325.rds")

#### Tidy ####

# Need to trim down the emergence data to stop at peak emergence.
# Create a dataset of total weekly emergence for all aquatic taxa.
dat_total_weekly <- emerge_dat %>%
  group_by(watershed, Date) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Year = year(Date))

dat_peaks <- peak_dat %>%
  select(watershed, Year, Date) %>%
  rename(peak_Date = Date)

dat_weekly_peaks <- left_join(dat_total_weekly, dat_peaks) %>%
  mutate(prepost = case_when(Date <= peak_Date ~ 1,
                             Date > peak_Date ~ 0))

dat_pre_peaks <- dat_weekly_peaks %>%
  filter(prepost == 1) %>%
  rename(date = Date) %>%
  select(watershed, Year, date, total_count)

# Join peaks with temperature data.
temp_dat <- temp_dat %>%
  mutate(Year = year(date)) %>%
  mutate(watershed = case_when(watershed == "W1" ~ "1",
                               watershed == "W2" ~ "2",
                               watershed == "W3" ~ "3",
                               watershed == "W4" ~ "4",
                               watershed == "W5" ~ "5",
                               watershed == "W6" ~ "6",
                               watershed == "W9" ~ "9"))

dat_temp_peaks <- left_join(temp_dat, dat_peaks,
                            by = c("watershed", "Year")) %>%
  mutate(prepost = case_when(date <= peak_Date ~ 1,
                             date > peak_Date ~ 0))

dat_temp_pre_peaks <- dat_temp_peaks %>%
  filter(prepost == 1) %>%
  select(watershed, Year, date, mean_tempC_sw)

# Need to infill temperature values, using mean for now.
dat_temp_pre_peaks <- dat_temp_pre_peaks %>%
  mutate(mean_tempC_ed = ifelse(is.na(mean_tempC_sw),
          (lag(mean_tempC_sw, default = NA) + lead(mean_tempC_sw, default = NA)) / 2, #if
          mean_tempC_sw)) # else

# This leaves two main instances of missingness - a two-day period in
# 2020 in W1, and a few weeks in January 2022 in W6.

# And finally join with the sticky trap dates/counts.
dat_temp_counts <- left_join(dat_temp_pre_peaks, dat_total_weekly,
                             by = c("watershed", "Year", "date" = "Date"))

#### Degree Days ####

dat_deg_days <- dat_temp_counts %>%
  # making NAs zeroes for now
  mutate(mean_tempC_ed = replace_na(mean_tempC_ed, 0)) %>%
  mutate(degree_day = mean_tempC_ed - 0) %>%
  group_by(watershed, Year) %>%
  mutate(sum_degree_days = cumsum(degree_day))

#### Plot ####

(fig_dd_w6 <- ggplot(dat_deg_days %>%
                    filter(watershed == 6),
                 aes(x = sum_degree_days,
                     y = total_count,
                     color = factor(Year))) +
  geom_point(alpha = 0.8, size = 3) +
  labs(x = "Cumulative Degree Days",
       y = "Total Week's Emergence") +
  theme_bw() +
  facet_grid(Year~.))
  

# End of script.

