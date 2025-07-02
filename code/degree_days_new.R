### Degree Days Calculation (revised)
### June 3, 2025
### Heili Lowman

#### README ####

# The following script will calculate cumulative degree days
# leading up to peak emergence for every counted year.

#### TO-DOs ####

# (1) Determine if January 1st is an appropriate start for each year.
# (2) Determine if 1degree Celsius is an appropriate baseline.
# (3) Calculate offset between watersheds for which there is data missing but
# its next nearest neighbor has data to help fill in. Missing site-years include:
# - W1 2018 & 2019
# - W3 2020 (don't trust this data due to drift)

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load temperature data.
temp_dat <- readRDS("data_working/stream_temps_w1234569.rds")

# Load emergence data.
peak_dat <- readRDS("data_working/warm_peak_emerge_dates_070125.rds")
emerge_dat <- readRDS("data_working/aquatic_counts_long_070125.rds")

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

# dat_pre_peaks <- dat_weekly_peaks %>%
#   filter(prepost == 1) %>%
#   rename(date = Date) %>%
#   select(watershed, Year, date, total_count)

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

# dat_temp_pre_peaks <- dat_temp_peaks %>%
#   filter(prepost == 1) %>%
#   select(watershed, Year, date, mean_tempC_sw)

# Need to infill temperature values, using mean for now.
dat_temp_peaks <- dat_temp_peaks %>%
  mutate(mean_tempC_ed = ifelse(is.na(mean_tempC_sw),
          (lag(mean_tempC_sw, default = NA) + lead(mean_tempC_sw, default = NA)) / 2, #if
          mean_tempC_sw)) # else

# This leaves two main instances of missingness - a two-day period in
# 2020 in W1, and a few weeks in January 2022 in W6.
# Assuming in both instances that these temperatures are below 1 deg C.

# And finally join with the sticky trap dates/counts.
dat_temp_counts <- left_join(dat_temp_peaks, dat_total_weekly,
                             by = c("watershed", "Year", "date" = "Date"))

#### Degree Days ####

dat_deg_days <- dat_temp_counts %>%
  # making NAs zeroes for now
  mutate(mean_tempC_ed = replace_na(mean_tempC_ed, 0)) %>%
  mutate(degree_day = mean_tempC_ed - 1) %>% # using 1 deg C as baseline
  mutate(degree_day_ed = case_when(degree_day > 0 ~ degree_day,
                                   TRUE ~ 0)) %>%
  group_by(watershed, Year) %>%
  mutate(sum_degree_days = cumsum(degree_day_ed)) %>%
  # and add column for julian day
  mutate(DOY = yday(date),
         peakDOY = yday(peak_Date))

dat_deg_days_trim <- dat_deg_days %>%
  mutate(keep = case_when(DOY == peakDOY ~ 1,
                          TRUE ~ 0)) %>%
  filter(keep == 1)

#### Plot ####

(fig_degday <- ggplot(dat_deg_days) +
   geom_line(aes(x = DOY, y = sum_degree_days)) +
   geom_vline(aes(xintercept = peakDOY)) +
   labs(x = "Day of Year",
        y = "Cumulative Degree Days") +
   facet_grid(watershed ~ Year) +
   theme_bw()) 

# hmmm this isn't super intuitive, so need to make another plot
# to show consistently of emergence timing across years

# And create list with which to add in count annotations
dat_text <- data.frame(
  label = c("n = 5", "n = 4", "n = 6", "n = 2", "n = 2"),
  Year = c(2018, 2019, 2020, 2021, 2022))

(fig_degday_panels <- ggplot(dat_deg_days) +
    geom_line(aes(x = DOY, y = sum_degree_days, color = watershed),
              alpha = 0.75) +
    geom_vline(aes(xintercept = peakDOY, color = watershed),
               alpha = 0.75, linewidth = 0.75) +
    scale_color_manual(values = c("#C0D7B5", "#AFC5A4", "#9BAF90", 
                                 "#829375", "#626F52", "#475035", "#3B422D")) +
    labs(x = "Day of Year",
         y = "Cumulative Degree Days") +
    facet_grid(. ~ Year) +
    geom_text(data = dat_text,
              mapping = aes(x = 290, y = 6000, label = label)) +
    theme_bw())

# Export figure.
# ggsave(plot = fig_degday_panels,
#        filename = "figures/peak_degdays_070225.jpg",
#        width = 40,
#        height = 8,
#        units = "cm")

(fig_degree_days <- ggplot(dat_deg_days_trim,
                           aes(x = watershed,
                               y = sum_degree_days,
                               fill = factor(Year))) +
    geom_point(size = 6, shape = 21, alpha = 0.75) +
    scale_fill_manual(values = c("white", "#C6B6E9",
                                 "#AA91DE","#8D6DD3", "#7148C8",
                                 "#5524BD","#3900B3")) +
    labs(x = "Watershed",
         y = "Cumulative Degree Days\nat Peak Emergence",
         fill = "Year") +
    ylim(c(100,500)) +
    theme_bw() +
    theme(text = element_text(size = 14)))

# ggsave(plot = fig_degree_days,
#        filename = "figures/peak_degree_days_070125.jpg",
#        width = 14,
#        height = 10,
#        units = "cm")

mean(dat_deg_days_trim$peakDOY) # 146 or May 26
sd(dat_deg_days_trim$peakDOY) # 9 or just over 1 week (which is data resolution)

mean(dat_deg_days_trim$mean_tempC_ed) # 12.5 deg C
sd(dat_deg_days_trim$mean_tempC_ed) # 3.7 deg C

mean(dat_deg_days_trim$degree_day_ed) # 11.51
sd(dat_deg_days_trim$degree_day_ed) # 3.7 deg C

# Removing the one weird day in W3 before calculating deg day stats
dat_deg_days_trim_ed <- dat_deg_days_trim %>%
  filter(sum_degree_days < 1000)

mean(dat_deg_days_trim_ed$sum_degree_days) # 280 deg. days
sd(dat_deg_days_trim_ed$sum_degree_days) # 57 deg. days

# End of script.

