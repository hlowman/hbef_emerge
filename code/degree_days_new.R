### Degree Days Calculation (revised)
### June 3, 2025
### Heili Lowman

#### README ####

# The following script will calculate cumulative degree days
# leading up to peak emergence for every counted year.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

# Load temperature data.
temp_dat <- readRDS("data_working/stream_temps_w1234569.rds")

# Load emergence data.
early_peak_dip_dat <- readRDS("data_working/warm_peak_emerge_dip_dates_070725.rds")
peak_dat <- readRDS("data_working/warm_peak_emerge_dates_070125.rds")
emerge_dat <- readRDS("data_working/aquatic_counts_complete_yrs_071825.rds")

#### Tidy ####

early_peak_dip_dat <- early_peak_dip_dat %>%
  rename(peak_Date = Date,
         peak_count = total_count)

# Need to trim down the emergence data to early season.

# Summarize by order.
dat_order <- emerge_dat %>%
  group_by(watershed, year, Date, Order) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

# Trim data down to only black flies in watersheds 5 & 6.
dat_dipt <- dat_order %>%
  filter(Order == "dipteran") %>%
  filter(watershed %in% c(5,6)) %>%
  mutate(month = month(Date)) %>%
  # a quick eyeball suggests all peaks happen post October 1 (!)
  mutate(period = case_when(month < 10 ~ "early",
                            TRUE ~ "late")) %>%
  rename(Year = year,
         date = Date) %>%
  select(-month)

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

dat_temp_peaks <- left_join(temp_dat, early_peak_dip_dat,
                            by = c("watershed", "Year")) %>%
  mutate(prepost = case_when(date <= peak_Date ~ 1,
                             date > peak_Date ~ 0))

# Need to infill temperature values, using mean for now.
dat_temp_peaks <- dat_temp_peaks %>%
  mutate(mean_tempC_ed = ifelse(is.na(mean_tempC_sw),
          (lag(mean_tempC_sw, default = NA) + lead(mean_tempC_sw, default = NA)) / 2, #if
          mean_tempC_sw)) # else

# This leaves two main instances of missingness - a two-day period in
# 2020 in W1, and a few weeks in January 2022 in W6.

# Also need to join emergence values to this.
dat_temp_peaks_emerge <- left_join(dat_temp_peaks, dat_dipt,
                                   by = c("watershed", "Year", "date"))

#### Degree Days & Cumulative Emergence ####

# Assuming in both instances of missingness that temperatures are below 1 deg C.
dat_deg_days <- dat_temp_peaks_emerge %>%
  # making NAs zeroes for now
  mutate(mean_tempC_ed = replace_na(mean_tempC_ed, 0)) %>%
  mutate(degree_day = mean_tempC_ed - 4) %>% # using 4 deg C as baseline
  mutate(degree_day_ed = case_when(degree_day > 0 ~ degree_day,
                                   TRUE ~ 0)) %>%
  group_by(watershed, Year) %>%
  mutate(total_count_ed = case_when(total_count > 0 ~ total_count,
                                    TRUE ~ 0)) %>%
  mutate(sum_degree_days = cumsum(degree_day_ed),
         sum_emergence = cumsum(total_count_ed)) %>%
  # and add column for julian day
  mutate(DOY = yday(date),
         peakDOY = yday(peak_Date)) %>% 
  ungroup()

#### Plot ####

(fig_degday <- ggplot(dat_deg_days) +
   geom_line(aes(x = DOY, y = sum_degree_days)) +
   geom_vline(aes(xintercept = peakDOY)) +
   labs(x = "Day of Year",
        y = "Cumulative Degree Days") +
   facet_grid(watershed ~ Year) +
   theme_bw()) 

# hmmm this isn't super intuitive, so need to make another plot
# to show consistency of emergence timing across years

dat_peaks <- dat_deg_days %>%
  filter(watershed %in% c(5,6)) %>%
  select(watershed, Year, peakDOY) %>%
  group_by(watershed, Year) %>%
  unique() %>%
  ungroup()

# And create list with which to add in DOY annotations
dat_text <- data.frame(
  label = c("Peak DOY = 155", "Peak DOY = 148", "Peak DOY = 147", "Peak DOY = 137", "Peak DOY = 136",
            "Peak DOY = 134", "Peak DOY = 140", "Peak DOY = 149", "Peak DOY = 137", "Peak DOY = 136"),
  Year = c(2018, 2019, 2020, 2021, 2022,
           2018, 2019, 2020, 2021, 2022),
  watershed = c(5,5,5,5,5,
                6,6,6,6,6))

# And we're going to trim down to just W5 & W6.

# Value used to transform the data to add second y-axis.
coeff <- 4

(fig_degday_56 <- ggplot(dat_deg_days %>%
                           filter(watershed %in% c(5,6))) +
    geom_line(aes(x = DOY, y = sum_degree_days),
              linewidth = 0.75) +
    geom_line(aes(x = DOY, y = sum_emergence/coeff, color = factor(Year))) +
    scale_color_brewer(palette = "Dark2") +
    geom_vline(aes(xintercept = peakDOY),
               linewidth = 0.75,
               color = "gray50") +
    labs(x = "Day of Year") +
    scale_y_continuous(
      name = "Cumulative Degree Days", # first axis
      sec.axis = sec_axis(~.*coeff, name = "Cumulative Emergence")) + # second axis
    facet_grid(Year ~ watershed,
               labeller = labeller(
                 watershed = c('5'="Watershed 5",
                               '6'="Watershed 6"))) +
    geom_text(data = dat_text,
              mapping = aes(x = 300, y = 250, label = label)) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Export figure.
# ggsave(plot = fig_degday_56,
#        filename = "figures/early_peak_degdays_072425.jpg",
#        width = 25,
#        height = 25,
#        units = "cm")

(fig_degday_peak_56 <- ggplot(dat_deg_days_trim %>%
                       filter(watershed %in% c(5,6)), 
                     aes(x = watershed, 
                         y = sum_degree_days)) +
    geom_boxplot(linewidth = 0.75, width = 0.4) +
    geom_jitter(size = 7, shape = 21, 
                width = 0.2, alpha = 0.8,
                aes(fill = factor(Year))) +
    labs(y = "Cumulative Degree Days\nat Peak Emergence",
         x = "Watershed",
         fill = "Year") +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Add to time series above.
(fig_degday <- fig_degday_56 + fig_degday_peak_56 +
    plot_layout(widths = c(3, 1)) +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_degday,
#        filename = "figures/degday_dipt_070725.jpg",
#        width = 60,
#        height = 15,
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

