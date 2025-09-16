### Degree Days Figures
### August 12, 2025
### Heili Lowman

#### README ####

# The following script will create figures for cumulative degree days
# leading up to peak emergence for every counted year.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)
library(RColorBrewer)

# Load temperature data.
temp <- readRDS("data_working/stream_temp_daily_W5_W6_2018_2025.rds")

# Load historical weekly dataset from EDI.
chem <- read_csv("data_raw/HubbardBrook_weekly_stream_chemistry_1963-2024.csv")

# Load emergence data.
emerge_dat <- readRDS("data_working/aquatic_counts_complete_yrs_081425.rds")
peak_dates <- readRDS("data_working/peaks_annual_dipt_emerge_081425.rds")

#### Tidy ####

# Need to trim down the emergence data to the early season.

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
temp_long <- temp %>%
  select(date, daily_TempC_W5_interp, daily_TempC_W6_interp) %>%
  pivot_longer(cols = daily_TempC_W5_interp:daily_TempC_W6_interp) %>%
  rename(TempC = value) %>%
  mutate(watershed = case_when(name == "daily_TempC_W5_interp" ~ "5",
                               name == "daily_TempC_W6_interp" ~ "6")) %>%
  select(date, watershed, TempC) %>%
  mutate(Year = year(date))

# Need to join to emergence values.
emerge_temp_dat <- left_join(temp_long, dat_dipt,
                             by = c("watershed", "Year", "date"))

#### Cumulative Degree Days & Emergence ####

# Adding columns to denote cumulative values for plotting purposes.
dat_deg_days <- emerge_temp_dat %>%
  mutate(degree_day = TempC - 4) %>% # using 4 deg C as baseline
  mutate(degree_day_ed = case_when(degree_day > 0 ~ degree_day,
                                   TRUE ~ 0)) %>%
  group_by(watershed, Year) %>% # and starting each year at Jan 1
  mutate(total_count_ed = case_when(total_count > 0 ~ total_count,
                                    TRUE ~ 0)) %>%
  mutate(sum_degree_days = cumsum(degree_day_ed),
         sum_emergence = cumsum(total_count_ed)) %>%
  # and add column for julian day
  mutate(DOY = yday(date)) %>% 
  ungroup()

# And make a separate dataset that match the peak dataset.
peak_dates_56 <- peak_dates %>%
  filter(watershed %in% c("5", "6")) %>%
  rename(date = Date) %>%
  rename(Year = year)

dat_deg_days_trim <- dat_deg_days %>%
  select(date, watershed, TempC, Year, sum_degree_days, sum_emergence, DOY)

peak_deg_days <- left_join(peak_dates_56, dat_deg_days_trim,
                           by = c("watershed", "Year", "date"))

#### Plot ####

# First, trying a plot to demonstrate variation in peak emergence dates
# and cumulative degree days at time of peak emergence.
early_peak_deg_days <- peak_deg_days %>%
  filter(period == "early")

(fig_degday1 <- ggplot(early_peak_deg_days) +
  geom_vline(aes(xintercept = peak_DOY, 
                 color = factor(Year),
                 linetype = watershed),
             linewidth = 1) +
  geom_hline(aes(yintercept = sum_degree_days, color = factor(Year),
                 linetype = watershed),
             linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Peak Emergence (DOY)",
       y = "Cumulative Degree Days",
       color = "Year",
       linetype = "Watershed") +
  theme_bw() +
  theme(text = element_text(size = 20))) # EEK

# Trying a similar plot but with points instead.
(fig_degday2 <- ggplot(early_peak_deg_days,
                       aes(x = sum_degree_days, y = peak_DOY,
                           color = factor(Year), shape = factor(watershed))) +
    geom_point(size = 10, alpha = 0.75) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = "Cumulative Degree Days",
         y = "Peak Emergence (DOY)",
         color = "Year",
         shape = "Watershed") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# And add summary plots of cumulative degree days in either watershed.
dat_deg_days_summary <- dat_deg_days %>%
  group_by(watershed, DOY) %>%
  summarize(sum_deg_days_0.025 = quantile(sum_degree_days, probs = 0.025),
            sum_deg_days_0.50 = median(sum_degree_days),
            sum_deg_days_0.975 = quantile(sum_degree_days, probs = 0.975),
            sum_emerge_0.025 = quantile(sum_emergence, probs = 0.025),
            sum_emerge_0.50 = median(sum_emergence),
            sum_emerge_0.975 = quantile(sum_emergence, probs = 0.975)) %>%
  ungroup()

(fig_degday3 <- ggplot(dat_deg_days_summary,
                       aes(x = DOY)) +
    geom_line(aes(y = sum_deg_days_0.50), 
              linewidth = 1, color = "#F28705") +
    geom_ribbon(aes(ymin = sum_deg_days_0.025,
                    ymax = sum_deg_days_0.975),
                fill = "#F28705",
                alpha = 0.2) +
    labs(x = "DOY",
         y = "Cumulative Degree Days") +
    facet_grid(watershed~.) +
    theme_bw() +
    theme(text = element_text(size = 20)))

(fig_degday4 <- ggplot(dat_deg_days_summary,
                       aes(x = DOY)) +
    geom_line(aes(y = sum_emerge_0.50),
              linewidth = 1) +
    geom_ribbon(aes(ymin = sum_emerge_0.025,
                    ymax = sum_emerge_0.975),
                alpha = 0.2) +
    labs(x = "DOY",
         y = "Cumulative Emergence") +
    facet_grid(watershed~.) +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Combine these three to describe phenology of degree days and emergence.
(fig_degday_full <- (fig_degday3 + fig_degday4 + fig_degday2 +
                      plot_annotation(tag_levels = "A")))

# Export figure.
# ggsave(plot = fig_degday_full,
#        filename = "figures/sum_emerge_degdays_082725.jpg",
#        width = 60,
#        height = 18,
#        units = "cm")

#### Statistics ####

# Examining difference in 5 & 6 for peak dates and cumulative degree days.
doy_stats <- early_peak_deg_days %>%
  select(watershed, Year, peak_DOY) %>%
  pivot_wider(names_from = watershed, values_from = peak_DOY) %>%
  mutate(diff = `5` - `6`)
dd_stats <- early_peak_deg_days %>%
  select(watershed, Year, sum_degree_days) %>%
  pivot_wider(names_from = watershed, values_from = sum_degree_days) %>%
  mutate(diff = `5` - `6`)

### Examining date of when 11 degrees C is reached historically. ###

# Note, it appears temperature was not routinely measured until WY1965.
chem_trim <- chem %>%
  select(site,date,waterYr,temp) %>%
  # and filtering only for the reference sites in our study
  filter(site %in% c("W3","W6"))

# Add columns to delineate DOY when 11 degree threshold was reached.
chem_trim <- chem_trim %>%
  mutate(DOY = yday(date),
         at_above_11 = case_when(temp >= 11 ~ "Yes",
                                 TRUE ~ "No"))

# And trim this down further to first instances.
first_over <- chem_trim %>%
  filter(at_above_11 == "Yes") %>%
  arrange(date) %>%
  group_by(site, waterYr) %>%
  slice_head() %>%
  ungroup()

# Make a dataset for the first and last 15 years of the record.
first_over_hist15 <- first_over %>%
  filter(waterYr %in% c(1965:1980))

first_over_pres15 <- first_over %>%
  filter(waterYr %in% c(2009:2023))

# And calculate mean dates of reaching 11 degrees.
mean(first_over_hist15$DOY) # 160.7188
sd(first_over_hist15$DOY) # 8.160623

mean(first_over_pres15$DOY) # 161.4
sd(first_over_pres15$DOY) # 6.970875

### Examining maximum temperatures in March for the same periods. ###

# Trim the dataset created above to maximum temperatures in March.
spring_max <- chem_trim %>%
  mutate(month = month(date)) %>%
  filter(month %in% c(3,4,5)) %>%
  arrange(date) %>%
  group_by(site, waterYr, month) %>%
  slice_max(temp, na_rm = TRUE) %>%
  ungroup()

# Make datasets for the first and last 15 years of the record.
spring_max_hist15 <- spring_max %>%
  filter(waterYr %in% c(1965:1980))

spring_max_pres15 <- spring_max %>%
  filter(waterYr %in% c(2009:2023))

# And calculate mean maximum temperatures for each month.
spring_max_hist15_summary <- spring_max_hist15 %>%
  group_by(month) %>%
  summarize(mean_max = mean(temp, na.rm = TRUE),
            sd_max = sd(temp, na.rm = TRUE)) %>%
  mutate(period = "historic")

spring_max_pres15_summary <- spring_max_pres15 %>%
  group_by(month) %>%
  summarize(mean_max = mean(temp, na.rm = TRUE),
            sd_max = sd(temp, na.rm = TRUE)) %>%
  mutate(period = "present")

# Join the datasets.
spring_max_summary <- rbind(spring_max_hist15_summary,
                            spring_max_pres15_summary)

# Plotting for easier visualization.
ggplot(spring_max_summary,
       aes(x = factor(month), y = mean_max, color = period)) +
  geom_point(alpha = 0.7,
             position = position_jitter(width = 0.2, seed = 123)) +
  geom_linerange(aes(ymin = mean_max - sd_max,
                     ymax = mean_max + sd_max),
                 position = position_jitter(width = 0.2, seed = 123)) +
  theme_bw()

# End of script.
