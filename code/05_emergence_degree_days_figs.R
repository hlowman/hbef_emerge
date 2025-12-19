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
library(nlme)
library(zoo)

# Load temperature data.
temp <- readRDS("data_working/stream_temp_daily_W5_W6_2018_2024.rds")

# Load historical weekly dataset from EDI.
chem <- read_csv("data_raw/HubbardBrook_weekly_stream_chemistry_1963-2024.csv")

# Load emergence data.
emerge_dat <- readRDS("data_working/aquatic_counts_complete_yrs_121925.rds")
peak_dates <- readRDS("data_working/peaks_annual_dipt_emerge_121925.rds")

#### Tidy ####

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

# Pivot temperature data.
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

# Also, calculate total seasonal degree days 
# to investigate a relationship with total emergence
dat_deg_days_ann <- dat_deg_days_trim %>%
  group_by(watershed, Year) %>%
  summarize(sum_ann_dd = max(sum_degree_days),
            sum_ann_emerge = max(sum_emergence)) %>%
  ungroup()

# Write function for extracting slope
slope_fun <- function(d) {
  
  df <- as.data.frame(d)
  
  model <- lm(sum_degree_days ~ DOY, df)
  
  coeff <- coef(model)
  
  slope <- coeff[2]
  
  return(slope)
  
}

# Also, calculate slope changes based on 7-day
# rolling average
dat_deg_days_rolling <- dat_deg_days_trim %>%
  group_by(watershed, Year) %>%
  mutate(slope_7day = rollapplyr(pick(sum_degree_days, DOY),
                                 width = 7,
                                 FUN = slope_fun,
                                 by.column = FALSE,
                                 fill = NA)) %>%
  ungroup()

# And add these to the original datasets for figures below.
dat_deg_days <- full_join(dat_deg_days, dat_deg_days_rolling)
peak_deg_days <- left_join(peak_deg_days, dat_deg_days_rolling)

#### Plot ####

# First, a plot to demonstrate variation 
# cumulative degree days.
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
              linewidth = 1, color = "#3793EC") +
    geom_ribbon(aes(ymin = sum_deg_days_0.025,
                    ymax = sum_deg_days_0.975),
                fill = "#3793EC",
                alpha = 0.2) +
    labs(x = "DOY",
         y = "Cumulative\nDegree Days") +
    facet_grid(.~watershed,
               labeller = labeller(
                 watershed = c('5'="Watershed 5",
                               '6'="Watershed 6"))) +
    theme_bw() +
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          strip.background = element_blank()))

# Need to trim data to appear reasonable given sticky trap
# deployment dates.
dat_deg_days_summary_trim <- dat_deg_days_summary %>%
  filter(DOY > 79) %>% # when values begin to change
  filter(DOY < 350) # when values stop changing

(fig_degday4 <- ggplot(dat_deg_days_summary_trim,
                       aes(x = DOY)) +
    geom_line(aes(y = sum_emerge_0.50),
              linewidth = 1) +
    geom_ribbon(aes(ymin = sum_emerge_0.025,
                    ymax = sum_emerge_0.975),
                alpha = 0.2) +
    xlim(0, 365) +
    labs(x = "DOY",
         y = "Cumulative Annual\nEmergence Count") +
    facet_grid(.~watershed) +
    theme_bw() +
    theme(text = element_text(size = 20),
          strip.background = element_blank(),
          strip.text = element_blank()))

# Combine these two to describe phenology of degree days and emergence.
(fig_degday_full <- (fig_degday3 / fig_degday4 + 
                      plot_annotation(tag_levels = "A")))

# Export figure.
# ggsave(plot = fig_degday_full,
#        filename = "figures/sum_emerge_degdays_121925.jpg",
#        width = 35,
#        height = 20,
#        units = "cm")

#### Statistics ####

##### Week Prior to Peak #####

# Create peak dataset to join with degree day dataset
peak_days <- peak_dates %>%
  filter(watershed %in% c(5,6)) %>%
  rename(Year = year) %>%
  select(watershed, Year, peak_DOY)

# Join with degree days.
peak_weeks <- dat_deg_days %>%
  left_join(peak_days) %>%
  group_by(watershed, Year) %>%
  mutate(peak_week = case_when(DOY %in% c(peak_DOY,
                                          peak_DOY-1,
                                          peak_DOY-2,
                                          peak_DOY-3,
                                          peak_DOY-4,
                                          peak_DOY-5,
                                          peak_DOY-6,
                                          peak_DOY-7) ~ "YES",
                               TRUE ~ "NO")) %>%
  ungroup() %>%
  filter(peak_week == "YES") %>%
  group_by(watershed, Year) %>%
  summarize(mean_temp = mean(TempC),
            sd_temp = sd(TempC)) %>%
  ungroup()

peak_weeks_ws <- dat_deg_days %>%
  left_join(peak_days) %>%
  group_by(watershed, Year) %>%
  mutate(peak_week = case_when(DOY %in% c(peak_DOY,
                                          peak_DOY-1,
                                          peak_DOY-2,
                                          peak_DOY-3,
                                          peak_DOY-4,
                                          peak_DOY-5,
                                          peak_DOY-6,
                                          peak_DOY-7) ~ "YES",
                               TRUE ~ "NO")) %>%
  ungroup() %>%
  filter(peak_week == "YES") %>%
  group_by(watershed) %>%
  summarize(mean_temp = mean(TempC),
            sd_temp = sd(TempC)) %>%
  ungroup()

peak_deg_days <- dat_deg_days %>%
  left_join(peak_days) %>%
  mutate(match = case_when(DOY == peak_DOY ~ "YES",
                           TRUE ~ "NO")) %>%
  filter(match == "YES") %>%
  group_by(watershed) %>%
  summarize(mean_dd = mean(sum_degree_days),
            sd_dd = sd(sum_degree_days)) %>%
  ungroup()

##### Historic Temp #####

# NOTE - not using this due to concerns re:
# comparison to weekly midday temps.

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
(fig_hist <- ggplot(spring_max_summary,
                  aes(x = factor(month), 
                      y = mean_max, 
                      fill = period)) +
    geom_linerange(aes(ymin = mean_max - sd_max,
                       ymax = mean_max + sd_max),
                   position = position_dodge(width = 0.5)) +
  geom_point(shape = 21, size = 5,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = c("black", "white")) +
  labs(x = "Month",
       y = "Mean Maximum Temperature (°C)",
       fill = "Period") +
  theme_bw())

# Export figure.
# ggsave(plot = fig_hist,
#        filename = "figures/spring_temps_111125.jpg",
#        width = 12,
#        height = 10,
#        units = "cm")

# End of script.
