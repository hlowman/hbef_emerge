### Emergence Data Indices
### April 8, 2024
### Heili Lowman

#### README ####

# The following script will calculate emergence indices using
# the HBEF aquatic insect emergence data.

# Data was downloaded from https://hbwater.org/restricted_QAQC/
# on May 17, 2024.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(viridis)
library(gt)
library(webshot2)
library(patchwork)

# Load data.
dat <- readRDS("data_working/aquatic_counts_long_051325.rds")
stream_dat <- read_csv("data_raw/HBEFdata_Current_2025-05-13.csv")

# Hex values for later use
"#3900B3", "#5524BD", "#7148C8", "#8D6DD3", "#AA91DE", "#C6B6E9"

#### Tidy ####

# Create a dataset of total weekly emergence for all aquatic taxa.
dat_total_weekly <- dat %>%
  group_by(watershed, Date) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

# Create same dataset, but for stoneflies only.
dat_total_weekly_sf <- dat %>%
  filter(Order %in% c("stonefly")) %>%
  group_by(watershed, Date) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

# And one for caddisflies only.
dat_total_weekly_cf <- dat %>%
  filter(Order %in% c("caddisfly")) %>%
  group_by(watershed, Date) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

#### Peak Emergence ####

# Calculate peak emergence by watershed and by year (single week).

# Add year column to datasets.
dat_total_weekly$Year <- year(dat_total_weekly$Date)
dat_total_weekly_sf$Year <- year(dat_total_weekly_sf$Date)
dat_total_weekly_cf$Year <- year(dat_total_weekly_cf$Date)

dat_peak <- dat_total_weekly %>%
  group_by(watershed, Year) %>%
  slice(which.max(total_count)) %>%
  mutate(jday = yday(Date)) %>%
  ungroup()

dat_peak_sf <- dat_total_weekly_sf %>%
  group_by(watershed, Year) %>%
  slice(which.max(total_count)) %>%
  mutate(jday = yday(Date)) %>%
  ungroup()

dat_peak_cf <- dat_total_weekly_cf %>%
  group_by(watershed, Year) %>%
  slice(which.max(total_count)) %>%
  mutate(jday = yday(Date)) %>%
  ungroup()

# Export for use in seasonal timetable.
# Only using peak dates for stoneflies & caddisflies 
# since we anticipate they are univoltine.
# saveRDS(dat_peak_sf,
#         "data_working/peak_emerge_sf_dates_051325.rds")
# saveRDS(dat_peak_cf,
#         "data_working/peak_emerge_cf_dates_051325.rds")

##### Warming #####

# Create peak emergence dataset specific to summer
# peak dates.
dat_peak_summer <- dat_total_weekly %>%
  mutate(month = month(Date)) %>%
  filter(month < 9) %>%
  group_by(watershed, Year) %>%
  slice(which.max(total_count)) %>%
  mutate(jday = yday(Date)) %>%
  ungroup()

# Export for use in seasonal timetable.
# saveRDS(dat_peak_summer,
#         "data_working/warm_peak_emerge_dates_051325.rds")

# Also, going to create peak emergence with a week on either side.
# Select columns of interest from peak dataset.
dat_peak_trim <- dat_peak %>%
  rename(Date_peak = Date) %>%
  select(watershed, Date_peak, Year)

dat_peak_3wk <- left_join(dat_total_weekly, dat_peak_trim) %>%
  mutate(peak_binary = case_when(Date == Date_peak ~ 1,
                                 TRUE ~ 0)) %>%
  group_by(watershed, Year) %>%
  mutate(peak_lag = peak_binary - lag(peak_binary)) %>%
  mutate(peak_lead = peak_binary - lead(peak_binary)) %>%
  ungroup() %>%
  filter(peak_lag > 0 |
           peak_lag < 0 |
           peak_lead > 0 |
           peak_lead < 0) %>%
  group_by(watershed, Year) %>%
  summarize(sum_total_3wk = sum(total_count, na.rm = TRUE)) %>%
  ungroup()

# Export for use in seasonal timetable.
# saveRDS(dat_peak_3wk,
#         "data_working/warm_peak_emerge_3wk_051325.rds")

##### Cooling #####

# Create peak emergence dataset specific to fall
# peak dates.
dat_peak_fall <- dat_total_weekly %>%
  mutate(month = month(Date)) %>%
  filter(month > 8) %>%
  group_by(watershed, Year) %>%
  slice(which.max(total_count)) %>%
  mutate(jday = yday(Date)) %>%
  ungroup()

# Export for use in seasonal timetable.
# saveRDS(dat_peak_fall,
#         "data_working/cool_peak_emerge_dates_051325.rds")

##### Table #####

# Number of individuals
dat_peak_no <- dat_peak %>%
  select(watershed, Year, total_count) %>%
  pivot_wider(names_from = watershed, values_from = total_count)

(table1 <- dat_peak_no %>%
    gt(rowname_col = "Year",
       groupname_col = NA) %>%
    tab_spanner(label = "Watershed", 
                columns = `1`:`HBK`) %>%
  tab_header(
    title = md("Peak Emergence (individuals)")))

# Export table.
# gtsave(data = table1,
#        filename = "figures/peak_emerge_table_051325.png")

# Date of peak
dat_peak_date <- dat_peak %>%
  mutate(Day = day(Date)) %>%
  mutate(Month = month(Date)) %>%
  unite(DayMonth, c("Month", "Day"), sep = "-") %>%
  select(watershed, Year, DayMonth) %>%
  pivot_wider(names_from = watershed, 
              values_from = DayMonth)

(table2 <- dat_peak_date %>%
    gt(rowname_col = "Year",
       groupname_col = NA) %>%
    tab_spanner(label = "Watershed", 
                columns = `1`:`HBK`) %>%
    tab_header(
      title = md("Date of Peak Emergence")))

# Export table.
# gtsave(data = table2,
#        filename = "figures/peak_date_table_051325.png")

##### Plot #####

# Create figure to display variance in peaks across years and watersheds.
(fig1 <- ggplot(dat_peak,
               aes(x = jday,
                   y = watershed,
                   fill = factor(Year))) +
   geom_point(size = 6, shape = 21, alpha = 0.75) +
   scale_fill_manual(values = c("black", "#3900B3", "#7148C8", 
                                "#8D6DD3", "#AA91DE", "white")) +
   xlim(100, 325) +
   labs(y = "Watershed",
        x = "Day of Year",
        fill = "Year",
        title = "Peak Emergence") +
   theme_bw() +
   theme(text = element_text(size = 16),
         plot.title = element_text(hjust = 0.5),
         legend.position = "none"))

#### Total Emergence ####

# Calculate total emergence by watershed and by year.

dat_sum <- dat_total_weekly %>%
  group_by(watershed, Year) %>%
  summarize(annual_count = sum(total_count, 
                               na.rm = TRUE)) %>%
  ungroup()

# Export for use in analyses.
# saveRDS(dat_sum,
#         "data_working/sum_emerge_051325.rds")

# And do the same for only the stonefly data.
dat_sum_sf <- dat_total_weekly_sf %>%
  group_by(watershed, Year) %>%
  summarize(annual_count = sum(total_count, 
                               na.rm = TRUE)) %>%
  ungroup()

# Export for use in analyses.
# saveRDS(dat_sum_sf,
#         "data_working/sum_emerge_sf_051325.rds")

# As well as caddisfly data.
dat_sum_cf <- dat_total_weekly_cf %>%
  group_by(watershed, Year) %>%
  summarize(annual_count = sum(total_count, 
                               na.rm = TRUE)) %>%
  ungroup()

# Export for use in analyses.
# saveRDS(dat_sum_cf,
#         "data_working/sum_emerge_cf_051325.rds")

#### Percentiles ####

# Add running (cumulative) total to data.
dat_total_weekly <- dat_total_weekly %>%
  group_by(watershed, Year) %>%
  mutate(running_total = cumsum(replace_na(total_count, 0))) %>%
  ungroup()

# Pull out maximum count data.
dat_max <- dat_total_weekly %>%
  group_by(watershed, Year) %>%
  slice(which.max(running_total)) %>%
  ungroup() %>%
  rename(sum_total = running_total) %>%
  select(watershed, Year, sum_total)

# And add to original dataset.
dat_total_weekly <- left_join(dat_total_weekly, dat_max) %>%
  mutate(percentile = running_total/sum_total)

# And finally calculate the 5th, 50th, and 95th percentiles to
# estimate emergence duration.
dat_duration <- dat_total_weekly %>%
  # create new julian day column
  mutate(jday = yday(Date)) %>%
  # creates a new column that assigns each observation to a group
  mutate(perc_group = factor(case_when(percentile < 0.05 ~ "<0.05",
                                       percentile >= 0.05 & 
                                         percentile < 0.5 ~ "0.05",
                                       percentile >= 0.5 & 
                                         percentile < 0.95 ~ "0.5",
                                       percentile >= 0.95 ~ "0.95"),
                             levels = c("<0.05", "0.05", "0.5", "0.95"))) %>%
  # and now I want to pull out the first instance in each group
  group_by(watershed, Year, perc_group) %>%
  slice(which.min(jday)) %>%
  ungroup()

# And widen for plotting.
dat_duration_wide <- dat_duration %>%
  select(watershed, Year, perc_group, jday) %>%
  pivot_wider(names_from = perc_group, values_from = jday) %>%
  select(watershed, Year, `0.05`, `0.5`, `0.95`) %>%
  mutate(year = factor(Year)) %>%
  mutate(duration = `0.95` - `0.05`)

##### Plot #####

(fig_duration <- ggplot(dat_duration_wide,
                        aes(y = year)) +
    geom_linerange(aes(xmin = `0.05`, xmax = `0.95`)) +
    geom_point(aes(x = `0.05`), size = 3,
               shape = 21, fill = "#B5C861") +
    geom_point(aes(x = `0.95`), size = 3,
               shape = 21, fill = "#976153") +
    labs(x = "DOY", y = "Year") +
    theme_bw() +
    facet_grid(watershed~., scales = "free"))

# Export figure.
# ggsave(plot = fig_duration,
#        filename = "figures/duration_emerge_051325.jpg",
#        width = 10,
#        height = 20,
#        units = "cm")

# Create figure to display variance in medians across years and watersheds.
(fig2 <- ggplot(dat_duration_wide,
                aes(x = `0.5`,
                    y = watershed,
                    fill = factor(Year))) +
    geom_point(size = 6, shape = 21, alpha = 0.75) +
    scale_fill_manual(values = c("black", "#3900B3", "#7148C8", 
                                 "#8D6DD3", "#AA91DE", "white")) +
    xlim(100, 325) +
    labs(y = "Watershed",
         x = "Day of Year",
         fill = "Year",
         title = "50% Emergence") +
    theme_bw() +
    theme(text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5)))

(fig_1_plus_2 <- fig1 + fig2 +
    plot_annotation(tag_levels = "a"))

# ggsave(plot = fig_1_plus_2,
#        filename = "figures/peak_median_emerge_051325.jpg",
#        width = 40,
#        height = 17,
#        units = "cm")

#### Temperature on Peaks ####

# Trim down original temperature dataset.
stream_temps <- stream_dat %>%
  filter(site %in% c("W1", "W2", "W3", "W4", "W5",
                     "W6", "W9", "HBK")) %>%
  mutate(Date = mdy(date)) %>%
  mutate(watershed = case_when(site == "W1" ~ "1",
                               site == "W2" ~ "2",
                               site == "W3" ~ "3",
                               site == "W4" ~ "4",
                               site == "W5" ~ "5",
                               site == "W6" ~ "6",
                               site == "W9" ~ "9",
                               site == "HBK" ~ "HBK")) %>%
  select(watershed, Date, temp)

# And join with all peak datasets created above.
dat_peak_summer <- left_join(dat_peak_summer, stream_temps, 
                      by = c("watershed", "Date")) %>%
  rename(temp_peak_warming = temp)

dat_peak_fall <- left_join(dat_peak_fall, stream_temps, 
                             by = c("watershed", "Date")) %>%
  rename(temp_peak_cooling = temp)

dat_peak_cf <- left_join(dat_peak_cf, stream_temps, 
                      by = c("watershed", "Date")) %>%
  rename(temp_peak_cf = temp)

dat_peak_sf <- left_join(dat_peak_sf, stream_temps, 
                         by = c("watershed", "Date")) %>%
  rename(temp_peak_sf = temp)

# Quick plots of temperatures of emergence.
(fig_temp1 <- ggplot(dat_peak_summer, aes(x = watershed,
                                          y = temp_peak_warming,
                                          fill = watershed)) +
    geom_boxplot(alpha = 0.8) +
    geom_jitter(alpha = 0.8) +
    labs(x = "Watershed", y = "Peak Warming Emergence Temperature (C)") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw() +
    theme(legend.position = "none"))

(fig_temp2 <- ggplot(dat_peak_fall, aes(x = watershed,
                                          y = temp_peak_cooling,
                                          fill = watershed)) +
    geom_boxplot(alpha = 0.8) +
    geom_jitter(alpha = 0.8) +
    labs(x = "Watershed", y = "Peak Cooling Emergence Temperature (C)") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw() +
    theme(legend.position = "none"))

(fig_temp3 <- ggplot(dat_peak_cf, aes(x = watershed,
                                        y = temp_peak_cf,
                                        fill = watershed)) +
    geom_boxplot(alpha = 0.8) +
    geom_jitter(alpha = 0.8) +
    labs(x = "Watershed", y = "Peak Caddisfly Emergence Temperature (C)") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw() +
    theme(legend.position = "none"))

(fig_temp4 <- ggplot(dat_peak_sf, aes(x = watershed,
                                      y = temp_peak_sf,
                                      fill = watershed)) +
    geom_boxplot(alpha = 0.8) +
    geom_jitter(alpha = 0.8) +
    labs(x = "Watershed", y = "Peak Stonefly Emergence Temperature (C)") +
    scale_fill_viridis(discrete = TRUE, option = "magma") +
    theme_bw() +
    theme(legend.position = "none"))

# Combine into a single plot.
(fig_all_temps <- (fig_temp1 | fig_temp2) /
    (fig_temp3 | fig_temp4))

# Export figure.
# ggsave(plot = fig_all_temps,
#        filename = "figures/emerge_temperatures_071624.jpg",
#        width = 30,
#        height = 25,
#        units = "cm")

#### Join indices ####

dat_peak <- dat_peak %>%
  rename(Date_peak = Date,
         total_count_peak = total_count,
         jday_peak = jday)

dat_indices <- full_join(dat_peak, dat_sum,
                         by = c("watershed",
                                "Year"))

dat_indices <- full_join(dat_indices, dat_duration_wide,
                         by = c("watershed",
                                "Year"))

# Plot indices versus on another.
(fig1_ind <- ggplot(dat_indices, aes(x = duration,
                                     y = annual_count)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Duration of Emergence (days)",
         y = "Annual Total Emergence (individuals)",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

(fig2_ind <- ggplot(dat_indices, aes(x = duration,
                                     y = jday_peak)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Duration of Emergence (days)",
         y = "Peak Emergence DOY",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

(fig3_ind <- ggplot(dat_indices, aes(x = duration,
                                     y = total_count_peak)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Duration of Emergence (days)",
         y = "Peak Emergence (individuals)",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

(fig4_ind <- ggplot(dat_indices, aes(x = jday_peak,
                                     y = annual_count)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence DOY",
         y = "Annual Total Emergence (individuals)",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

(fig5_ind <- ggplot(dat_indices, aes(x = total_count_peak,
                                     y = annual_count)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence (individuals)",
         y = "Annual Total Emergence (individuals)",
         color = "Watershed") +
    theme_bw())

(fig6_ind <- ggplot(dat_indices, aes(x = jday_peak,
                                     y = total_count_peak)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence DOY",
         y = "Peak Emergence (individuals)",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

# Combine into a single plot.
(fig_all_indices <- (fig1_ind | fig2_ind | fig3_ind) /
  (fig4_ind | fig6_ind | fig5_ind))

# Export figure.
# ggsave(plot = fig_all_indices,
#        filename = "figures/emerge_indices_050224.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

# Hmmm, duration, it seems, may be less of a driver/important metric
# than timing and magnitude of the peak (since this is most strongly
# correlated with annual total emergence).
# There may also be a strong artefact of collection timing to the
# duration window, which may confound findings.

# And, similarly, examine stonefly data.
dat_peak_sf <- dat_peak_sf %>%
  rename(Date_peak = Date,
         total_count_peak = total_count,
         jday_peak = jday)

dat_indices_sf <- full_join(dat_peak_sf, dat_sum_sf,
                         by = c("watershed",
                                "Year"))

(fig1_ind_sf <- ggplot(dat_indices_sf, aes(x = jday_peak,
                                     y = annual_count)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence DOY",
         y = "Annual Total Emergence (individuals)",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

(fig2_ind_sf <- ggplot(dat_indices_sf, aes(x = total_count_peak,
                                     y = annual_count)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence (individuals)",
         y = "Annual Total Emergence (individuals)",
         color = "Watershed") +
    theme_bw())

(fig3_ind_sf <- ggplot(dat_indices_sf, aes(x = jday_peak,
                                     y = total_count_peak)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence DOY",
         y = "Peak Emergence (individuals)",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

# Combine into a single plot.
fig_all_sf_indices <- fig1_ind_sf | fig3_ind_sf | fig2_ind_sf

# Export figure.
# ggsave(plot = fig_all_sf_indices,
#        filename = "figures/emerge_indices_sf_062424.jpg",
#        width = 30,
#        height = 8,
#        units = "cm")

# So the tight correlation between peak and annual emergence
# holds when only examining stoneflies.

# And finally, examine caddisfly data.
dat_peak_cf <- dat_peak_cf %>%
  rename(Date_peak = Date,
         total_count_peak = total_count,
         jday_peak = jday)

dat_indices_cf <- full_join(dat_peak_cf, dat_sum_cf,
                            by = c("watershed",
                                   "Year"))

(fig1_ind_cf <- ggplot(dat_indices_cf, aes(x = jday_peak,
                                           y = annual_count)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence DOY",
         y = "Annual Total Emergence (individuals)",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

(fig2_ind_cf <- ggplot(dat_indices_cf, aes(x = total_count_peak,
                                           y = annual_count)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence (individuals)",
         y = "Annual Total Emergence (individuals)",
         color = "Watershed") +
    theme_bw())

(fig3_ind_cf <- ggplot(dat_indices_cf, aes(x = jday_peak,
                                           y = total_count_peak)) +
    geom_point(aes(color = watershed), size = 3) +
    scale_color_manual(values = cal_palette("figmtn")) +
    labs(x = "Peak Emergence DOY",
         y = "Peak Emergence (individuals)",
         color = "Watershed") +
    theme_bw() +
    theme(legend.position = "none"))

# Combine into a single plot.
fig_all_cf_indices <- fig1_ind_cf | fig3_ind_cf | fig2_ind_cf

# Export figure.
ggsave(plot = fig_all_cf_indices,
       filename = "figures/emerge_indices_cf_062424.jpg",
       width = 30,
       height = 8,
       units = "cm")

# So the tight correlation between peak and annual emergence
# holds when only examining caddisflies as well.

# End of script.
