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

# Load data.
dat <- read_csv("data_raw/sticky_trap_counts_051724.csv")

#### Tidy ####

# First, need to correct an incorrect date.
dat[1393, 4] <- "2020-08-03"

# And quickly check the data structure.
str(dat)

# Next, need to pivot to make into long format.
dat_long <- dat %>%
  pivot_longer(cols = dipteran_large:other_small,
               names_to = "id",
               values_to = "count")

# Also need to add additional columns to plot more easily.
dat_long <- dat_long %>%
  # create formatted date column
  mutate(Date = ymd(date)) %>%
  # create separate order column
  mutate(Order = factor(case_when(id %in% c("dipteran_large",
                                            "dipteran_small") ~ "dipteran",
                                  id %in% c("terrestrial_large",
                                            "terrestrial_small") ~ "terrestrial",
                                  id %in% c("caddisfly_large",
                                            "caddisfly_small") ~ "caddisfly",
                                  # note, there aren't any mayfly_small currently
                                  id %in% c("mayfly_large",
                                            "mayfly_small") ~ "mayfly",
                                  # also no stonefly_small currently
                                  id %in% c("stonefly_large",
                                            "stonefly_small") ~ "stonefly",
                                  id %in% c("other_large",
                                            "other_small") ~ "other"),
                        levels = c("dipteran", "mayfly", 
                                   "stonefly", "caddisfly",
                                   "terrestrial", "other"))) %>%
  # create separate size column
  mutate(Size = case_when(id %in% c("dipteran_large",
                                    "terrestrial_large",
                                    "caddisfly_large",
                                    "mayfly_large",
                                    "stonefly_large",
                                    "other_large") ~ "large",
                          id %in% c("dipteran_small",
                                    "terrestrial_small",
                                    "caddisfly_small",
                                    "mayfly_small",
                                    "stonefly_small",
                                    "other_small") ~ "small"))

# And create a dataset of total weekly emergence,
# including ONLY aquatic taxa.
dat_total_weekly <- dat_long %>%
  filter(Order %in% c("caddisfly", "dipteran",
                      "mayfly", "stonefly")) %>%
  group_by(watershed, Date) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  # dropping data with missing dates for now
  drop_na(Date)

#### Warming Peak Emergence ####

# Calculate peak emergence by watershed and by year (single week).

# Add year column to dataset.
dat_total_weekly$Year <- year(dat_total_weekly$Date)

dat_peak <- dat_total_weekly %>%
  group_by(watershed, Year) %>%
  slice(which.max(total_count)) %>%
  mutate(jday = yday(Date)) %>%
  ungroup()

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
#         "data_working/peak_emerge_dates_052824.rds")

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
#         "data_working/peak_emerge_3wk_060424.rds")
  
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
#         "data_working/peak_emerge_dates_cooling_052824.rds")

# And reformat for easier comparison.
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
gtsave(data = table1,
       filename = "figures/peak_emerge_table_040824.png")

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
gtsave(data = table2,
       filename = "figures/peak_date_table_040824.png")

#### Total Emergence ####

# Calculate total emergence by watershed and by year.

dat_sum <- dat_total_weekly %>%
  group_by(watershed, Year) %>%
  summarize(annual_count = sum(total_count, 
                               na.rm = TRUE)) %>%
  ungroup()

# Export for use in analyses.
# saveRDS(dat_sum,
#         "data_working/sum_emerge_052824.rds")

# And reformat for easier comparison.
# Number of individuals
dat_sum_wide <- dat_sum %>%
  pivot_wider(names_from = watershed, 
              values_from = annual_count)

(table3 <- dat_sum_wide %>%
    gt(rowname_col = "Year",
       groupname_col = NA) %>%
    tab_spanner(label = "Watershed", 
                columns = `1`:`HBK`) %>%
    tab_header(
      title = md("Total Emergence (individuals)")))

# Export table.
gtsave(data = table3,
       filename = "figures/annual_emerge_table_040824.png")

#### Duration ####

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

# And finally calculate the 5th and 95th percentiles to
# estimate emergence duration.
dat_duration <- dat_total_weekly %>%
  # create new julian day column
  mutate(jday = yday(Date)) %>%
  # creates a new column that assigns each observation to a group
  mutate(perc_group = factor(case_when(percentile < 0.05 ~ "<0.05",
                                       percentile >= 0.05 & 
                                         percentile < 0.95 ~ "0.05",
                                       percentile >= 0.95 ~ "0.95"),
                             levels = c("<0.05", "0.05", "0.95"))) %>%
  # and now I want to pull out the first instance in each group
  group_by(watershed, Year, perc_group) %>%
  slice(which.min(jday)) %>%
  ungroup()

# And widen for plotting.
dat_duration_wide <- dat_duration %>%
  select(watershed, Year, perc_group, jday) %>%
  pivot_wider(names_from = perc_group, values_from = jday) %>%
  select(watershed, Year, `0.05`, `0.95`) %>%
  mutate(year = factor(Year)) %>%
  mutate(duration = `0.95` - `0.05`)

# Plot.
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
#        filename = "figures/duration_emerge_041124.jpg",
#        width = 10,
#        height = 20,
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

# End of script.
