### Time Series Figures of Emergence Alongside Conditions
### August 6, 2025
### Heili Lowman

#### README ####

# The following script will create figures to showcase the
# most/least productive years alongside concurrent stream
# conditions.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)
library(lubridate)
library(scales)

# Load data.
insect_dat <- readRDS("data_working/aquatic_counts_complete_yrs_081425.rds")
stream_dat <- readRDS("data_working/stream_climate_qchem.rds")

#### Tidy ####

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

# Also, need to calculate flow duration curves by year.
flow_dat <- stream_dat %>%
  select(WS, DATE, Streamflow) %>%
  mutate(Year = year(DATE))

curves5 <- data.frame()

for(i in c(2017:2024)){
  yearly_curve <- flow_dat %>%
    filter(WS == 5) %>%
    filter(Year == i) %>%
    arrange(desc(Streamflow)) %>%
    mutate(rank = row_number(),
           exceedance_perc = (rank / (n() + 1)) * 100)
    
  curves5 <- rbind(curves5, yearly_curve)
}

#### Plot ####

##### Fig 1 #####

# First figure will compare W5 & W6 in 2019
# to demonstrate variation among watersheds in a given year

# I will initially make a revised dataset that I can use
# to plot the previous year and the current year's data
# simultaneously. - NB: Chose not to go this route because
# the figure gets way too messy.

# dat_2019 <- dat_all %>%
#   filter(DATE < "2020-01-01" &
#            DATE > "2018-12-31") %>%
#   mutate(group = "present")
# 
# dat_2018 <- dat_all %>%
#   filter(DATE < "2019-01-01" &
#            DATE > "2017-12-31") %>%
#   mutate(DATE_plus1 = DATE + years(1)) %>%
#   select(-DATE) %>%
#   rename(DATE = DATE_plus1) %>%
#   mutate(group = "minus1yr")
# 
# dat_manip <- full_join(dat_2019, dat_2018)

(fig_compare1a <- ggplot(dat_all %>%
                          filter(watershed == "5") %>%
                          filter(Date < "2020-01-01" &
                                Date > "2018-12-31") %>%
                          drop_na(total_count), 
                         aes(x = Date, y = total_count)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%b-%Y"),
               limits = as.Date(c('2019-01-01','2020-01-01'))) +
  ylim(0, 4000) +
  labs(y = "Weekly Emergence",
       title = "Watershed 5") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 20),
        legend.position = "none"))

(fig_compare1b <- ggplot(dat_all %>%
                           filter(watershed == "6") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(total_count), 
                         aes(x = Date, y = total_count)) +
    geom_line(linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 4000) +
    labs(title = "Watershed 6") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1c <- ggplot(dat_all %>%
                           filter(watershed == "5") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31"),
                         aes(x = Date, y = Streamflow)) +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 80) +
    labs(y = "Discharge (mm/day)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1d <- ggplot(dat_all %>%
                           filter(watershed == "6") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31"), 
                         aes(x = Date, y = Streamflow)) +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 80) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1e <- ggplot(dat_all %>%
                           filter(watershed == "5") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(temp), 
                         aes(x = Date, y = temp)) +
    geom_line(color = "#D95F02", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 20) +
    labs(y = "Temperature (°C)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1f <- ggplot(dat_all %>%
                           filter(watershed == "6") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(temp), 
                         aes(x = Date, y = temp)) +
    geom_line(color = "#D95F02", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 20) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1g <- ggplot(dat_all %>%
                           filter(watershed == "5") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(chla_T), 
                         aes(x = Date, y = chla_T)) +
    geom_line(color = "#66A61E", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 10) +
    labs(x = "Date", y = "Chlorophyll a (mg/L)") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1h <- ggplot(dat_all %>%
                           filter(watershed == "6") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(chla_T), 
                         aes(x = Date, y = chla_T)) +
    geom_line(color = "#66A61E", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 10) +
    labs(x = "Date") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

# Pulling the first figure together.
(fig_compare1 <- (fig_compare1a + fig_compare1b) /
    (fig_compare1c + fig_compare1d) /
    (fig_compare1e + fig_compare1f) /
    (fig_compare1g + fig_compare1h))

# Export figure.
# ggsave(plot = fig_compare1,
#        filename = "figures/w5_w6_2019_081425.jpg",
#        width = 30,
#        height = 30,
#        units = "cm")

##### Fig 2 #####

# Second figure will demonstrate variation within a watershed across years.
# This will instead present 2020-2022 for Watershed 5 alone.

# In order to have the lines plot appropriately, I will add a column to
# group by year.
dat_all <- dat_all %>%
  mutate(Year = year(Date)) %>%
  mutate(month = month(Date))

(fig_compare2a <- ggplot(dat_all %>%
                           filter(watershed == "5") %>%
                           filter(Date < "2023-01-01" &
                                    Date > "2019-12-31") %>%
                           drop_na(total_count), 
                         aes(x = Date, y = total_count,
                             group = Year)) +
    geom_line(linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Weekly Emergence") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare2b <- ggplot(dat_all %>%
                           filter(watershed == "5") %>%
                           filter(Date < "2023-01-01" &
                                    Date > "2019-12-31"), 
                         aes(x = Date, y = Streamflow)) +
    geom_hline(aes(yintercept = 0.0440),
               color = "#7570B3", linewidth = 0.75) + # 10%tile historical flow
    # geom_hline(aes(yintercept = 1.128),
    #            color = "#375377", linewidth = 0.75) + # median historical flow
    geom_hline(aes(yintercept = 22.82592),
               color = "#8DA0CB", linewidth = 0.75) + # 99%tile historical flow
    annotate("text", x = ymd("2022-09-01"), y = 10, size = 8,
             label = "10th", color = "#7570B3") +
    # annotate("text", x = ymd("2022-09-01"), y = 4, 
    #          label = "50th", color = "#375377") +
    annotate("text", x = ymd("2022-09-01"), y = 32, size = 8,
             label = "99th", color = "#8DA0CB") +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Q (mm/day)") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Quickly make a separate dataset with which to calculate cumulative low flow days.
dat_w5 <- dat_all %>%
  filter(watershed == "5") %>%
  mutate(year = year(Date),
         low_flow_days = ifelse(Streamflow <= 0.0440, 1, 0)) %>%
  arrange(Date) %>%
  group_by(year) %>%
  mutate(sum_low_flow_days = cumsum(low_flow_days)) %>%
  ungroup()

# Figure for cumulative plots of low flow days.
(fig_compare2c <- ggplot(dat_w5 %>%
                           filter(Date < "2023-01-01" &
                                    Date > "2019-12-31"), 
                         aes(x = Date, y = sum_low_flow_days,
                             group = year)) +
    geom_line(color = "#7570B3", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Cumulative\nLow Flow Days",
         x = "Date") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Figure with flow duration curves by year.
(fig_compare_fd <- ggplot(curves5 %>%
                            filter(DATE < "2023-01-01" &
                                     DATE > "2019-12-31"), 
                          aes(x = exceedance_perc, 
                              y = Streamflow)) +
    geom_line(color = "#7570B3", linewidth = 1) +
    #scale_y_log10(breaks=c(.01,.1,1,10,100),labels=c(.01,.1,1,10,100)) +
    labs(x = "Percent Time Flow Exceeded",
         y = "Q (mm/day)") +
    facet_grid(.~Year) +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Pulling the first figure together.
(fig_compare2 <- (fig_compare2a) /
    (fig_compare2b) /
    (fig_compare_fd) ) +
  plot_annotation(tag_levels = "A")

# Export figure.
# ggsave(plot = fig_compare2,
#        filename = "figures/w5_20_21_22_100125.jpg",
#        width = 45,
#        height = 25,
#        units = "cm")

# And creating a similar plot for W5's full record.
(fig_compare3a <- ggplot(dat_w5 %>%
                           filter(Date < "2024-01-01" &
                                    Date > "2017-12-31") %>%
                           drop_na(total_count), 
                         aes(x = Date, y = total_count,
                             color = period,
                             group = Year)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("black", "gray70")) +
    scale_x_date(date_breaks = "6 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2018-01-01','2024-01-01'))) +
    labs(y = "Weekly Emergence") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare3b <- ggplot(dat_w5 %>%
                           filter(Date < "2024-01-01" &
                                    Date > "2017-12-31"), 
                         aes(x = Date, y = sum_low_flow_days,
                             group = year)) +
    geom_line(color = "#7570B3", linewidth = 1) +
    scale_x_date(date_breaks = "6 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2018-01-01','2024-01-01'))) +
    labs(y = "Cumulative\nLow Flow Days",
         x = "Date") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Pulling the second figure together.
(fig_compare3 <- (fig_compare3a) /
    (fig_compare3b))

# Export figure.
# ggsave(plot = fig_compare3,
#        filename = "figures/w5_18_thru_24_090125.jpg",
#        width = 45,
#        height = 15,
#        units = "cm")

#### Temp at Peak Emergence ####

# Slice entire dataset to include only peaks by year and site.
dat_early_peaks <- dat_all %>%
  filter(month < 10) %>%
  group_by(watershed, year) %>%
  slice_max(total_count, na_rm = TRUE) %>%
  ungroup() %>%
  mutate(keep = case_when(duplicate == "Dup" ~ "No",
                          watershed == 1 & year %in% c(2018,2019,2020) |
                          watershed == 2 & year %in% c(2018,2019,2020) |
                          watershed == 3 & year %in% c(2018,2019,2020) |
                          watershed == 4 & year %in% c(2018,2019,2020) |
                          watershed == 5 & year %in% c(2018,2019, 2020,
                                                  2021, 2022, 2023, 2024) |
                          watershed == 6 & year %in% c(2018,2019, 2020,
                                                  2021, 2022, 2023, 2024) |
                          watershed == 9 & year %in% c(2018,2019,2020) ~ "Yes",
                          TRUE ~ NA)) %>%
  filter(keep == "Yes") %>%
  drop_na(keep)

mean(dat_early_peaks$temp, na.rm = TRUE) # 10.93
sd(dat_early_peaks$temp, na.rm = TRUE) # 1.79

# And finally, generate a figure to add to the supplement.
# Note, this drops 52 records due to lack of data available
# for WY24 in 5&6 at this time.
(fig_about_peak <- ggplot(dat_all %>% 
                           mutate(keep = case_when(duplicate == "Dup" ~ "No",
                                                   watershed == 1 & year %in% c(2018,2019,2020) |
                                                     watershed == 2 & year %in% c(2018,2019,2020) |
                                                     watershed == 3 & year %in% c(2018,2019,2020) |
                                                     watershed == 4 & year %in% c(2018,2019,2020) |
                                                     watershed == 5 & year %in% c(2018,2019, 2020,
                                                                                  2021, 2022, 2023, 2024) |
                                                     watershed == 6 & year %in% c(2018,2019, 2020,
                                                                                  2021, 2022, 2023, 2024) |
                                                     watershed == 9 & year %in% c(2018,2019,2020) ~ "Yes",
                                                   TRUE ~ NA)) %>%
                           filter(keep == "Yes"),
                         aes(x = temp, y = total_count)) +
  geom_point(shape = 21, size = 3) +
  labs(x = "Daytime Temperature (°C)",
       y = "Weekly Aquatic Diptera Emergence") +
  theme_bw() +
  theme(text = element_text(size = 20)))

# Export figure.
# ggsave(plot = fig_about_peak,
#        filename = "figures/temp_about_peak_091625.jpg",
#        width = 16,
#        height = 14,
#        units = "cm")

# End of script.
