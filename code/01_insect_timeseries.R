### Time Series Data Summary
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data summarizing
# and visualization of the HBEF aquatic insect emergence data.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)
library(lubridate)
library(RColorBrewer)
library(scales)

# Load data.
dat <- readRDS("data_working/aquatic_counts_complete_yrs_081425.rds")

#### Tidy ####

# Summarize by order.
dat_order <- dat %>%
  group_by(watershed, year, Date, Order) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

# Trim data down to only black flies.
dat_dipt <- dat_order %>%
  filter(Order == "dipteran") %>%
  mutate(month = month(Date)) %>%
  # a quick eyeball suggests all peaks happen post October 1 (!)
  mutate(period = case_when(month < 10 ~ "early",
                            TRUE ~ "late"))

# Calculate total annual emergence for black flies vs. EPT
dat_annual_wide <- dat_order %>%
  mutate(group = ifelse(Order == "dipteran", "D", "EPT")) %>%
  group_by(watershed, year, group) %>%
  summarize(sum_total_count = sum(replace_na(total_count,0))) %>%
  ungroup() %>%
  pivot_wider(names_from = group,
              values_from = sum_total_count) %>%
  # and impose manuscript data filters
  mutate(keep = case_when(watershed %in% c("5","6") & 
                            year %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024) |
                            watershed %in% c("1","2", "3","4", "9", "HBK") &
                            year %in% c(2018, 2019) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y")

# In addition to mean annual emergence for all taxa separately...
dat_annual_stats_56 <- dat_order %>%
  group_by(watershed, year, Order) %>%
  summarize(sum_total_count = sum(replace_na(total_count,0))) %>%
  ungroup() %>%
  # and impose W5 & W6 data filters
  mutate(keep = case_when(watershed %in% c("5","6") & 
                            year %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y") %>%
  # and finally summarize by order
  group_by(Order) %>%
  summarize(mean_annual = mean(sum_total_count),
            sd_annual = sd(sum_total_count)) %>%
  ungroup()

# ... and together
dat_annual_stats_all <- dat_order %>%
  group_by(watershed, year) %>%
  summarize(sum_total_count = sum(replace_na(total_count,0))) %>%
  ungroup() %>%
  # and impose manuscript data filters
  mutate(keep = case_when(watershed %in% c("5","6") & 
                            year %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024) |
                            watershed %in% c("1","2", "3","4", "9", "HBK") &
                            year %in% c(2018, 2019, 2020) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y") %>%
  # and finally summarize by WS
  group_by(watershed) %>%
  summarize(mean_annual = mean(sum_total_count),
            sd_annual = sd(sum_total_count)) %>%
  ungroup()

# Calculate total insect emergence
dat_annual_sum <- dat_order %>%
  group_by(watershed, year) %>%
  summarize(sum_total_count = sum(replace_na(total_count, 0))) %>%
  ungroup()

# Calculate total annual black fly emergence
dat_dipt_sum <- dat_dipt %>%
  # and impose manuscript data filters
  mutate(keep = case_when(watershed %in% c("5","6") &
                            year %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024) |
                            watershed %in% c("1","2", "3","4", "9", "HBK") &
                            year %in% c(2018, 2019, 2020) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y") %>%
  group_by(watershed, year) %>%
  summarize(sum_total_count = sum(replace_na(total_count, 0))) %>%
  ungroup()

# Export for use in future scripts.
# saveRDS(dat_dipt_sum, "data_working/sum_annual_dipt_emerge_091225.rds")

# Difference between largest and smallest sites in 2018
11442/1807 # 633%
# And in 2019
20733/3992 # 519%

# Summary statistics of annual emergence.
dat_dipt_stat <- dat_dipt_sum %>%
  group_by(watershed, period) %>%
  summarize(mean_total = mean(sum_total_count),
            sd_total = sd(sum_total_count)) %>%
  ungroup()

# Calculate weekly emergence
dat_dipt_weekly <- dat_dipt %>%
  group_by(watershed, year) %>%
  summarize(mean_weekly = mean(total_count, na.rm = TRUE),
            sd_weekly = sd(total_count, na.rm = TRUE),
            min_weekly = min(total_count, na.rm = TRUE),
            max_weekly = max(total_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mean_minus_sd = case_when(mean_weekly-sd_weekly > 0 ~
                                     mean_weekly-sd_weekly,
                                   mean_weekly-sd_weekly <= 0 ~
                                     0))

# Pull out spring peak emergence dates
dat_dipt_peaks <- dat_dipt %>%
  group_by(watershed, year, period) %>%
  slice_max(total_count) %>%
  ungroup() %>%
  filter(period == "early") %>%
  mutate(peak_DOY = yday(Date))

# Export for use in future scripts.
# saveRDS(dat_dipt_peaks, "data_working/peaks_annual_dipt_emerge_091225.rds")

# Make a joined dataset for plotting, rather than a table, below.
dat_dipt_plotting <- full_join(dat_dipt_peaks, dat_dipt_weekly,
                               by = c("watershed", "year"))
dat_dipt_plotting <- full_join(dat_dipt_plotting, dat_dipt_sum,
                               by = c("watershed", "year"))

#### Plot ####

# Pasting color scheme colors here for future reference
"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D"
"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494"

# Figure showing time series of all orders
(fig1_all <- ggplot(dat_order %>%
                       filter(watershed %in% c(5,6)), aes(x = Date, 
                                                          y = total_count,
                                                          group = year)) +
   geom_line(linewidth = 1) +
   scale_x_continuous(
     breaks = seq.Date(as.Date("2018-01-01"), 
                       as.Date("2024-12-31"), 
                       by = "1 year"),
     labels = ~ format(.x, "%Y")) +
   labs(y = "Weekly Count") +
   facet_grid(Order~watershed, 
              labeller = labeller(
                watershed = c('5'="Watershed 5",
                              '6'="Watershed 6"),
                Order = c('dipteran'="Diptera",
                          'mayfly'="Ephemeroptera",
                          'stonefly'="Plecoptera",
                          'caddisfly'="Trichoptera")),
              scales = "free") +
   theme_bw() +
   theme(text = element_text(size = 20),
         strip.background = element_blank()))

# Figure showing annual counts of all orders
(fig1_box <- ggplot(dat_annual_sum %>%
                       filter(watershed %in% c(5,6)), 
                     aes(x = watershed, 
                         y = sum_total_count)) +
    geom_boxplot(linewidth = 0.75, width = 0.4) +
    geom_jitter(size = 7, shape = 21, 
                width = 0.2, alpha = 0.8,
                aes(fill = factor(year))) +
    labs(y = "Annual Total Count",
         x = "Watershed",
         fill = "Year") +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Add to time series above.
(fig_all <- fig1_box + fig1_all +
    plot_layout(widths = c(1, 4)) +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_all,
#        filename = "figures/emerge_all_091225.jpg",
#        width = 50,
#        height = 20,
#        units = "cm")

# Figure showing only EPT taxa time series (for SI)
(fig1_ept <- ggplot(dat_order %>%
                      filter(watershed %in% c(5,6)) %>%
                      filter(!Order == "dipteran"), aes(x = Date, 
                                                         y = total_count,
                                                         color = Order,
                                                         group = interaction(Order, year))) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("#FC8D62", 
                                  "#8DA0CB", "#E78AC3"),
                       labels = c("Trichoptera",
                                  "Plecoptera", "Ephemeroptera")) +
    scale_x_continuous(
      breaks = seq.Date(as.Date("2018-01-01"), 
                        as.Date("2024-12-31"), 
                        by = "1 year"),
      labels = ~ format(.x, "%Y")) +
    #scale_y_log10() +
    labs(y = "Weekly Count of Aquatic Insects") +
    facet_grid(watershed~., 
               labeller = labeller(
                 watershed = c('5'="Watershed 5",
                               '6'="Watershed 6"))) +
    theme_bw() +
    theme(text = element_text(size = 20)))

# ggsave(plot = fig1_ept,
#        filename = "figures/emerge_ept_082725.jpg",
#        width = 40,
#        height = 18,
#        units = "cm")

# Figure showing only Diptera time series (for SI)
(suppfig1_dipt <- ggplot(dat_dipt %>% 
                       mutate(keep = case_when(watershed %in% c("1","2", "3",
                                                                  "4", "9", "HBK") &
                                                 year %in% c(2018, 2019, 2020) ~ "Y",
                                               TRUE ~ "N")) %>%
                       # and impose filter
                       filter(keep == "Y"), 
                     aes(x = Date, y = total_count, group = year)) +
   geom_line(linewidth = 1) +
   scale_x_continuous(
     breaks = seq.Date(as.Date("2018-01-01"), 
                       as.Date("2020-12-31"), 
                       by = "1 year"),
     labels = ~ format(.x, "%Y")) +
   labs(y = "Weekly Count of Aquatic Diptera") +
   facet_grid(watershed~., 
              labeller = labeller(
                watershed = c('1'="W1",
                              '2'="W2",
                              '3'="W3",
                              '4'="W4",
                              '5'="W5",
                              '6'="W6",
                              '9'="W9",
                              'HBK'="HB"),
                scales = "free")) +
   theme_bw() +
   theme(text = element_text(size = 20)))

# Figure showing annual counts of Diptera only (for SI)
(suppfig2_dipt <- ggplot(dat_dipt_sum %>%
                       filter(watershed %in% c('1','2','3',
                                               '4','9','HBK')), 
                     aes(x = watershed, 
                         y = sum_total_count)) +
    geom_boxplot(linewidth = 0.75, width = 0.4) +
    geom_jitter(size = 7, shape = 21, 
                width = 0.2, alpha = 0.8,
                aes(fill = factor(year))) +
    labs(y = "Annual Total Count of Aquatic Diptera",
         x = "Watershed",
         fill = "Year") +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Add to time series above.
(fig_dipt <- suppfig2_dipt + suppfig1_dipt +
    plot_layout(widths = c(1, 4)) +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_dipt,
#        filename = "figures/emerge_dipt_091225.jpg",
#        width = 50,
#        height = 20,
#        units = "cm")

# Figure depicting characteristics of annual/peak Diptera emergence
(fig_peak <- ggplot(dat_dipt_plotting %>%
                      filter(year %in% c(2018, 2019, 2020)),
                    aes(x = factor(year), y = peak_DOY,
                        fill = sum_total_count)) +
    geom_point(shape = 21, size = 5) +
    labs(y = "Spring Peak DOY",
         x = "Year",
         fill = "Annual Aquatic\nDiptera Emergence") +
    scale_fill_gradient2(low = "white", mid = "grey80",
                         high = "black", midpoint = 7000) +
    facet_grid(.~watershed,
               labeller = labeller(
                 watershed = c('1'="W1", '2'="W2", '3'="W3",
                               '4'="W4", '5'="W5", '6'="W6",
                               '9'="W9", 'HBK'="HB"))) +
    scale_x_discrete(labels = c("18", "19", "20")) +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Looking to see what this relationship is too...
# Annual emergence vs. peak date
(fig_peak_xy <- ggplot(dat_dipt_plotting %>%
                      filter(year %in% c(2018, 2019, 2020)),
                    aes(x = peak_DOY, y = sum_total_count)) +
    geom_point(shape = 16, size = 4) +
    labs(y = "Annual Aquatic\nDiptera Emergence",
         x = "Spring Peak DOY") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Figure depicting characteristics of weekly Diptera emergence
(fig_weekly <- ggplot(dat_dipt_plotting %>%
                      filter(year %in% c(2018, 2019, 2020)),
                    aes(x = factor(year), y = mean_weekly)) +
    geom_linerange(aes(ymin = mean_minus_sd, 
                       ymax = mean_weekly+sd_weekly),
                   linewidth = 1) +
    geom_point(shape = 16, size = 5) +
    labs(y = "Mean Weekly Aquatic\nDiptera Emergence",
         x = "Year") +
    scale_x_discrete(labels = c("18", "19", "20")) +
    facet_grid(.~watershed,
               labeller = labeller(
                 watershed = c('1'="W1",
                               '2'="W2",
                               '3'="W3",
                               '4'="W4",
                               '5'="W5",
                               '6'="W6",
                               '9'="W9",
                               'HBK'="HB"))) +
    theme_bw() +
    theme(text = element_text(size = 20)))

(fig_weekly_xy <- ggplot(dat_dipt_plotting %>%
                        filter(year %in% c(2018, 2019, 2020)),
                      aes(x = mean_weekly, y = sd_weekly)) +
    geom_point(shape = 16, size = 4) +
    labs(y = "S.D. Weekly Aquatic\nDiptera Emergence",
         x = "Mean Weekly Aquatic\nDiptera Emergence") +
    theme_bw() +
    theme(text = element_text(size = 20)))

(fig_annual_xy <- ggplot(dat_dipt_plotting %>%
                           filter(year %in% c(2018, 2019, 2020)),
                         aes(x = total_count, y = sum_total_count)) +
    geom_point(shape = 16, size = 4) +
    labs(y = "Annual Total Count\nof Aquatic Diptera",
         x = "Peak Aquatic\nDiptera Emergence") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Created joined figures.
(fig_peak_weekly <- fig_peak / fig_weekly +
    plot_annotation(tag_levels = "A"))

(fig_peak_weekly_xy <- fig_peak_xy + fig_weekly_xy + fig_annual_xy +
    plot_annotation(tag_levels = "A"))

# Export figures.
# ggsave(plot = fig_peak_weekly,
#        filename = "figures/emerge_peak_weekly_091225.jpg",
#        width = 50,
#        height = 20,
#        units = "cm")
# 
# ggsave(plot = fig_peak_weekly_xy,
#        filename = "figures/emerge_peak_weekly_xy_091225.jpg",
#        width = 50,
#        height = 15,
#        units = "cm")

#### Statistics ####

# Estimate range
dat_dipt_plotting <- dat_dipt_plotting %>%
  mutate(range_weekly = max_weekly - min_weekly)

# Early peak dataset
dat_dipt_early_peaks_trim <- dat_dipt_peaks %>%
  filter(period == "early") %>%
  filter(year %in% c(2018, 2019, 2020))

mean(dat_dipt_early_peaks_trim$peak_DOY) # 154
sd(dat_dipt_early_peaks_trim$peak_DOY) # 14

# Calculate mean and variability about annual emergence from W5 & W6.
dat_dipt_sum56_stats <- dat_dipt_sum %>%
  filter(watershed %in% c("5", "6")) %>%
  filter(period == "early") %>%
  group_by(watershed) %>%
  summarize(mean_emerge = mean(sum_total_count),
            sd_emerge = sd(sum_total_count)) %>%
  ungroup()

# End of script.
