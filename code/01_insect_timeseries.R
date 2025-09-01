### Time Series Data Summary
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data summarizing
# and visualization of the HBEF aquatic insect emergence data,
# focusing primarily aquatic black flies.

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

# In addition to mean annual emergence for all taxa.
dat_annual_stats <- dat_order %>%
  group_by(watershed, year, Order) %>%
  summarize(sum_total_count = sum(replace_na(total_count,0))) %>%
  ungroup() %>%
  # and impose manuscript data filters
  mutate(keep = case_when(watershed %in% c("5","6") & 
                            year %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024) |
                            watershed %in% c("1","2", "3","4", "9", "HBK") &
                            year %in% c(2018, 2019) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y") %>%
  # and finally summarize by order
  group_by(Order) %>%
  summarize(mean_annual = mean(sum_total_count),
            sd_annual = sd(sum_total_count)) %>%
  ungroup()

# Calculate total black fly emergence
dat_dipt_sum <- dat_dipt %>%
  group_by(watershed, year, period) %>%
  summarize(sum_total_count = sum(replace_na(total_count, 0))) %>%
  ungroup()

# Export for use in future scripts.
#saveRDS(dat_dipt_sum, "data_working/sum_annual_dipt_emerge_081425.rds")

# Difference between largest and smallest sites in 2018
9415/1374 # 685%
# And in 2019
19508/3522 # 554%

# Summary statistics of annual emergence.
dat_dipt_stat <- dat_dipt_sum %>%
  group_by(watershed, period) %>%
  summarize(mean_total = mean(sum_total_count),
            sd_total = sd(sum_total_count)) %>%
  ungroup()

# Calculate weekly emergence
dat_dipt_weekly <- dat_dipt %>%
  group_by(watershed, year, period) %>%
  summarize(mean_weekly = mean(total_count, na.rm = TRUE),
            min_weekly = min(total_count, na.rm = TRUE),
            max_weekly = max(total_count, na.rm = TRUE)) %>%
  ungroup()

# Pull out peak emergence dates
dat_dipt_peaks <- dat_dipt %>%
  group_by(watershed, year, period) %>%
  slice_max(total_count) %>%
  ungroup() %>%
  mutate(peak_DOY = yday(Date))

# Export for use in future scripts.
#saveRDS(dat_dipt_peaks, "data_working/peaks_annual_dipt_emerge_081425.rds")

# Make a joined dataset for plotting, rather than a table, below.
dat_dipt_plotting <- full_join(dat_dipt_peaks, dat_dipt_weekly,
                               by = c("watershed", "year", "period"))
dat_dipt_plotting <- full_join(dat_dipt_plotting, dat_dipt_sum,
                               by = c("watershed", "year", "period"))

#### Plot ####

# Pasting color scheme colors here for future reference
"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D"
"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494"

# Figure showing time series of all orders
(fig1_all <- ggplot(dat_order %>%
                       filter(watershed %in% c(5,6)), aes(x = Date, 
                                                          y = total_count,
                                                          color = Order,
                                                          group = interaction(Order, year))) +
   geom_line(linewidth = 1) +
   scale_color_manual(values = c("#66C2A5", "#FC8D62", 
                                 "#8DA0CB", "#E78AC3"),
                      labels = c("Diptera", "Trichoptera",
                                 "Plecoptera", "Ephemeroptera")) +
   scale_x_continuous(
     breaks = seq.Date(as.Date("2018-01-01"), 
                       as.Date("2024-12-31"), 
                       by = "1 year"),
     labels = ~ format(.x, "%Y")) +
   scale_y_log10() +
   labs(y = "Weekly Count of Aquatic Insects") +
   facet_grid(watershed~., 
              labeller = labeller(
                watershed = c('5'="Watershed 5",
                              '6'="Watershed 6"))) +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "bottom"))

# Figure showing annual counts of EPT vs. Diptera orders
(fig1_all_annual <- ggplot(dat_annual_wide, 
                           aes(x = D, y = EPT)) +
    geom_point(size = 3) +
    labs(x = "Annual Count of Aquatic Diptera",
         y = "Annual Count of EPT") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Add to time series above.
(fig_all <- fig1_all + fig1_all_annual +
    plot_layout(widths = c(3, 1)) +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_all,
#        filename = "figures/emerge_all_082725.jpg",
#        width = 50,
#        height = 15,
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
(fig1_dipt <- ggplot(dat_dipt %>%
                       filter(watershed %in% c(5,6)), aes(x = Date, 
                                      y = total_count,
                                      color = period,
                                      group = year)) +
   geom_line(linewidth = 1) +
   scale_color_manual(values = c("black", "gray70")) +
   scale_x_continuous(
     breaks = seq.Date(as.Date("2018-01-01"), 
                       as.Date("2024-12-31"), 
                       by = "1 year"),
     labels = ~ format(.x, "%Y")) +
   labs(y = "Weekly Count of Aquatic Diptera") +
   facet_grid(watershed~., 
              labeller = labeller(
                watershed = c('5'="Watershed 5",
                              '6'="Watershed 6"))) +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "none"))

# Figure showing annual counts of Diptera only (for SI)
(fig2_dipt <- ggplot(dat_dipt_sum %>%
                       filter(watershed %in% c(5,6)) %>%
                       filter(period == "early"), 
                     aes(x = watershed, 
                         y = sum_total_count)) +
    geom_boxplot(linewidth = 0.75, width = 0.4) +
    geom_jitter(size = 7, shape = 21, 
                width = 0.2, alpha = 0.8,
                aes(fill = factor(year))) +
    labs(y = "Annual Count of Aquatic Diptera",
         x = "Watershed",
         fill = "Year") +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Add to time series above.
(fig_dipt <- fig1_dipt + fig2_dipt +
    plot_layout(widths = c(5, 1)) +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_dipt,
#        filename = "figures/emerge_dipt_082725.jpg",
#        width = 45,
#        height = 22,
#        units = "cm")

# Figure depicting characteristics of annual/peak Diptera emergence
(fig_peak <- ggplot(dat_dipt_plotting %>%
                      filter(period == "early") %>%
                      filter(year %in% c(2018, 2019, 2020)),
                    aes(x = factor(year), y = peak_DOY,
                        fill = factor(year), 
                        size = sum_total_count)) +
    geom_point(shape = 21) +
    labs(y = "Early Period Peak DOY",
         x = "Year",
         fill = "Year",
         size = "Annual Emergence") +
    scale_size_continuous(breaks = c(3000, 8000, 13000)) +
    scale_x_discrete(labels = c("18", "19", "20")) +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    facet_grid(.~watershed,
               labeller = labeller(
                 watershed = c('1'="W1",
                               '2'="W2",
                               '3'="W3",
                               '4'="W4",
                               '5'="W5",
                               '6'="W6",
                               '9'="W9",
                               'HBK'="HBK"))) +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Figure depicting characteristics of weekly Diptera emergence
(fig_weekly <- ggplot(dat_dipt_plotting %>%
                      filter(period == "early") %>%
                      filter(year %in% c(2018, 2019, 2020)),
                    aes(x = factor(year), y = mean_weekly,
                        color = factor(year))) +
    geom_linerange(aes(ymin = min_weekly, ymax = max_weekly),
                   linewidth = 1) +
    geom_point(shape = 21, stroke = 1,
               size = 3, fill = "white") +
    labs(y = "Mean Weekly Emergence",
         x = "Year",
         color = "Year",
         size = "Annual Emergence") +
    scale_x_discrete(labels = c("18", "19", "20")) +
    scale_color_brewer(palette = "Dark2", guide = "none") +
    facet_grid(.~watershed,
               labeller = labeller(
                 watershed = c('1'="W1",
                               '2'="W2",
                               '3'="W3",
                               '4'="W4",
                               '5'="W5",
                               '6'="W6",
                               '9'="W9",
                               'HBK'="HBK"))) +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Created joined figure.
(fig_peak_weekly <- fig_peak / fig_weekly +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_peak_weekly,
#        filename = "figures/emerge_peak_weekly_090125.jpg",
#        width = 40,
#        height = 20,
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
