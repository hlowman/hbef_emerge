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
library(nlme)

# Load data.
dat <- readRDS("data_working/aquatic_counts_complete_yrs_081425.rds")

#### Tidy ####

##### All Taxa ####

# Summarize by order.
dat_order <- dat %>%
  group_by(watershed, year, Date, Order) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

# Weekly CV by order.
dat_cv_order <- dat %>%
  # make all NAs 0s to calculate CV properly
  mutate(count_ed = ifelse(is.na(count), 0, count)) %>%
  group_by(watershed, year, Date, Order) %>%
  # note, this will summarize across all traps collected in a given week
  summarize(mean_count = mean(count, na.rm = TRUE),
            sd_count = sd(count, na.rm = TRUE)) %>%
  ungroup() %>%
  # and now calculate weekly C.V. for plotting
  mutate(cv_count = sd_count/mean_count) %>%
  # and add modified column for plotting
  mutate(cv_count_ed = ifelse(is.na(cv_count), 0, cv_count))

# And calculate annual-level CV
dat_cv_annual <- dat_cv_order %>%
  #filter(year < 2021) %>%
  group_by(watershed, Order) %>%
  # first, remove missing values
  filter(!is.na(cv_count)) %>%
  summarize(mean_cv = mean(cv_count, na.rm = TRUE)) %>%
  ungroup()

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
                            year %in% c(2018, 2019, 2020) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y")

# Calculate mean annual emergence for separate taxa in W5 & W6
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

# Calculate mean annual emergence across all taxa and sites
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

# Calculate total annual insect emergence
dat_annual_sum <- dat_order %>%
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

##### Black flies ####

# Trim data down to only black flies.
dat_dipt <- dat_order %>%
  filter(Order == "dipteran") %>%
  mutate(month = month(Date)) %>%
  # a quick eyeball suggests all peaks happen post October 1 (!)
  mutate(period = case_when(month < 10 ~ "early",
                            TRUE ~ "late"))

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

# Summary statistics of annual dipteran emergence.
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

##### Annual Metrics ####

# Figure showing annual counts of all orders
(fig1_annual_sum <- ggplot(dat_annual_sum, 
                           aes(x = watershed, 
                               y = sum_total_count)) +
    #geom_boxplot(linewidth = 0.75, width = 0.4) +
    geom_jitter(size = 5, shape = 21, 
                width = 0.1, alpha = 0.8,
                aes(fill = factor(year))) +
    labs(y = "Annual Total Count",
         x = "Watershed",
         fill = "Year") +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Export figure.
# ggsave(plot = fig1_annual_sum,
#        filename = "figures/emerge_annual_110325.jpg",
#        width = 40,
#        height = 10,
#        units = "cm")

# Figure showing time series of C.V. values
# Due to narrative order, trimming down to W5 & W6 only
(fig_weekly_cv56 <- ggplot(dat_cv_order %>%
                             mutate(order_f = factor(Order,
                                                     levels = c("dipteran",
                                                                "caddisfly",
                                                                "stonefly",
                                                                "mayfly"))) %>%
                           filter(watershed %in% c(5,6)), 
                           aes(x = Date, 
                               y = cv_count_ed,
                               group = interaction(Order, year))) +
    geom_line(size = 1, alpha = 0.8,
              aes(color = order_f)) +
    labs(y = "Weekly Coefficient of\nVariation",
         x = "Date",
         color = "Order") +
    scale_color_manual(values = c("dipteran" = "#FFAA00",
                                  "stonefly" = "#69B9FA", 
                                  "caddisfly" = "#4B8FF7", 
                                  "mayfly" = "#6B6D9F"),
                       labels = c("dipteran" = "Diptera", 
                                  "stonefly" = "Trichoptera", 
                                  "caddisfly" = "Plecoptera", 
                                  "mayfly" = "Ephemeroptera")) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    facet_grid(watershed~.))

# Export figure.
# ggsave(plot = fig_weekly_cv56,
#        filename = "figures/cv_weekly56_110425.jpg",
#        width = 40,
#        height = 10,
#        units = "cm")

# Figure showing time series of all orders
(fig1_all <- ggplot(dat_order %>%
                       filter(watershed %in% c(5,6)), aes(x = Date, 
                                                          y = total_count,
                                                          group = year,
                                                          color = Order)) +
   geom_line(linewidth = 1) +
   scale_x_continuous(
     breaks = seq.Date(as.Date("2018-01-01"), 
                       as.Date("2024-12-31"), 
                       by = "1 year"),
     labels = ~ format(.x, "%Y")) +
   scale_color_manual(values = c("#FFAA00","#69B9FA","#4B8FF7","#6B6D9F")) +
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
         strip.background = element_blank(),
         legend.position = "none"))

# Export figure.
# ggsave(plot = fig1_all,
#        filename = "figures/emerge_all_110425.jpg",
#        width = 40,
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

##### Peak Emergence #####

# Figure depicting characteristics of peak Diptera emergence
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
                 watershed = c("1"="W1", "2"="W2", "3" ="W3",
                               "4"="W4", "5"="W5", "6"="W6",
                               "9"="W9", "HBK"="HB"))) +
    scale_x_discrete(labels = c("18", "19", "20")) +
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

# Looking to see what these relationships are too...
# Peak emergence vs. peak date
(fig_peak_xy <- ggplot(dat_dipt_plotting %>%
                         filter(watershed %in% c(5, 6)),
                       aes(x = peak_DOY, y = total_count,
                           color = factor(year), shape = factor(watershed))) +
    geom_point(size = 10, alpha = 0.75) +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Peak Aquatic\nDiptera Emergence",
         x = "Spring Peak DOY") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Weekly SD vs. weekly mean emergence
(fig_weekly_xy <- ggplot(dat_dipt_plotting %>%
                        filter(watershed %in% c(5,6)),
                      aes(x = mean_weekly, y = sd_weekly)) +
    geom_point(shape = 16, size = 4) +
    labs(y = "S.D. Weekly Aquatic\nDiptera Emergence",
         x = "Mean Weekly Aquatic\nDiptera Emergence") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Annual emergence vs. peak emergence
(fig_annual_xy <- ggplot(dat_dipt_plotting %>%
                           filter(watershed %in% c(5,6)),
                         aes(x = total_count, y = sum_total_count,
                             color = factor(year), shape = factor(watershed))) +
    geom_point(size = 10, alpha = 0.75) +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Annual Total Count\nof Aquatic Diptera",
         x = "Peak Aquatic\nDiptera Emergence",
         color = "Year",
         shape = "Watershed") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Created joined figures.
(fig_peak_weekly <- fig_peak / fig_weekly +
    plot_annotation(tag_levels = "A"))

(fig_peak_xy <- fig_peak_xy + fig_annual_xy +
    plot_annotation(tag_levels = "A"))

# Export figures.
# ggsave(plot = fig_peak_weekly,
#        filename = "figures/emerge_peak_weekly_091225.jpg",
#        width = 50,
#        height = 20,
#        units = "cm")

ggsave(plot = fig_peak_xy,
       filename = "figures/emerge_peak_xy_110425.jpg",
       width = 40,
       height = 17,
       units = "cm")

#### Statistics ####

# Estimate range
dat_dipt_plotting <- dat_dipt_plotting %>%
  mutate(range_weekly = max_weekly - min_weekly)

# Estimate mean peak emergence from W5 & W6
dat_dipt_early_peaks_trim <- dat_dipt_peaks %>%
  filter(period == "early") %>%
  filter(watershed %in% c(5,6))

mean(dat_dipt_early_peaks_trim$peak_DOY) # 140
sd(dat_dipt_early_peaks_trim$peak_DOY) # 9

# Calculate mean and variability about annual emergence from W5 & W6.
dat_dipt_sum56_stats <- dat_dipt_sum %>%
  filter(watershed %in% c("5", "6")) %>%
  filter(period == "early") %>%
  group_by(watershed) %>%
  summarize(mean_emerge = mean(sum_total_count),
            sd_emerge = sd(sum_total_count)) %>%
  ungroup()

# Calculate C.V. of emergence by year.
dat_annual_stats_redo <- dat_annual_sum %>%
  group_by(watershed) %>%
  summarize(mean_count = mean(sum_total_count),
         sd_count = sd(sum_total_count)) %>%
  mutate(cv_count = sd_count/mean_count) %>%
  ungroup()

# LMEMs for peak dataset.

# Trim down data to only include W5 & W6
dat_dipt_plotting56 <- dat_dipt_plotting %>%
  filter(watershed %in% c(5,6))

## 1 ## Peak DOY vs. peak emergence totals
hist(dat_dipt_plotting56$peak_DOY)
hist(dat_dipt_plotting56$total_count)
# both appear roughly normally distributed considering the low sample size
# so I will be leaving them as is rather than applying a transformation
# of some kind

# Simple linear regression
peak.lm1 <- lm(total_count ~ peak_DOY, data = dat_dipt_plotting56)
summary(peak.lm1)
plot(peak.lm1) # again, looks alright considering low SS
# Re-fit with gls to compare better with lmem() function
peak.lm2 <- gls(total_count ~ peak_DOY, data = dat_dipt_plotting56)
# Fit multi-level model to account for site-level variation
peak.lm3 <- lme(total_count ~ peak_DOY,
                  random = ~1|watershed,
                  data = dat_dipt_plotting56 %>%
                    mutate(watershed = factor(watershed)))
# Compare the two model structures
AIC(peak.lm2, peak.lm3) # essentially identical
# Examine residuals
plot(peak.lm3)
qqnorm(peak.lm3)
# Summary of multi-level model 
summary(peak.lm3)

## 2 ## Peak DOY vs. peak emergence totals
hist(dat_dipt_plotting56$total_count)
hist(dat_dipt_plotting56$sum_total_count)
# again leaving them as is

# Simple linear regression
peak2.lm1 <- lm(sum_total_count ~ total_count, data = dat_dipt_plotting56)
summary(peak2.lm1)
plot(peak2.lm1) # again, looks alright considering low SS
# Re-fit with gls to compare better with lmem() function
peak2.lm2 <- gls(sum_total_count ~ total_count, data = dat_dipt_plotting56)
# Fit multi-level model to account for site-level variation
peak2.lm3 <- lme(sum_total_count ~ total_count,
                random = ~1|watershed,
                data = dat_dipt_plotting56 %>%
                  mutate(watershed = factor(watershed)))
# Compare the two model structures
AIC(peak2.lm2, peak2.lm3) # essentially identical again
# Examine residuals
plot(peak2.lm3)
qqnorm(peak2.lm3)
# Summary of multi-level model 
summary(peak2.lm3)

# End of script.
