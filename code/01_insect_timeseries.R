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
dat <- readRDS("data_working/aquatic_counts_complete_yrs_120925.rds")

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
  # and add modified columns for plotting
  mutate(cv_count_ed = ifelse(is.na(cv_count), 0, cv_count),
         sd_count_ed = ifelse(is.na(sd_count), 0, sd_count),
         mean_count_ed = ifelse(is.nan(mean_count), 0, mean_count)) %>%
  # and create additional columns for ribbons
  mutate(min = ifelse(mean_count_ed - sd_count_ed < 0, 
                      0, mean_count_ed - sd_count_ed),
         max = mean_count_ed + sd_count_ed)

# And calculate annual-level CV
dat_cv_annual <- dat_cv_order %>%
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
              values_from = sum_total_count)

# Calculate mean annual emergence for separate taxa in W5 & W6
dat_annual_stats_56 <- dat_order %>%
  group_by(watershed, year, Order) %>%
  summarize(sum_total_count = sum(replace_na(total_count,0))) %>%
  ungroup() %>%
  # and impose W5 & W6 data filters
  mutate(keep = case_when(watershed %in% c("5","6") ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y") %>%
  # and finally summarize by order
  group_by(Order) %>%
  summarize(mean_annual = mean(sum_total_count),
            sd_annual = sd(sum_total_count)) %>%
  ungroup()

# Calculate total annual insect emergence
dat_annual_sum <- dat_order %>%
  group_by(watershed, year) %>%
  summarize(sum_total_count = sum(replace_na(total_count, 0))) %>%
  ungroup() %>%
  # and make additional column for borders
  mutate(border = case_when(year %in% c(2018, 2019, 2020) ~ 1.5,
                            year %in% c(2021, 2022, 2023, 2024) ~ 0.25))

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
  group_by(watershed, year) %>%
  summarize(sum_total_count = sum(replace_na(total_count, 0))) %>%
  ungroup()

# Export for use in future scripts.
# saveRDS(dat_dipt_sum, "data_working/sum_annual_dipt_emerge_120925.rds")

# Difference between largest and smallest sites in 2018
11442/1807 # 633%
# And in 2019
20733/3992 # 519%

# Summary statistics of annual dipteran emergence.
dat_dipt_stat <- dat_dipt_sum %>%
  group_by(watershed) %>%
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
# saveRDS(dat_dipt_peaks, "data_working/peaks_annual_dipt_emerge_120925.rds")

# Make a joined dataset for plotting, rather than a table, below.
dat_dipt_plotting <- full_join(dat_dipt_peaks, dat_dipt_weekly,
                               by = c("watershed", "year"))
dat_dipt_plotting <- full_join(dat_dipt_plotting, dat_dipt_sum,
                               by = c("watershed", "year"))

#### Plot ####

# Pasting color scheme colors here for future reference
"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D"
"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494"
"#F7FCF5" "#E5F5E0" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D" "#238B45" "#005A32"
"#FCFBFD" "#EFEDF5" "#DADAEB" "#BCBDDC" "#9E9AC8" "#807DBA" "#6A51A3" "#4A1486"

##### Annual Metrics ####

dat_annual_sum <- dat_annual_sum %>%
  mutate(watershed_f = factor(case_when(watershed == "1" ~ "W1",
                                        watershed == "2" ~ "W2",
                                        watershed == "3" ~ "W3",
                                        watershed == "4" ~ "W4",
                                        watershed == "5" ~ "W5",
                                        watershed == "6" ~ "W6",
                                        watershed == "9" ~ "W9",
                                        watershed == "HBK" ~ "HBK"),
                              levels = c("W1", "W2", "W3",
                                         "W4", "W5", "W6",
                                         "W9", "HBK")))

# Figure showing annual counts of all orders
(fig1_annual_sum <- ggplot(dat_annual_sum, 
                           aes(x = factor(year), 
                               y = sum_total_count,
                               color = factor(year),
                               shape = watershed_f)) +
    geom_point(size = 5, stroke = 1.5,
              position = position_dodge(width = 0.3)) +
    scale_color_manual(values = c("#005A32", "#41AB5D", "#A1D99B",
                                 "grey70", "#9E9AC8", "#6A51A3",
                                 "#4A1486"),
                      guide = "none") +
    scale_shape_manual(values = c(15, 22, 17, 24, 16, 21, 18, 23)) +
    labs(y = "Annual Total Count",
         x = "Year",
         shape = "Site") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Export figure.
# ggsave(plot = fig1_annual_sum,
#        filename = "figures/emerge_annual_120925.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

# Figure showing time series of all orders
(fig1_all <- ggplot(dat_order %>%
                       mutate(order_f = factor(Order,
                                              levels = c("dipteran",
                                                         "caddisfly",
                                                         "stonefly",
                                                         "mayfly"))) %>%
                       filter(watershed %in% c(5,6)), 
                    aes(x = Date, y = total_count,
                        group = year, color = order_f)) +
   geom_line(linewidth = 2) +
   scale_x_continuous(
     breaks = seq.Date(as.Date("2018-01-01"), 
                       as.Date("2024-12-31"), 
                       by = "1 year"),
     labels = ~ format(.x, "%Y")) +
    scale_color_manual(values = c("dipteran" = "#FFAA00",
                                  "stonefly" = "#D46F10", 
                                  "caddisfly" = "#A99CD9", 
                                  "mayfly" = "#654783"),
                       labels = c("dipteran" = "Diptera", 
                                  "caddisfly" = "Trichoptera", 
                                  "stonefly" = "Plecoptera", 
                                  "mayfly" = "Ephemeroptera")) +
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
   theme(text = element_text(size = 40),
         strip.background = element_blank(),
         legend.position = "none"))

# Export figure.
# ggsave(plot = fig1_all,
#        filename = "figures/emerge_all_120925.jpg",
#        width = 70,
#        height = 35,
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
    geom_line(linewidth = 2, aes(color = order_f)) +
    labs(y = "Weekly Coefficient of\nVariation",
         x = "Date",
         color = "Order") +
    scale_color_manual(values = c("dipteran" = "#FFAA00",
                                  "stonefly" = "#D46F10", 
                                  "caddisfly" = "#A99CD9", 
                                  "mayfly" = "#654783"),
                       labels = c("dipteran" = "Diptera", 
                                  "caddisfly" = "Trichoptera", 
                                  "stonefly" = "Plecoptera", 
                                  "mayfly" = "Ephemeroptera")) +
    theme_bw() +
    theme(text = element_text(size = 40),
          strip.background = element_blank(),
          legend.position = "none") +
    facet_grid(Order~watershed, 
               labeller = labeller(
                 watershed = c('5'="Watershed 5",
                               '6'="Watershed 6"),
                 Order = c('dipteran'="Diptera",
                           'mayfly'="Ephemeroptera",
                           'stonefly'="Plecoptera",
                           'caddisfly'="Trichoptera"))))

# Export figure.
# ggsave(plot = fig_weekly_cv56,
#        filename = "figures/cv_weekly56_120925.jpg",
#        width = 70,
#        height = 35,
#        units = "cm")

##### Peak Emergence #####

# Peak emergence vs. peak date
(fig_peak_xy <- ggplot(dat_dipt_plotting %>%
                         filter(watershed %in% c(5, 6)),
                       aes(x = peak_DOY, y = total_count,
                           color = factor(year), 
                           shape = factor(watershed))) +
    geom_point(size = 10, stroke = 2) +
    scale_color_manual(values = c("#005A32", "#41AB5D", "#A1D99B",
                                 "grey80", "#9E9AC8", "#6A51A3",
                                 "#4A1486")) +
    scale_shape_manual(values = c(16, 21)) +
    labs(y = "Peak Counts\nof Aquatic Diptera",
         x = "Spring Peak DOY") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Annual emergence vs. peak emergence
(fig_annual_xy <- ggplot(dat_dipt_plotting %>%
                           filter(watershed %in% c(5,6)),
                         aes(x = total_count, y = sum_total_count,
                             color = factor(year), 
                             shape = factor(watershed))) +
    geom_point(size = 10, stroke = 2) +
    scale_color_manual(values = c("#005A32", "#41AB5D", "#A1D99B",
                                 "grey80", "#9E9AC8", "#6A51A3",
                                 "#4A1486")) +
    scale_shape_manual(values = c(16, 21)) +
    labs(y = "Annual Total Count\nof Aquatic Diptera",
         x = "Peak Counts\nof Aquatic Diptera",
         color = "Year",
         shape = "Watershed") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Created joined figure.
(fig_peak_xy <- fig_peak_xy + fig_annual_xy +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_peak_xy,
#        filename = "figures/emerge_peak_xy_120925.jpg",
#        width = 40,
#        height = 17,
#        units = "cm")

#### Statistics ####

##### Annual Metrics #####

# Regression to investigate effect of year and site on total emergence

# Examine variables
hist(dat_annual_sum$sum_total_count) # looks ok
ggplot(dat_annual_sum, aes(x = watershed_f,
                           y = sum_total_count)) +
  geom_jitter() +
  theme_bw()
ggplot(dat_annual_sum, aes(x = factor(year),
                           y = sum_total_count)) +
  geom_jitter() +
  theme_bw()

# Not worried about correlations here, so just need
# to make all possible covariates factors.
dat_annual_sum <- dat_annual_sum %>%
  mutate(year_f = factor(year))

annual.lm1 <- lm(sum_total_count ~ year_f + watershed_f,
                 data = dat_annual_sum)

# Examine residuals
plot(annual.lm1) # look good!

# Examine results
summary(annual.lm1)

# Calculate mean annual emergence across all taxa and sites
dat_site_stats_all <- dat_order %>%
  group_by(watershed, year) %>%
  summarize(sum_total_count = sum(replace_na(total_count,0))) %>%
  ungroup() %>%
  # and finally summarize by WS
  group_by(watershed) %>%
  summarize(mean_annual = mean(sum_total_count),
            sd_annual = sd(sum_total_count)) %>%
  ungroup() %>%
  mutate(cv_annual = sd_annual/mean_annual)

# and flipping this to be by year
dat_annual_stats_all <- dat_order %>%
  group_by(watershed, year) %>%
  summarize(sum_total_count = sum(replace_na(total_count,0))) %>%
  ungroup() %>%
  # and finally summarize by WS
  group_by(year) %>%
  summarize(mean_annual = mean(sum_total_count),
            sd_annual = sd(sum_total_count)) %>%
  ungroup() %>%
  mutate(cv_annual = sd_annual/mean_annual)

# Estimate % of dipterans counted in W5 and W6
dat_perc <- dat_order %>%
  pivot_wider(names_from = Order, values_from = total_count) %>%
  mutate(total = dipteran + caddisfly + stonefly + mayfly) %>%
  group_by(watershed, year) %>%
  summarize(tot_dipt = sum(dipteran),
            tot_all = sum(total)) %>%
  ungroup() %>%
  filter(watershed %in% c(5,6)) %>%
  summarize(all_dipt = sum(tot_dipt),
            all = sum(tot_all)) %>%
  mutate(perc = all_dipt/all)

##### Peak Emergence #####

# Estimate mean peak emergence from W5 & W6
dat_dipt_early_peaks_trim <- dat_dipt_peaks %>%
  filter(period == "early") %>%
  filter(watershed %in% c(5,6))

mean(dat_dipt_early_peaks_trim$peak_DOY) # 140
sd(dat_dipt_early_peaks_trim$peak_DOY) # 9

# Run paired t-test to investigate for consistent difference
# between sites
w5 <- dat_dipt_early_peaks_trim %>%
  filter(watershed == 5) %>%
  select(peak_DOY)
w6 <- dat_dipt_early_peaks_trim %>%
  filter(watershed == 6) %>%
  select(peak_DOY)

vartest <- var.test(w5$peak_DOY,
                    w6$peak_DOY,
                    alternative = "two.sided",
                    conf.level = 0.95) # p > 0.05 so accept null (var are equal)

diff <- as.data.frame(cbind(w5$peak_DOY, w6$peak_DOY)) %>%
  mutate(difference = V1 - V2)

shapiro.test(diff$difference) # p = 0.04 so accept alternative, not norm. dist.

# Using non-parametric Wilcox test due to evaluation above
(wilcoxtest <- wilcox.test(w5$peak_DOY, w6$peak_DOY, paired = TRUE))
# No significant difference, p = 0.2012

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
