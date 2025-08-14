### Black Fly Data Summary
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data summarizing
# and visualization of the HBEF aquatic insect emergence data,
# focusing on only aquatic black flies.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)
library(lubridate)
library(RColorBrewer)

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

# Calculate total emergence (reported in Table 2)
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

# Calculate weekly emergence (reported in Table 2)
dat_dipt_weekly <- dat_dipt %>%
  group_by(watershed, year, period) %>%
  summarize(mean_weekly = mean(total_count, na.rm = TRUE),
            min_weekly = min(total_count, na.rm = TRUE),
            max_weekly = max(total_count, na.rm = TRUE)) %>%
  ungroup()

# Pull out peak emergence dates (reported in Table 2)
dat_dipt_peaks <- dat_dipt %>%
  group_by(watershed, year, period) %>%
  slice_max(total_count) %>%
  ungroup() %>%
  mutate(peak_DOY = yday(Date))

# Export for use in future scripts.
#saveRDS(dat_dipt_peaks, "data_working/peaks_annual_dipt_emerge_081425.rds")

# Early peak dataset
dat_dipt_early_peaks <- dat_dipt_peaks %>%
  filter(period == "early")

# Impose manuscript year filters
dat_dipt_early_peaks_trim <- dat_dipt_early_peaks %>%
  mutate(keep = case_when(watershed %in% c("5","6") & 
                            year %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024) |
                            watershed %in% c("1","2", "3","4", "9", "HBK") &
                            year %in% c(2018, 2019) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y")

mean(dat_dipt_early_peaks_trim$peak_DOY) # 149
sd(dat_dipt_early_peaks_trim$peak_DOY) # 16

#### Plot ####

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
   labs(y = "Weekly Count of Simuliidae") +
   facet_grid(watershed~., 
              labeller = labeller(
                watershed = c('5'="Watershed 5",
                              '6'="Watershed 6"))) +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "none"))

# Pasting color scheme colors here for future reference
"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D"

(fig2_dipt <- ggplot(dat_dipt_sum %>%
                       filter(watershed %in% c(5,6)) %>%
                       filter(period == "early"), 
                     aes(x = watershed, 
                         y = sum_total_count)) +
    geom_boxplot(linewidth = 0.75, width = 0.4) +
    geom_jitter(size = 7, shape = 21, 
                width = 0.2, alpha = 0.8,
                aes(fill = factor(year))) +
    labs(y = "Annual Count of Simuliidae",
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
#        filename = "figures/emerge_dipt_081425.jpg",
#        width = 45,
#        height = 22,
#        units = "cm")

#### Statistics ####

# Calculate mean and variability about annual emergence from W5 & W6.
dat_dipt_sum56_stats <- dat_dipt_sum %>%
  filter(watershed %in% c("5", "6")) %>%
  filter(period == "early") %>%
  group_by(watershed) %>%
  summarize(mean_emerge = mean(sum_total_count),
            sd_emerge = sd(sum_total_count)) %>%
  ungroup()

# End of script.
