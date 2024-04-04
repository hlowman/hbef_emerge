### Emergence Data Exploration
### March 14, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF aquatic insect emergence data.

# Data was downloaded from https://hbwater.org/restricted_QAQC/
# on March 14, 2024.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(viridis)

# Load data.
dat <- read_csv("data_raw/sticky_trap_counts.csv")

#### Tidy ####

# First, need to pivot to make into long format.
dat_long <- dat %>%
  pivot_longer(cols = dipteran_large:other_small,
              names_to = "id",
              values_to = "count")

# Also need to add additional columns to plot more easily.
dat_long <- dat_long %>%
  # create formatted date column
  mutate(Date = mdy(date)) %>%
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

#### Plot ####

# Basic timeseries plot
# Summarize by order
dat_order <- dat_long %>%
  group_by(watershed, Date, Order) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

(fig1 <- ggplot(dat_order, aes(x = Date, 
                              y = total_count,
                              color = Order)) +
   geom_line() +
   scale_x_continuous(
      breaks = seq.Date(as.Date("2018-01-01"), 
                        as.Date("2023-12-31"), 
                        by = "2 year"),
      labels = ~ format(.x, "%Y")) +
   labs(y = "Count (Large + Small)",
        caption = "HBEF Emergence Data - accessed 3.14.24") +
   scale_color_manual(values = cal_palette("figmtn")) +
   facet_grid(Order ~ watershed, scales = "free_y") +
   theme_bw() +
   theme(text = element_text(size = 20)))

# Export figure.
# ggsave(plot = fig1,
#        filename = "figures/emerge_031824.jpg",
#        width = 50,
#        height = 20,
#        units = "cm")

# Ok, let's also zoom in on watershed 6 since that's the longest
# record of all the datasets.
dat_w6 <- dat_long %>%
  filter(watershed == 6) %>%
  group_by(Date, Order, Size) %>%
  # note, this will sum across all traps collected ina given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

(fig2 <- ggplot(dat_w6, aes(x = Date, 
                            y = total_count,
                            color = Order,
                            alpha = Size)) +
    geom_point() +
    scale_alpha_discrete(range=c(1, 0.5)) +
    labs(y = "Count",
         caption = "Watershed 6 Emergence Data") +
    scale_color_manual(values = cal_palette("figmtn")) +
    facet_grid(Order ~ Size, scales = "free_y") +
    theme_bw())

# Export figure.
# ggsave(plot = fig2,
#        filename = "figures/emerge_w6_031824.jpg",
#        width = 15,
#        height = 20,
#        units = "cm")

# Looking at the timeseries overall.
dat_w6_2 <- dat_long %>%
  filter(watershed == 6) %>%
  group_by(Date) %>%
  # note, this will sum across all traps collected ina given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

(fig2.2 <- ggplot(dat_w6_2, aes(x = Date, 
                            y = total_count)) +
    geom_point(color = "#6B6D9F") +
    labs(y = "Count",
         caption = "Watershed 6 Emergence Data") +
    theme_bw())

# Also curious, do stonefly and dipteran numbers correlate?
dat_sd <- dat_order %>%
  filter(Order %in% c("dipteran", "stonefly")) %>%
  mutate(Year = year(Date)) %>%
  group_by(watershed, Year, Order) %>%
  summarize(annual_count = sum(total_count)) %>%
  ungroup() %>%
  drop_na(Year) %>%
  pivot_wider(names_from = Order, values_from = annual_count)

(fig_stone_black <- ggplot(dat_sd, aes(x = dipteran,
                                      y = stonefly,
                                      color = watershed)) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = TRUE) +
  theme_bw())

# Export figure.
ggsave(plot = fig_stone_black,
       filename = "figures/annual_stonefly_blackfly_040424.jpg",
       width = 15,
       height = 10,
       units = "cm")

# Investigate when peak emergence occurred for aquatic species only.
dat_peak_order <- dat_long %>%
  filter(Order %in% c("dipteran", "caddisfly", "mayfly", "stonefly")) %>%
  group_by(watershed, Date, Order) %>%
  # note, this will sum across all traps
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  # and select for the peak emergence each year
  mutate(year = year(Date)) %>%
  group_by(watershed, year, Order) %>%
  slice(which.max(total_count)) %>%
  # add column for plotting
  mutate(jday = yday(Date))

(fig3 <- ggplot(dat_peak_order, aes(x = year, 
                            y = jday,
                            color = Order)) +
    geom_point(size = 6) +
    scale_x_continuous(breaks = c(2020, 2022)) +
    labs(y = "DOY",
         x = "Year",
         caption = "Peak Emergence Data by Watershed") +
    scale_color_manual(values = cal_palette("figmtn")) +
    facet_grid(Order ~ watershed) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Export figure.
# ggsave(plot = fig3,
#        filename = "figures/peak_emerge_031824.jpg",
#        width = 50,
#        height = 20,
#        units = "cm")

# And examine accumulation curves for these same aquatic species.
dat_aq <- dat_long %>%
  filter(Order %in% c("dipteran", "caddisfly", "mayfly", "stonefly")) %>%
  group_by(watershed, Date) %>%
  # note, this will sum across all traps, orders, and sizes
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = year(Date),
         jday = yday(Date)) %>%
  group_by(watershed, year) %>%
  mutate(running_total = cumsum(replace_na(total_count, 0))) %>%
  ungroup()

# Export this for use in making figures with physical timeseries.
saveRDS(dat_aq, "data_working/aquatic_taxa_counts_032824.rds")
  
(fig4 <- ggplot(dat_aq %>%
                  drop_na(year), 
                aes(x = jday, 
                y = running_total,
                color = watershed)) +
    geom_line(size = 1) +
    #xlim(100, 250) +
    labs(y = "Cumulative Sum of Emerged Insects",
         x = "DOY",
         caption = "Cumulative Emergence by Year") +
    scale_color_manual(values = cal_palette("kelp1",
                                            n = 8,
                                            type = "continuous")) +
    facet_grid(year ~ .,) +
    theme_bw())

# Export figure.
# ggsave(plot = fig4,
#        filename = "figures/cumulative_emerge_031824.jpg",
#        width = 15,
#        height = 30,
#        units = "cm")

# Examine overall peak emergence as well as percentiles of overall emergence.
# First, going to pull out maximum count data to add to this dataset.
dat_max <- dat_aq %>%
  group_by(watershed, year) %>%
  slice(which.max(running_total)) %>%
  ungroup() %>%
  # and drop extra years
  drop_na(Date) %>%
  rename(sum_total = running_total) %>%
  select(watershed, year, sum_total)

dat_peak_all <- dat_aq %>%
  # select for the peak emergence each year
  group_by(watershed, year) %>%
  slice(which.max(total_count)) %>%
  ungroup() %>%
  # There's some NA data for W4 I need to resolve here with Adam...
  drop_na(Date) %>%
  # And I'm going to add a column with which to distinguish W6.
  mutate(code = case_when(watershed == 6 ~ "YES",
                          TRUE ~ "NO"))

(fig5 <- ggplot(dat_peak_all, aes(x = year, 
                              y = jday,
                              color = watershed,
                              alpha = code)) +
    geom_jitter(size = 3, width = 0.1) +
    scale_alpha_manual(values = c(0.4, 1), guide = "none") +
    labs(y = "DOY",
         x = "Year",
         caption = "Peak Emergence Data by Watershed") +
    scale_color_manual(values = cal_palette("kelp2", n = 8, type = "continuous")) +
    theme_bw())

# Export figure.
# ggsave(plot = fig5,
#        filename = "figures/peak_emerge_all_032724.jpg",
#        width = 15,
#        height = 10,
#        units = "cm")

# Add maximum tally to dat_aq to be able to calculate percentile thresholds.
dat_aq <- left_join(dat_aq, dat_max) %>%
  mutate(percentile = running_total/sum_total) %>%
  # And I'm going to add a column with which to distinguish W6.
  mutate(code = case_when(watershed == 6 ~ "YES",
                          TRUE ~ "NO"))

# Create table of dates when percentiles are reached.
dat_perc <- dat_aq %>%
  # creates a new column that assigns each observation to a group
  mutate(perc_group = factor(case_when(percentile <= 0.25 ~ "<0.25",
                                percentile > 0.25 & percentile <= 0.5 ~ "0.25",
                                percentile > 0.5 & percentile <= 0.75 ~ "0.5",
                                percentile > 0.75 ~ "0.75"),
         levels = c("<0.25", "0.25", "0.5", "0.75"))) %>%
  # and now I want to pull out the first instance in each group
  group_by(watershed, year, perc_group) %>%
  slice(which.min(jday)) %>%
  ungroup()

(fig6.0 <- ggplot(dat_perc %>%
                  # filtering out <.25 because that's a sampling artefact
                  filter(perc_group %in% c("0.25", "0.5", "0.75")) %>%
                  # and making a version with only W6 for the slides
                  filter(watershed == 6), 
                aes(x = perc_group, 
                    y = jday)) +
    geom_point(size = 3, color = "#3C672C") +
    geom_hline(yintercept = 183) +
    labs(y = "DOY",
         x = "Percentile",
         caption = "Cumulative Count Percentiles of Aquatic Taxa Emergence Data") +
    facet_grid(. ~ year) +
    theme_bw())

# Export figure.
# ggsave(plot = fig6.0,
#        filename = "figures/perc_emerge_W6_032724.jpg",
#        width = 28,
#        height = 10,
#        units = "cm")

(fig6 <- ggplot(dat_perc %>%
                  # filtering out <.25 because that's a sampling artefact
                  filter(perc_group %in% c("0.25", "0.5", "0.75")), 
                aes(x = perc_group, 
                    y = jday,
                    color = watershed,
                    alpha = code)) +
    geom_jitter(size = 3, width = 0.1) +
    scale_alpha_manual(values = c(0.4, 1), guide = "none") +
    geom_hline(yintercept = 183) +
    labs(y = "DOY",
         x = "Percentile",
         caption = "Cumulative Count Percentiles of Aquatic Taxa Emergence Data") +
    scale_color_manual(values = cal_palette("kelp2", n = 8, type = "continuous")) +
    facet_grid(. ~ year) +
    theme_bw())

# Export figure.
# ggsave(plot = fig6,
#        filename = "figures/perc_emerge_all_032524.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

# Also, making a version of this plot so all sites are separately facetted.
(fig6.2 <- ggplot(dat_perc %>%
                  # filtering out <.25 because that's a sampling artefact
                  filter(perc_group %in% c("0.25", "0.5", "0.75")), 
                aes(x = perc_group, 
                    y = jday,
                    color = watershed)) +
    geom_point(size = 6) +
    geom_hline(yintercept = 183) +
    labs(y = "DOY",
         x = "Percentile",
         caption = "Cumulative Count Percentiles of Aquatic Taxa Emergence Data") +
    scale_color_manual(values = cal_palette("kelp2", n = 8, type = "continuous")) +
    facet_grid(watershed ~ year) +
    theme_bw() +
    theme(text = element_text(size = 20), legend.position = "none"))

# Export figure.
# ggsave(plot = fig6.2,
#        filename = "figures/perc_emerge_allfacets_032724.jpg",
#        width = 30,
#        height = 30,
#        units = "cm")
  
# End of script.
  