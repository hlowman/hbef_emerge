### Emergence Data Summary
### May 13, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data tidying
# and visualization of the HBEF aquatic insect emergence data.

# Primarily, this will focus on time series of emergence, both
# in real time and cumulative plots by watershed.

# Data was downloaded from https://hbwater.org/restricted_QAQC/
# on May 13, 2025.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(viridis)

# Load data.
dat <- read_csv("data_raw/sticky_trap_counts_051325.csv")

# Hex values for later use
"#3900b3ff", "#714dbfff", "#9e6b90ff", "#cf9270ff", "#ebb698ff"
"#3900B3", "#5524BD", "#7148C8", "#8D6DD3", "#AA91DE", "#C6B6E9"
"#404873ff", "#505990ff", "#5f6aadff", "#6f7bcaff", "#7e8be6ff", "#bfc5f3ff"

#### Tidy ####

# First, need to correct an incorrect date.
dat[1393, 4] <- ymd("2020-08-03")

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
                                  # other refers to other terrestrial insects
                                  # terrestrial refers to terrestrial diptera only
                                  id %in% c("other_large",
                                            "other_small") ~ "other"),
                        levels = c("dipteran", "caddisfly", "stonefly",
                                   "mayfly", "terrestrial", "other"))) %>%
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

# And finally, remove the years for which we do not yet have full records.
dat_long_trim <- dat_long %>%
  # make a year column to filter by
  mutate(year = year(Date)) %>%
  mutate(keep = case_when(watershed %in% c(5,6) & 
                                year %in% c(2018, 2019, 2020, 2021, 2022, 2023) |
                              watershed == 1 &
                                year %in% c(2018, 2019, 2020, 2021) |
                              watershed %in% c(2,9) &
                                year %in% c(2018, 2019, 2020) |
                              watershed == 3 &
                                year %in% c(2018, 2020) |
                              watershed %in% c(4, "HBK") &
                                year %in% c(2018, 2019) ~ "Y",
                              TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y")

# Export data file for use in future scripts.
saveRDS(dat_long_trim, "data_working/aquatic_counts_long_051325.rds")

#### Plot ####

##### Timeseries #####

# Summarize by order
dat_order <- dat_long_trim %>%
  group_by(watershed, year, Date, Order) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

(fig1 <- ggplot(dat_order, aes(x = Date, 
                               y = total_count,
                               color = Order,
                               group = year)) +
    geom_line() +
    scale_x_continuous(
      breaks = seq.Date(as.Date("2018-01-01"), 
                        as.Date("2024-12-31"), 
                        by = "3 year"),
      labels = ~ format(.x, "%Y")) +
    labs(y = "Count (Large + Small)",
         caption = "HBEF Emergence Data - accessed 5.13.25") +
    scale_color_manual(values = cal_palette("figmtn")) +
    facet_grid(Order ~ watershed, scales = "free_y") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Export figure.
# ggsave(plot = fig1,
#        filename = "figures/emerge_051324.jpg",
#        width = 50,
#        height = 20,
#        units = "cm")

# Timeseries plot including only aquatic insects
dat_order_aq <- dat_order %>%
  filter(Order %in% c("dipteran", "mayfly", "stonefly", "caddisfly"))

(fig2 <- ggplot(dat_order_aq, aes(x = Date, 
                               y = total_count,
                               color = Order,
                               group = year)) +
    geom_line(linewidth = 0.5) +
    scale_x_continuous(
      breaks = seq.Date(as.Date("2018-01-01"), 
                        as.Date("2024-12-31"), 
                        by = "2 year"),
      labels = ~ format(.x, "%Y")) +
    labs(y = "Count") +
    scale_color_manual(values = c("#3900b3ff", "#714dbfff", "#9e6b90ff", "#cf9270ff")) +
    facet_grid(Order ~ watershed, scales = "free_y",
               labeller = labeller(
                 watershed = c('1'="Watershed 1", '2'="Watershed 2",
                           '3'="Watershed 3", '4'="Watershed 4",
                           '4'="Watershed 4", '5'="Watershed 5",
                           '6'="Watershed 6", '9'="Watershed 9",
                           'HBK'="Hubbard Brook"),
                 Order = c('dipteran'="Diptera", 'mayfly'="Ephemeroptera",
                          'stonefly'="Plecoptera", 'caddisfly'="Tricoptera"))) +
    theme_bw() +
    theme(text = element_text(size = 16),
          legend.position = "none"))

# Export figure.
# ggsave(plot = fig2,
#        filename = "figures/emerge_aq_051324.jpg",
#        width = 40,
#        height = 17,
#        units = "cm")

# Summarize by watershed
dat_ws <- dat_long_trim %>%
  filter(Order %in% c("dipteran", "mayfly", "stonefly", "caddisfly")) %>%
  group_by(watershed, year, Date) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

(fig3 <- ggplot(dat_ws, aes(x = Date, 
                            y = total_count,
                            group = year)) +
    geom_line(linewidth = 0.5) +
    scale_x_continuous(
      breaks = seq.Date(as.Date("2018-01-01"), 
                        as.Date("2024-12-31"), 
                        by = "1 year"),
      labels = ~ format(.x, "%Y")) +
    labs(y = "Count") +
    facet_grid(watershed ~ ., #scales = "free_y",
               labeller = labeller(
                 watershed = c('1'="Watershed 1", '2'="Watershed 2",
                               '3'="Watershed 3", '4'="Watershed 4",
                               '4'="Watershed 4", '5'="Watershed 5",
                               '6'="Watershed 6", '9'="Watershed 9",
                               'HBK'="Hubbard Brook"))) +
    theme_bw() +
    theme(text = element_text(size = 16)))

# Export figure.
# ggsave(plot = fig3,
#        filename = "figures/emerge_ws_051324.jpg",
#        width = 17,
#        height = 30,
#        units = "cm")

##### Cumulative #####

# Calculate cumulative emergence
dat_cumulative <- dat_ws %>%
  group_by(watershed, year) %>%
  mutate(sum_total_count = cumsum(replace_na(total_count, 0))) %>%
  ungroup() %>%
  mutate(doy = yday(Date))

(fig4 <- ggplot(dat_cumulative, aes(x = doy, 
                                    y = sum_total_count,
                                    group = year,
                                    color = factor(year))) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("black", "#5524BD", "#7148C8", 
                                  "#8D6DD3", "#AA91DE", "#C6B6E9")) +
    labs(y = "Count",
         x = "Day of Year",
         color = "Year") +
    facet_wrap(watershed ~ ., ncol = 4,
               labeller = labeller(
                 watershed = c('1'="Watershed 1", '2'="Watershed 2",
                               '3'="Watershed 3", '4'="Watershed 4",
                               '4'="Watershed 4", '5'="Watershed 5",
                               '6'="Watershed 6", '9'="Watershed 9",
                               'HBK'="Hubbard Brook"))) +
    theme_bw() +
    theme(text = element_text(size = 16)))

# Export figure.
# ggsave(plot = fig4,
#        filename = "figures/emerge_cumulative_051324.jpg",
#        width = 40,
#        height = 17,
#        units = "cm")

# End of script.
