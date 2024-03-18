### Emergence Data Exploration
### March 14, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF aquatic insect emergency data.

# Data was downloaded from https://hbwater.org/restricted_QAQC/
# on March 14, 2024.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)

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
  # note, this will sum across all traps collected ina given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

(fig1 <- ggplot(dat_order, aes(x = Date, 
                              y = total_count,
                              color = Order)) +
   geom_line() +
   labs(y = "Count (Large + Small)",
        caption = "HBEF Emergence Data - accessed 3.14.24") +
   scale_color_manual(values = cal_palette("figmtn")) +
   facet_grid(Order ~ watershed, scales = "free_y") +
   theme_bw())

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

# Investigate when peak emergence occurred for aquatic species.
dat_peak <- dat_long %>%
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

(fig3 <- ggplot(dat_peak, aes(x = year, 
                            y = jday,
                            color = Order)) +
    geom_point(size = 3) +
    labs(y = "DOY",
         x = "Year",
         caption = "Peak Emergence Data by Watershed") +
    scale_color_manual(values = cal_palette("figmtn")) +
    facet_grid(Order ~ watershed) +
    theme_bw())

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
  mutate(running_total = cumsum(total_count)) %>%
  ungroup()
  
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
  
# End of script.
  