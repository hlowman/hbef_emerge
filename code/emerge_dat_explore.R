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
  summarize(total_count = sum(count, rm.na = TRUE)) %>%
  ungroup()

(fig1 <- ggplot(dat_order, aes(x = Date, 
                              y = total_count,
                              color = Order)) +
   geom_point() +
   labs(y = "Count (Large + Small)") +
   scale_color_manual(values = cal_palette("figmtn")) +
   facet_grid(Order ~ watershed, scales = "free") +
   theme_bw())

# Export figure.
# ggsave(plot = fig1,
#        filename = "figures/emerge_031424.jpg",
#        width = 50,
#        height = 20,
#        units = "cm")

# Hmmm, something's not looking quite right -
# need to check to be sure this collapsing isn't doing
# anything strange.
  
# End of script.
  