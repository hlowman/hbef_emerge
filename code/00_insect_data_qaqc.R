### Aquatic Insect Emergence Data Prep
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data tidying
# of the HBEF aquatic insect emergence data.

# Data was downloaded from https://hbwater.org/restricted_QAQC/
# on July 1, 2025.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load data.
dat <- read_csv("data_raw/sticky_trap_counts_070125.csv")

#### Tidy ####

# First, need to correct a missing date.
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
                            year %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024) |
                            watershed %in% c(1,2,9) &
                            year %in% c(2018, 2019, 2020) |
                            watershed == 3 &
                            year %in% c(2018, 2020) |
                            watershed %in% c(4, "HBK") &
                            year %in% c(2018, 2019) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y")

# And make a dataset with only aquatic taxa.
dat_long_aq <- dat_long_trim %>%
  filter(Order %in% c("dipteran", "caddisfly", "stonefly", "mayfly"))

# Export data file for use in future scripts.
# saveRDS(dat_long_aq, "data_working/aquatic_counts_complete_yrs_071825.rds")

# End of script.
