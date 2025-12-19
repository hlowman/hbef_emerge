### Aquatic Insect Emergence Data Prep
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data tidying
# of the HBEF aquatic insect emergence data.

# Data was downloaded from https://hbwater.org/restricted_QAQC/
# on December 19, 2025.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load data.
dat <- read_csv("data_raw/sticky_trap_counts_121925.csv")

#### Tidy ####

# First, need to correct missing dates.
dat <- dat %>%
  mutate(date_ed = case_when(sample_id %in% c("ST231101",
                                              "ST231102",
                                              "ST231103",
                                              "ST231104",
                                              "ST231105") ~ ymd("2023-10-23"),
                             sample_id %in% c("ST231141",
                                              "ST231142") ~ ymd("2023-10-30"),
                             sample_id %in% c("ST200456",
                                              "ST200457",
                                              "ST200458",
                                              "ST200459",
                                              "ST200460") ~ ymd("2020-06-01"),
                             sample_id %in% c("ST200496",
                                              "ST200497",
                                              "ST200498",
                                              "ST200499",
                                              "ST200500") ~ ymd("2020-06-08"),
                             sample_id %in% c("ST200536",
                                              "ST200537",
                                              "ST200538",
                                              "ST200539",
                                              "ST200540") ~ ymd("2020-06-15"),
                             sample_id %in% c("ST200576",
                                              "ST200577",
                                              "ST200578",
                                              "ST200579",
                                              "ST200580") ~ ymd("2020-06-22"),
                             sample_id %in% c("ST211370") ~ ymd("2021-11-08"),
                             sample_id %in% c("ST210312") ~ ymd("2021-05-03"),
                             sample_id %in% c("ST200808") ~ ymd("2020-08-03"),
                             TRUE ~ date))

# Next, need to pivot to make into long format.
dat_long <- dat %>%
  pivot_longer(cols = dipteran_large:other_small,
               names_to = "id",
               values_to = "count")

# Also need to add additional columns to plot more easily.
dat_long <- dat_long %>%
  # create formatted date column
  mutate(Date = ymd(date_ed)) %>%
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
                                    "other_small") ~ "small")) %>%
  mutate(Year = year(Date))

# Examine data coverage.
ggplot(dat_long, aes(x = Date,
                     y = watershed,
                     color = watershed,
                     group = Year)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_grid(watershed ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none")

# And finally, remove the years for which we do not yet have full records.
dat_long_trim <- dat_long %>%
  # make a year column to filter by
  mutate(year = year(Date)) %>%
  mutate(keep = case_when(watershed %in% c("5","6") & 
                            year %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024) |
                            watershed %in% c("1","2", "3","4", "9", "HBK") &
                            year %in% c(2018, 2019, 2020) ~ "Y",
                          TRUE ~ "N")) %>%
  # and impose filter
  filter(keep == "Y")

# And make a dataset with only aquatic taxa.
dat_long_aq <- dat_long_trim %>%
  filter(Order %in% c("dipteran", "caddisfly", "stonefly", "mayfly"))

# Export data file for use in future scripts.
saveRDS(dat_long_aq, "data_working/aquatic_counts_complete_yrs_121925.rds")

# End of script.
