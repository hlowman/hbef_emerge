### Emergence Data Indices
### April 8, 2024
### Heili Lowman

#### README ####

# The following script will calculate emergence indices using
# the HBEF aquatic insect emergence data.

# Data was downloaded from https://hbwater.org/restricted_QAQC/
# on April 8, 2024.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(viridis)
library(gt)
library(webshot2)

# Load data.
dat <- read_csv("data_raw/sticky_trap_counts_040824.csv")

#### Tidy ####

# First, need to correct the sole incorrect date.
dat[1393, 4] <- "2020-08-03"

# And quickly check the data structure.
str(dat)

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

# And create a dataset of total weekly emergence.
dat_total_weekly <- dat_long %>%
  group_by(watershed, Date) %>%
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  # dropping data with missing dates for now
  drop_na(Date)

#### Peak Emergence ####

# Calculate peak emergence by watershed and by year.

# Add year column to dataset.
dat_total_weekly$Year <- year(dat_total_weekly$Date)

dat_peak <- dat_total_weekly %>%
  group_by(watershed, Year) %>%
  slice(which.max(total_count)) %>%
  mutate(jday = yday(Date)) %>%
  ungroup()

# And reformat for easier comparison.
# Number of individuals
dat_peak_no <- dat_peak %>%
  select(watershed, Year, total_count) %>%
  pivot_wider(names_from = watershed, values_from = total_count)

(table1 <- dat_peak_no %>%
    gt(rowname_col = "Year",
       groupname_col = NA) %>%
    tab_spanner(label = "Watershed", 
                columns = `1`:`HBK`) %>%
  tab_header(
    title = md("Peak Emergence (individuals)")))

# Export table.
gtsave(data = table1,
       filename = "figures/peak_emerge_table_040824.png")

# Date of peak
dat_peak_date <- dat_peak %>%
  mutate(Day = day(Date)) %>%
  mutate(Month = month(Date)) %>%
  unite(DayMonth, c("Month", "Day"), sep = "-") %>%
  select(watershed, Year, DayMonth) %>%
  pivot_wider(names_from = watershed, 
              values_from = DayMonth)

(table2 <- dat_peak_date %>%
    gt(rowname_col = "Year",
       groupname_col = NA) %>%
    tab_spanner(label = "Watershed", 
                columns = `1`:`HBK`) %>%
    tab_header(
      title = md("Date of Peak Emergence")))

# Export table.
gtsave(data = table2,
       filename = "figures/peak_date_table_040824.png")

#### Total Emergence ####

# Calculate total emergence by watershed and by year.

dat_sum <- dat_total_weekly %>%
  group_by(watershed, Year) %>%
  summarize(annual_count = sum(total_count, 
                               na.rm = TRUE)) %>%
  ungroup()

# And reformat for easier comparison.
# Number of individuals
dat_sum_wide <- dat_sum %>%
  pivot_wider(names_from = watershed, 
              values_from = annual_count)

(table3 <- dat_sum_wide %>%
    gt(rowname_col = "Year",
       groupname_col = NA) %>%
    tab_spanner(label = "Watershed", 
                columns = `1`:`HBK`) %>%
    tab_header(
      title = md("Total Emergence (individuals)")))

# Export table.
gtsave(data = table3,
       filename = "figures/annual_emerge_table_040824.png")

#### Kurtosis ####

# End of script.
