### Degree Days Calculation
### June 17, 2024
### Heili Lowman

#### README ####

# The following script will calculate degree days to emergence using
# the HBEF water temperature data.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load data tidied by Danielle and her students.
dat <- read_csv("data_raw/QT_15min_EmergeSeasons.csv")

#### Examine ####

# Before tidying the data, I'll examine the raw data
# for the time periods of interest.
ggplot(dat, aes(x = datetime, y = tempC_sw)) +
  geom_point() + 
  labs(x = "Date", y = "Temperature (C)") +
  theme_bw() +
  facet_grid(Site_ID~.)

# Watersheds 2, 4, 5, & 6 appear to have the most complete records.
# Lacking data entirely for HBK, W3, and W9.

#### Tidy data ####

# First, I'll filter only for the warming to peak season,
# aggregate by day, and calculate the mean
# daily water temperature at each site.
dat_daily <- dat %>%
  # first, filter by season of interest
  filter(Season == "Warming to Peak") %>%
  # and drop places where records simply don't exist
  drop_na(tempC_sw) %>%
  # create new formatted date columns
  # because grouping by the date column along is
  # spitting back an integer for some reason
  mutate(Y = year(date),
         M = month(date),
         D = day(date)) %>%
  group_by(Site_ID, Year, Y, M, D) %>%
  summarize(mean_tempC_sw = mean(tempC_sw, na.rm = TRUE),
            max_tempC_sw = max(tempC_sw, na.rm = TRUE),
            min_tempC_sw = min(tempC_sw, na.rm = TRUE)) %>%
  ungroup() %>%
  # replace all resulting NaNs with NAs
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  # and re-create date column
  mutate(date = make_date(Y, M, D)) %>%
  # and keep only columns of interest
  select(Site_ID, Year, date, mean_tempC_sw, min_tempC_sw, max_tempC_sw)

#### Degree Days ####

# There are two main ways of calculating degree days, so
# we need to add columns that accomplish both.
dat_daily <- dat_daily %>%
  # average daily temp - base temp (1C)
  mutate(degree_day_1 = mean_tempC_sw - 1,
  # (max daily temp + min daily temp) / 2 - base temp (1C)
         degree_day_2 = ((max_tempC_sw + min_tempC_sw)/2) - 1)

# Now, calculate cumulative degree days by site and year.
dat_cdd <- dat_daily %>%
  group_by(Site_ID, Year) %>%
  summarize(cum_degree_day_1 = sum(degree_day_1),
            cum_degree_day_2 = sum(degree_day_2)) %>%
  ungroup()

# And I need to remove those years for which data is not
# complete and the cumulative degree days should be disregarded.
dat_cdd <- dat_cdd[-1,]

# Export for use in analyses.
#saveRDS(dat_cdd, "data_working/sum_degreedays_061724.rds")

# End of script.
