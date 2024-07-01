### Degree Days Calculation
### June 17, 2024
### Heili Lowman

#### README ####

# The following script will calculate degree days to emergence using
# the HBEF water temperature data.

# It will also calculate the rate of temperature increase for the
# warming to peak season due to concerns of conflation of degree 
# days with peak timing (i.e., later timing naturally leads to
# greater degree day values).

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load data tidied by Danielle and her students.
dat <- read_csv("data_raw/QT_15min_EmergeSeasons.csv")
datw3 <- read_csv("data_raw/HBEF_W3_UNHWQAL_WaterTemp.csv")
datw9 <- read_csv("data_raw/HBEF_W9_UNHWQAL_WaterTemp.csv")

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

# And do the same for the other two datasets.
datw3_daily <- datw3 %>%
  mutate(Date = mdy_hm(Date)) %>%
  mutate(season = case_when(Date >= ymd("2018-02-16") &
                           Date < ymd("2018-05-29") ~ "WarmingToPeak18",
                         Date >= ymd("2019-03-19") &
                           Date < ymd("2019-06-10") ~ "WarmingToPeak19",
                         Date >= ymd("2020-03-03") &
                           Date < ymd("2020-05-26") ~ "WarmingToPeak20",
                         TRUE ~ NA)) %>%
  mutate(Season = case_when(season %in% c("WarmingToPeak18",
                                          "WarmingToPeak19",
                                          "WarmingToPeak20") ~ "Warming to Peak",
                            TRUE ~ NA)) %>%
  filter(Season == "Warming to Peak") %>% 
  drop_na(TempC) %>%
  mutate(Y = year(Date),
         M = month(Date),
         D = day(Date)) %>%
  group_by(Y, M, D) %>%
  summarize(mean_tempC_sw = mean(TempC, na.rm = TRUE),
            max_tempC_sw = max(TempC, na.rm = TRUE),
            min_tempC_sw = min(TempC, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  mutate(date = make_date(Y, M, D)) %>%
  mutate(Site_ID = "W3") %>%
  mutate(Year = Y) %>%
  select(Site_ID, Year, date, mean_tempC_sw, min_tempC_sw, max_tempC_sw)

datw9_daily <- datw9 %>%
  mutate(Date = mdy_hm(Date)) %>%
  mutate(Season = case_when(Date >= ymd("2020-03-08") &
                              Date < ymd("2020-06-22") ~ "Warming to Peak",
                            TRUE ~ NA)) %>%
  filter(Season == "Warming to Peak") %>% 
  drop_na(TempC) %>%
  mutate(Y = year(Date),
         M = month(Date),
         D = day(Date)) %>%
  group_by(Y, M, D) %>%
  summarize(mean_tempC_sw = mean(TempC, na.rm = TRUE),
            max_tempC_sw = max(TempC, na.rm = TRUE),
            min_tempC_sw = min(TempC, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  mutate(date = make_date(Y, M, D)) %>%
  mutate(Site_ID = "W3") %>%
  mutate(Year = Y) %>%
  select(Site_ID, Year, date, mean_tempC_sw, min_tempC_sw, max_tempC_sw)

#### Degree Days ####

# There are two main ways of calculating degree days, so
# we need to add columns that accomplish both.
# First, the overall dataset.
dat_daily <- dat_daily %>%
  # average daily temp - base temp (1C)
  mutate(degree_day_1 = mean_tempC_sw - 1,
  # (max daily temp + min daily temp) / 2 - base temp (1C)
         degree_day_2 = ((max_tempC_sw + min_tempC_sw)/2) - 1)

# Now, calculate cumulative degree days by site and year.
dat_cdd <- dat_daily %>%
  # create a new doy column
  mutate(doy = yday(date)) %>%
  group_by(Site_ID, Year) %>%
  summarize(cum_degree_day_1 = sum(degree_day_1),
            cum_degree_day_2 = sum(degree_day_2),
            # also calculate rate of change of temperature
            delta_temp = coef(lm(mean_tempC_sw ~ doy))[[2]]) %>%
  ungroup()

# And I need to remove those years for which data is not
# complete and the cumulative degree days should be disregarded.
dat_cdd <- dat_cdd[-3,]

# Next, for the other two watersheds.
datw3_daily <- datw3_daily %>%
  # average daily temp - base temp (1C)
  mutate(degree_day_1 = mean_tempC_sw - 1,
         # (max daily temp + min daily temp) / 2 - base temp (1C)
         degree_day_2 = ((max_tempC_sw + min_tempC_sw)/2) - 1)

datw9_daily <- datw9_daily %>%
  # average daily temp - base temp (1C)
  mutate(degree_day_1 = mean_tempC_sw - 1,
         # (max daily temp + min daily temp) / 2 - base temp (1C)
         degree_day_2 = ((max_tempC_sw + min_tempC_sw)/2) - 1)

# Now, calculate cumulative degree days by site and year.
datw3_cdd <- datw3_daily %>%
  mutate(doy = yday(date)) %>%
  group_by(Site_ID, Year) %>%
  summarize(cum_degree_day_1 = sum(degree_day_1),
            cum_degree_day_2 = sum(degree_day_2),
            delta_temp = coef(lm(mean_tempC_sw ~ doy))[[2]]) %>%
  ungroup()

datw9_cdd <- datw9_daily %>%
  mutate(doy = yday(date)) %>%
  group_by(Site_ID, Year) %>%
  summarize(cum_degree_day_1 = sum(degree_day_1),
            cum_degree_day_2 = sum(degree_day_2),
            delta_temp = coef(lm(mean_tempC_sw ~ doy))[[2]]) %>%
  ungroup()

# Join with data above.
dat_cdd_all <- rbind(dat_cdd, datw3_cdd)
dat_cdd_all <- rbind(dat_cdd, datw9_cdd)

# Export for use in analyses.
# saveRDS(dat_cdd_all, "data_working/sum_degreedays_070124.rds")

# End of script.
