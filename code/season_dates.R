### Phenological Season Delineation
### April 30, 2024
### Heili Lowman

#### README ####

# The following script will aggregate data sources to delineate
# the phenological seasons in Hubbard Brook Watersheds.

# Warming, snow and canopy-free season (Warming)
# START: start of snowmelt (1st changepoint in Q)

# Leaf season (Leaf)
# START: first date of 50% leaf area index (LAI) surpassed

# Cooling, snow and canopy-free season (Cooling)
# START: first date to fall below 50% LAI

# Snow season (Snow)
# START: first day of snow survey

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

#### Data ####

# Load snowmelt data from Danielle.
snowmelt_data <- read_csv("data_raw/HB_spring_melt_duration_emerge.csv")

# Load smoothed MODIS LAI data.
lai_data <- readRDS("data_working/hb_lai_processed_050124.rds")

# Load snow course data.
snow_data <- read_csv("data_raw/HBEF_snowcourse_1956-2023.csv")

#### Warming Start Date ####

# For the start of the warming season, we'll be using the first
# of four change points in the stream discharge data between one
# week prior to max SWE and three weeks after snow disappearance.

snowmelt_start <- snowmelt_data %>%
  select(site_id, Yr, start_day) %>%
  rename("Site_ID" = "site_id",
         "Year" = "Yr",
         "Warming" = "start_day")

#### Leaf Start Date ####

# For the start of the leaf or canopy season, we'll be using the
# first time, in the processed/interpolated data, that the LAI
# surpasses the median annual value.
View(lai_data$HBK)

# Will create a workflow to notate this date and then iterate over
# the full list.
test <- lai_data$HBK %>%
  group_by(Year) %>%
  # calculate median
  mutate(median_LAI = median(LAI_proc, na.rm = TRUE)) %>%
  ungroup() %>%
  # create new column to delineate threshold
  mutate(above_median = case_when(LAI_proc <= median_LAI ~ 0,
                                  LAI_proc > median_LAI ~ 1,
                                  TRUE ~ NA)) %>%
  drop_na(LAI_proc) %>%
  # and select for only the days where the threshold is reached
  mutate(start_day = above_median - lag(above_median, 
                                       default = above_median[1])) %>%
  filter(start_day == 1) %>%
  select(Year, DOY)

leaf_start <- function(lai_ts) {
  
  df <- lai_ts %>%
    group_by(Year) %>%
    # calculate median
    mutate(median_LAI = median(LAI_proc, na.rm = TRUE)) %>%
    ungroup() %>%
    # create new column to delineate threshold
    mutate(above_median = case_when(LAI_proc <= median_LAI ~ 0,
                                    LAI_proc > median_LAI ~ 1,
                                    TRUE ~ NA)) %>%
    drop_na(LAI_proc) %>%
    # and select for only the days where the threshold is reached
    mutate(start_day = above_median - lag(above_median, 
                                          default = above_median[1])) %>%
    filter(start_day == 1) %>%
    select(Year, DOY)
  
  return(df)
  
}

# Apply the above function to all sites.
lai_leaf_starts <- lapply(lai_data, function(x) leaf_start(x))

# Turn the resulting list into a usable dataframe.
leaf_start <- plyr::ldply(lai_leaf_starts, data.frame) %>%
  rename("Site_ID" = ".id",
         "Leaf" = "DOY")

#### Cooling Start Date ####

# For the start of the cooling season, we'll be using the
# first time, in the processed/interpolated data, that the LAI
# dips below the median annual value at the end of the year.

# Will create a workflow to notate this date and then iterate over
# the full list.
test2 <- lai_data$HBK %>%
  group_by(Year) %>%
  # calculate median
  mutate(median_LAI = median(LAI_proc, na.rm = TRUE)) %>%
  ungroup() %>%
  # create new column to delineate threshold
  mutate(above_median = case_when(LAI_proc <= median_LAI ~ 0,
                                  LAI_proc > median_LAI ~ 1,
                                  TRUE ~ NA)) %>%
  drop_na(LAI_proc) %>%
  # and select for only the days where the threshold is reached
  mutate(start_day = above_median - lag(above_median, 
                                        default = above_median[1])) %>%
  # this time we select for the first time it dips *below*
  filter(start_day == -1) %>%
  select(Year, DOY)

cooling_start <- function(lai_ts) {
  
  df <- lai_ts %>%
    group_by(Year) %>%
    # calculate median
    mutate(median_LAI = median(LAI_proc, na.rm = TRUE)) %>%
    ungroup() %>%
    # create new column to delineate threshold
    mutate(above_median = case_when(LAI_proc <= median_LAI ~ 0,
                                    LAI_proc > median_LAI ~ 1,
                                    TRUE ~ NA)) %>%
    drop_na(LAI_proc) %>%
    # and select for only the days where the threshold is reached
    mutate(start_day = above_median - lag(above_median, 
                                          default = above_median[1])) %>%
    # this time we select for the first time it dips *below*
    filter(start_day == -1) %>%
    select(Year, DOY)
  
  return(df)
  
}

# Apply the above function to all sites.
lai_cooling_starts <- lapply(lai_data, function(x) cooling_start(x))

# Turn the resulting list into a usable dataframe.
cooling_start <- plyr::ldply(lai_cooling_starts, data.frame) %>%
  rename("Site_ID" = ".id",
         "Cooling" = "DOY")

#### Snow Start Date ####

# For the start of the snow season, we'll be using the first
# day of the snow record based on two snow courses (one each for
# the north and south-facing slopes).

# Trim to dates and sites of interest.
snow_data_2017 <- snow_data %>%
  mutate(date = ymd(Date)) %>%
  filter(Site %in% c("STA2", "STA9", "STA17", 
                     "STA19", "STAHQ")) %>%
  mutate(slope = factor(case_when(Site %in% c("STA2", "STA9") ~ "South",
                           Site %in% c("STA17", "STA19") ~ "North",
                           TRUE ~ "HQ"))) %>%
  filter(date > ymd("2017-01-01"))

# First survey dates.
# The dates across courses are so similar, I will err on the side
# of the minimum or first date between courses.
snow_first <- snow_data_2017 %>%
  # Note, WINTER usually refers to the year AFTER which it starts,
  # so Winter 2019 starts in December 2018.
  group_by(slope, WINTER) %>%
  slice_min(date) %>%
  ungroup() %>%
  # Ok, so this only selects the minimum dates, including both if there
  # are two, so I need to select only the first one.
  group_by(slope, WINTER) %>%
  slice_head(n = 1) %>%
  ungroup()

# CAUTION: There is no W 2022/2023 data for the South-facing slope,
# or the HQ sites, so need to omit those for now.

# Snowcourse 2 is in W1. Snowcourse 9 is in W6. - SOUTH (facing)
# Snowcourse 17 & 19 are in W8. - NORTH (facing)
# I'll match the HQ to the mainstem HBK.

# Trim dataset for better merging.
snow_start <- snow_first %>%
  select(date, slope) %>%
  mutate(Year = year(date)) %>%
  mutate(Snow = yday(date)) %>%
  # Trim date that doesn't apply.
  filter(date > ymd("2017-06-01")) %>%
  # Removing excess date column
  select(-date)

# And trim duplicate years.
snow_start <- snow_start[-c(6,18),]

#### Join Seasons ####

join1 <- full_join(snowmelt_start, leaf_start)
join2 <- full_join(join1, cooling_start)

# Need to add slope to the dataset
join2$slope <- case_when(join2$Site_ID == "HBK" ~ "HQ",
                         join2$Site_ID %in% c("W1", "W2", "W3",
                                        "W4", "W5", "W6") ~ "South",
                         join2$Site_ID == "W9" ~ "North",
                         TRUE ~ NA)

join3 <- full_join(join2, snow_start)

# Re-organize a bit.
join3 <- join3 %>%
  select(Site_ID, slope, Year, Warming, Leaf, Cooling, Snow)

# Export for future use.
#saveRDS(join3, "data_working/season_dates_pheno_050724.rds")

#### Plot Seasons ####

# Using function to calculate date from DOY and year alone, link here : 
# https://stackoverflow.com/questions/63963616/julian-day-to-date-vector

yearyearday <- function(yr, yd) {
  base <- as.Date(paste0(yr, "-01-01")) # take Jan 1 of year
  day <- base + yd - 1
}

# Need to re-format a bit before plotting.
dat_long <- join3 %>%
  mutate(Warming_date = yearyearday(Year, Warming),
         Leaf_date = yearyearday(Year, Leaf),
         Cooling_date = yearyearday(Year, Cooling),
         Snow_date = yearyearday(Year, Snow)) %>%
  select(Site_ID, Year, Warming_date, Leaf_date,
         Cooling_date, Snow_date) %>%
  rename("Warming" = "Warming_date",
         "Leaf" = "Leaf_date",
         "Cooling" = "Cooling_date",
         "Snow" = "Snow_date") %>%
  pivot_longer(cols = Warming:Snow, names_to = "Season") %>%
  mutate(Date = ymd(value)) %>%
  mutate(Season = factor(Season, levels = c("Warming",
                                            "Leaf",
                                            "Cooling",
                                            "Snow")))

ggplot(dat_long, aes(x = Date, y = Site_ID, color = Season)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#FFAA00",
                                "#609048",
                                "#D46F10",
                                "#69B9FA")) + 
  # spring
  #geom_linerange(aes(xmin = hms1a - Date, ymax = hms2a - Date)) +
  labs(x = "Date", y = "Site") +
  theme_bw()

# End of script.
