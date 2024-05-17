### Phenological Season Delineation
### April 30, 2024
### Heili Lowman

#### README ####

# The following script will aggregate data sources to delineate
# the phenological seasons in Hubbard Brook Watersheds.

# Warming, snow and canopy-free season (Warming)
# START: start of snowmelt (1st changepoint in Q)

# Warming season to peak emergence
# START: start of snowmelt (1st changepoint in Q)
# END: date of peak mid-year emergence

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
snowmelt_data <- read_csv("data_raw/HB_spring_melt_duration_emerge_051724.csv")

# Load smoothed MODIS LAI data.
lai_data <- readRDS("data_working/hb_lai_processed_050124.rds")

# Load peak emergence dates.
peak_data <- readRDS("data_working/peak_emerge_dates_051724.rds")

# Load snow course data.
snow_data <- read_csv("data_raw/HBEF_snowcourse_1956-2023.csv")

#### Warming Start Date ####

# For the start of the warming season, we'll be using the first
# of four change points in the stream discharge data between one
# week prior to max SWE and three weeks after snow disappearance.

snowmelt_start <- snowmelt_data %>%
  mutate(Site_ID = case_when(site_id == "HB" ~ "HBK",
                             TRUE ~ site_id)) %>%
  select(Site_ID, Yr, start_day) %>%
  rename("Year" = "Yr",
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

#### Peak Date ####

# Format the peak emergence dataset properly.
peak_start <- peak_data %>%
  mutate(Site_ID = case_when(watershed == 1 ~ "W1",
                   watershed == 2 ~ "W2",
                   watershed == 3 ~ "W3",
                   watershed == 4 ~ "W4",
                   watershed == 5 ~ "W5",
                   watershed == 6 ~ "W6",
                   watershed == 9 ~ "W9",
                   TRUE ~ "HBK")) %>%
  select(Site_ID, Year, jday) %>%
  rename("Peak" = "jday")

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
  ungroup() %>%
  # Also, there are a few incorrect dates at the beginning
  # of certain records, so I need to fix these
  mutate(date_ed = case_when(Site == "STA2" & WINTER == 2023 ~
                               ymd("2022-11-29"),
                             Site == "STAHQ" & WINTER == 2023 ~
                               ymd("2022-11-29"),
                             TRUE ~ date))

# Snowcourse 2 is in W1. Snowcourse 9 is in W6. - SOUTH (facing)
# Snowcourse 17 & 19 are in W8. - NORTH (facing)
# I'll match the HQ to the mainstem HBK.

# Trim dataset for better merging.
snow_start <- snow_first %>%
  select(date_ed, slope) %>%
  mutate(Year = year(date_ed)) %>%
  mutate(Snow = yday(date_ed)) %>%
  # Trim date that doesn't apply.
  filter(date_ed > ymd("2017-06-01")) %>%
  # Removing excess date column
  select(-date_ed)

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

# Add peak emergence dates.
join4 <- full_join(join3, peak_start)

# Re-organize a bit.
join4 <- join4 %>%
  select(Site_ID, slope, Year, 
         Warming, Leaf, Peak, 
         Cooling, Snow)

# Export for future use.
# saveRDS(join4, "data_working/season_dates_pheno_051724.rds")

# Using function to calculate date from DOY and year alone, link here : 
# https://stackoverflow.com/questions/63963616/julian-day-to-date-vector

yearyearday <- function(yr, yd) {
  base <- as.Date(paste0(yr, "-01-01")) # take Jan 1 of year
  day <- base + yd - 1
}

# Need to re-format a bit before plotting.
dat_long <- join4 %>%
  mutate(Warming_date = yearyearday(Year, Warming),
         Leaf_date = yearyearday(Year, Leaf),
         Peak_date = yearyearday(Year, Peak),
         Cooling_date = yearyearday(Year, Cooling),
         Snow_date = yearyearday(Year, Snow)) %>%
  select(Site_ID, Year, Warming_date, Leaf_date,
         Peak_date, Cooling_date, Snow_date) %>%
  rename("Warming" = "Warming_date",
         "Leaf" = "Leaf_date",
         "Peak" = "Peak_date",
         "Cooling" = "Cooling_date",
         "Snow" = "Snow_date") %>%
  pivot_longer(cols = Warming:Snow, names_to = "Season") %>%
  mutate(Date = ymd(value)) %>%
  mutate(Season = factor(Season, levels = c("Warming",
                                            "Leaf",
                                            "Peak",
                                            "Cooling",
                                            "Snow")))

#### Plot Seasons ####

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

# Trying another format to see if I can get a gantt-type chart
# working.
dat_wide <- join3 %>%
  mutate(Warming_date = yearyearday(Year, Warming),
         Leaf_date = yearyearday(Year, Leaf),
         Cooling_date = yearyearday(Year, Cooling),
         Snow_date = yearyearday(Year, Snow)) %>%
  select(Site_ID, Year, Warming_date, Leaf_date,
         Cooling_date, Snow_date) %>%
  rename("Warming_start" = "Warming_date",
         "Leaf_start" = "Leaf_date",
         "Cooling_start" = "Cooling_date",
         "Snow_start" = "Snow_date") %>%
  # Need to add special column for next years warming
  # otherwise snow plots the WHOLE year
  # This is proving very difficult with the lead() function
  # for some reason, so hard coding it for the moment
  mutate(next_Warming_start = c("2019-03-15","2020-03-07","2021-03-09", NA,
                                "2019-03-28","2020-03-07", NA,
                                "2019-03-19","2020-02-23", NA,
                                "2019-03-29","2020-03-03", NA, 
                                "2019-03-18","2020-02-24","2021-03-09", NA,
                                "2019-03-18","2020-03-03","2021-03-09",
                                "2022-02-13","2023-04-04", NA,
                                "2019-03-28","2020-03-09", NA,
                                NA, NA, NA, NA, NA, NA, NA,
                                "2018-02-20", NA, NA, 
                                "2018-02-20", NA, NA, NA,
                                "2018-02-17", NA, NA, NA,
                                "2018-03-09", NA, NA, NA, 
                                "2018-02-12", NA, NA,
                                "2018-03-29", 
                                "2018-03-03", NA, NA, NA)) %>%
  mutate(next_Warming_start = ymd(next_Warming_start))

(season_fig <- ggplot(dat_wide, aes(y = Site_ID)) +
  # warming season
  geom_linerange(aes(xmin = Warming_start,
                     xmax = Leaf_start),
                 size = 5,
                 color = "#FFAA00") +
  # leaf season
  geom_linerange(aes(xmin = Leaf_start,
                     xmax = Cooling_start),
                 size = 5,
                 color = "#609048") +
  # cooling season
  geom_linerange(aes(xmin = Cooling_start,
                     xmax = Snow_start),
                 size = 5,
                 color = "#D46F10") +
  # snow season
  geom_linerange(aes(xmin = Snow_start,
                     xmax = next_Warming_start), # CAREFUL HERE
                 size = 5,
                 color = "#69B9FA") +
  labs(x = "Date", y = "Site") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw())

# Save plot.
# ggsave(plot = season_fig,
#        filename = "figures/seasons_pheno_050924.jpg",
#        width = 20,
#        height = 10,
#        units = "cm")

#### Compiled Season Durations ####

# Create dataset with "start" and "end" columns for
# each of the following seasons:
# Warming
# Warming to Peak
# Leaf
# Cooling
# Snow

dat_seasons <- dat_long %>%
  select(Site_ID, Year, Season, Date) %>%
  # arrange in order of date
  arrange(Date) %>%
  # group by site
  group_by(Site_ID) %>%
  # removing peak dates for now because this will complicate
  # the lag steps
  filter(Season != "Peak") %>%
  rename("Start_Date" = "Date") %>%
  # make a new leading values column
  mutate(End_Date = lead(Start_Date)) %>%
  ungroup() # omg yay!

# Now, to make beginning of warming season to peak emergence data.
dat_peak_only <- dat_long %>%
  select(Site_ID, Year, Season, Date) %>%
  # Removing leaf season so it calculates the leading date
  # from start of warming to peak dates
  filter(Season != "Leaf") %>%
  arrange(Date) %>%
  group_by(Site_ID) %>%
  # keeping in Peak dates now, filter later
  rename("Start_Date" = "Date") %>%
  # make a new leading values column
  mutate(End_Date = lead(Start_Date)) %>%
  ungroup() %>%
  # filter for "Warming" which begins with warming season
  # and ends with peak emergence
  filter(Season == "Warming") %>%
  select(-Season) %>% # but also remove this column
  # so that we can rename it properly
  mutate(Season = "Warming to Peak")

dat_seasons <- full_join(dat_seasons, dat_peak_only) %>%
  arrange(End_Date, Site_ID)

# Export as csv so can select only for years of interest
write_csv(dat_seasons, "data_working/season_dates_pheno_wpeak_051724.csv")

# End of script.
