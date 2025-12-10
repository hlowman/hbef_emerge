### Temperature Data Prep
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data tidying
# of the stream temperature data.

# The challenge here is that temperature data is collected by
# various parties, so I will be doing the following to 
# harmonize it (centering about the FS data, since it is the
# richest record):

# - Load in data collected by the FS (2012-2022) & HBWatER (2020-present)
# - Calculate the offset between the two for 2020-2022 in W6
# - Apply a correction to the HB data for the 2022-2024 years in W6
# This will complete the W6 record, keeping the 2018-2021 data intact
# - Calculate the offset between W6 and W5 for 2018-2022 FS data
# - Use the correction to estimate the W5 data for the 2022-2024 years
# - And finally calculate daily mean temperatures and fill gaps

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(zoo)

# Load data.
temp_fs <- read_csv("data_raw/HBEF_streamtemp_roughlyCleaned.csv") # from Danielle/Nina, 5 minute intervals
temp_hbwater <- read_csv("data_raw/hbef_temp.csv") # from HBWatER, 15 minute intervals
chem <- read_csv("data_raw/HubbardBrook_weekly_stream_chemistry_1963-2024.csv") # from EDI

#### Tidy ####

# Found a suite of duplicates from HB Water data for some reason,
# so first need to remove those.
temp_hbwater_nodup <- unique(temp_hbwater)
# No clue why those are in there.

#### Offset Calculation W6 FS/HBWatER ####

# Trim down datasets to include only w6 in the overlapping years.
temp_fs6 <- temp_fs %>%
  select(TIMESTAMP, Streamtemp_W6) %>%
  drop_na(Streamtemp_W6) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, tz = "America/New_York",
                                # MUST SET TIMEZONE, otherwise this gets all wonky
                                format = "%m/%d/%Y %H:%M"))

temp_fs6_15 <- temp_fs6 %>%
  # also rounding time to nearest 15 minutes to better join with HBWatER data
  mutate(TIMESTAMP_15 = round_date(TIMESTAMP, unit = "15 mins")) %>%
  group_by(TIMESTAMP_15) %>%
  summarize(tempC = mean(Streamtemp_W6, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(tempC_FS = tempC)

# And make sure HBWatER timezone matches FS.
print(temp_fs6_15$TIMESTAMP_15[1])
print(temp_hbwater_nodup$datetime[1])

temp_hbwater6 <- temp_hbwater_nodup %>%
  # need to fix date
  mutate(datetime_ed = as.POSIXct(as.character(datetime), tz = "America/New_York",
                                  # MUST SET TIMEZONE, otherwise this gets all wonky
                                  format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(watershedID == 6) %>%
  mutate(datetime_15 = round_date(datetime_ed, unit = "15 mins")) %>%
  select(datetime_15, tempC) %>%
  # and need to remove the strange missingness delineator
  filter(!tempC == "\\N") %>%
  mutate(tempC = as.numeric(tempC)) %>%
  rename(tempC_HBWatER = tempC) %>%
  select(datetime_15, tempC_HBWatER)

print(temp_hbwater6$datetime_15[1]) # Ok, phew.

# Joined dates span April 2020 through August 2022
temp_both <- inner_join(temp_fs6_15, temp_hbwater6, by = c("TIMESTAMP_15" = "datetime_15")) %>%
  mutate(diff = tempC_FS - tempC_HBWatER)

# Plot stream temperature.
ggplot(temp_both, 
       aes(x = tempC_HBWatER,
           y = tempC_FS)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(x = "HBWatER Stream Temperature",
       y = "USFS Stream Temperature") +
  theme_bw()
# Appears to be just shy of 1:1.
# Also the strong deviations tend to happen
# most frequently September of 2020.

ggplot(temp_both, aes(x = TIMESTAMP_15, y = diff)) + geom_point()
# The difference really skyrockets in September 2020, so
# I will remove these dates before proceeding

temp_both_ed <- temp_both %>%
  mutate(Year = year(TIMESTAMP_15),
         Month = month(TIMESTAMP_15)) %>%
  mutate(remove = case_when(Year == 2020 & Month == 9 ~ "Yes",
                            TRUE ~ "No")) %>%
  filter(remove == "No")

# Fit a linear model to determine the offset.
# Using FS as response, since that is what we will be estimating.
lm_temp <- lm(tempC_FS ~ tempC_HBWatER, data = temp_both_ed)

# Examine model output.
summary(lm_temp)
# y = 0.9018772x + 0.4802097
# Multiple R^2 = 0.9822
# p < 2.2e-16

# Examine model residuals.
plot(lm_temp)

# Trying the same model with log-transformed values.
lm_temp2 <- lm(log(tempC_FS) ~ log(tempC_HBWatER), data = temp_both_ed)
summary(lm_temp2)
plot(lm_temp2)
# Residuals look the same (minus 1 outlier). There's no need to 
# overcomplicate things here, so using the original formula.

#### Correction to HB W6 data ####

# Trim HB data to the dates that it needs to be corrected for.
temp_hb6_corrected <- temp_hbwater6 %>%
  dplyr::filter(datetime_15 >= as.POSIXct("2022-08-12 00:00:00",
                                          tz = "America/New_York",
                                          format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(Temp_corr = (0.9018772*tempC_HBWatER) + 0.4802097)

# Assemble full W6 record.
temp_hb6_corrected$Source <- "HBWatER"
temp_fs6$Source <- "FS" 
# remembering to switch back to using 5 min record 
# so aggregates properly when calculating daily means

temp_hb6_corrected_trim <- temp_hb6_corrected %>%
  filter(datetime_15 < as.POSIXct("2025-01-01 00:00", tz = "America/New_York",
                                  format = "%Y-%m-%d %H:%M")) %>%
  rename(datetime = datetime_15,
         TempC = Temp_corr) %>%
  select(datetime, TempC, Source)

temp_fs6_trim <- temp_fs6 %>%
  filter(TIMESTAMP < as.POSIXct("2022-08-12 00:00", tz = "America/New_York",
                                format = "%Y-%m-%d %H:%M")) %>%
  filter(TIMESTAMP > as.POSIXct("2017-12-31 23:59", tz = "America/New_York",
                                   format = "%Y-%m-%d %H:%M")) %>%
  # double check dates to be sure this filtered correctly
  rename(datetime = TIMESTAMP,
         TempC = Streamtemp_W6) %>%
  select(datetime, TempC, Source)

temp_fullrecord_w6 <- rbind(temp_fs6_trim,
                            temp_hb6_corrected_trim)

#### Offset Calculation W6/W5 ####

# Trim down recent FS dataset to include only instances
# of overlap for watersheds 5 & 6.
temp_fs56_overlap <- temp_fs %>%
  select(TIMESTAMP, Streamtemp_W5, Streamtemp_W6) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, tz = "America/New_York",
                                # MUST SET TIMEZONE, otherwise this gets all wonky
                                format = "%m/%d/%Y %H:%M")) %>%
  # and now filter down to period of interest (2018-2022)
  filter(TIMESTAMP < as.POSIXct("2022-08-12 00:00", tz = "America/New_York",
                                format = "%Y-%m-%d %H:%M") &
           TIMESTAMP > as.POSIXct("2017-12-31 23:59", tz = "America/New_York",
                                  format = "%Y-%m-%d %H:%M"))

# First, plot stream temperature in W5 as a function of W6.
ggplot(temp_fs56_overlap, 
       aes(x = Streamtemp_W6,
           y = Streamtemp_W5)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(x = "W6 Stream Temperature",
       y = "W5 Stream Temperature") +
  theme_bw()

# Seems to be nearly a 1:1 relationship, but let's check to see where 
# the greatest deviations are.
temp_fs56_overlap <- temp_fs56_overlap %>%
  mutate(diff = Streamtemp_W5 - Streamtemp_W6)
# Many issues with june and september, so removing those
# prior to generating the relationship below.

# Fit a linear model to determine the offset to be applied
# in order to generate the record for W5 using the W6 data.
lm_temp3 <- lm(Streamtemp_W5 ~ Streamtemp_W6, data = temp_fs56_overlap %>%
                 filter(abs(diff) < 5))

# Examine model output.
summary(lm_temp3)
# y = 0.9434919x + 0.3723405
# Multiple R^2 = 0.9903
# p < 2.2e-16

# Examine model residuals.
plot(lm_temp3)

# Trying the same model with log-transformed values.
lm_temp4 <- lm(log(Streamtemp_W5) ~ log(Streamtemp_W6), 
               data = temp_fs56_overlap %>%
                 filter(Streamtemp_W5 > 0) %>%
                 filter(Streamtemp_W6 > 0) %>%
                 filter(abs(diff) < 5))
summary(lm_temp4)
plot(lm_temp4)
# Residuals look worse, and there's no need to overcomplicate
# things here, so using the original formula.

#### Correction to generate W5 data ####

# Using the latter part of the W6 record generated above.
temp_w5_generated <- temp_fullrecord_w6 %>%
  filter(datetime >= as.POSIXct("2022-08-12 00:00", tz = "America/New_York",
                                format = "%Y-%m-%d %H:%M")) %>%
  rename(TempC_W6 = TempC,
         Source_W6 = Source) %>%
  mutate(TempC = 0.9434919*TempC_W6 + 0.3723405) %>% 
  select(datetime, TempC) %>%
  mutate(Source = "HBWatER_W6offset")

# Bring the first part of the FS record for W5.
temp_fs5 <- temp_fs %>%
  select(TIMESTAMP, Streamtemp_W5) %>%
  drop_na(Streamtemp_W5) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, tz = "America/New_York",
                                # MUST SET TIMEZONE, otherwise this gets all wonky
                                format = "%m/%d/%Y %H:%M")) %>%
  filter(TIMESTAMP > as.POSIXct("2017-12-31 23:59", tz = "America/New_York",
                               format = "%Y-%m-%d %H:%M") &
           TIMESTAMP <= as.POSIXct("2022-08-11 23:59", tz = "America/New_York",
                                 format = "%Y-%m-%d %H:%M")) %>%
  rename(datetime = TIMESTAMP,
         TempC = Streamtemp_W5) %>%
  mutate(Source = "FS") %>%
  select(datetime, TempC, Source)

# And join together to create the full W5 dataset.
temp_fullrecord_w5 <- rbind(temp_fs5,
                            temp_w5_generated)

#### Daily Mean ####

# Join together the two datasets.
temp_fullrecord_w5$watershed <- 5
temp_fullrecord_w6$watershed <- 6

temp_fullrecord <- full_join(temp_fullrecord_w5, temp_fullrecord_w6)

# Aggregate daily data and export that as well.
temp_fullrecord_daily <- temp_fullrecord %>%
  mutate(date = date(datetime)) %>%
  group_by(date, watershed) %>%
  summarize(daily_TempC = mean(TempC, na.rm = TRUE)) %>%
  ungroup()

# And a quick plot to examine the data itself
# to look for outliers
ggplot(temp_fullrecord_daily, aes(x = date,
                                  y = daily_TempC)) +
  geom_point() +
  facet_wrap(.~watershed) +
  theme_bw() # looking alright

#### Gap Fill ####

# Check to be sure no days are missing.
temp_fullrecord_daily <- temp_fullrecord_daily %>%
  group_by(watershed) %>%
  mutate(diff_date = (interval(date, lag(date))) %/% days(1)) %>%
  ungroup()
# Ok, there are a few instances, so I'll chose to interpolate.

high_missingness <- temp_fullrecord_daily %>%
  filter(diff_date < -6)

# Based on the above missingness, the periods I should interpret
# with care are August 2023, October/November 2023, April 2024,
# and December 2024.

# First, need to create a full record of dates to join by.
all_dates <- seq(as.Date("2018-01-01"), as.Date("2024-12-31"), 
                 by = "day")

df <- as.data.frame(all_dates) %>%
  rename(date = all_dates)

# And make the full record wide format.
temp_fullrecord_daily_wide <- temp_fullrecord_daily %>%
  select(-diff_date) %>%
  pivot_wider(names_from = watershed,
              values_from = daily_TempC)

temp_fullrecord_daily_all <- left_join(df, temp_fullrecord_daily_wide)

# Percent missing in both cases.
sum(is.na(temp_fullrecord_daily_all$`5`)) # 127 days or 130/2557 = 5%
sum(is.na(temp_fullrecord_daily_all$`6`)) # 171 days or 171/2557 = 7%

# Using a linear interpolation.
temp_fullrecord_daily_all <- temp_fullrecord_daily_all %>%
  arrange(date) %>%
  mutate(daily_TempC_W5_interp = na.approx(`5`),
         daily_TempC_W6_interp = na.approx(`6`)) %>%
  ungroup()

#### Export ####

# Export files for later use.
# saveRDS(temp_fullrecord,
#         "data_working/stream_temp_W5_W6_2018_2024.rds")
# saveRDS(temp_fullrecord_daily_all,
#         "data_working/stream_temp_daily_W5_W6_2018_2024.rds")

# End of script.
