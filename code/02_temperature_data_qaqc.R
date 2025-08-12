### Temperature Data Prep
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data tidying
# of the stream temperature data.

# The challenge here is that temperature data is collected by
# various parties, so I will be doing the following to 
# harmonize it:

# - Load in data collected by the FS (2012-2021) & HBWatER (2020-present)
# - Calculate the offset between the two for 2020 and 2021 in W6
# - Apply a correction to the FS data for the 2018-2020 years in W6
# - Load in the historic weekly data collected by HBWatER (1960s-present)
# - Calculate the offset between W6 and W5 for the full record
# - Apply a correction to the W5 data for the years 2018 through 2024

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

#### Offset Calculation FS/HBWatER ####

# Trim down datasets to include only w6 in the overlapping years.
temp_fs6 <- temp_fs %>%
  select(TIMESTAMP, Streamtemp_W6) %>%
  drop_na(Streamtemp_W6) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%m/%d/%Y %H:%M")) %>%
  # also rounding time to nearest 15 minutes to better join with the HBWatER data
  mutate(TIMESTAMP_15 = round_date(TIMESTAMP, unit = "15 mins")) %>%
  group_by(TIMESTAMP_15) %>%
  summarize(tempC = mean(Streamtemp_W6, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(tempC_FS = tempC)

temp_hbwater6 <- temp_hbwater %>%
  filter(watershedID == 6) %>%
  mutate(datetime_15 = round_date(datetime, unit = "15 mins")) %>%
  select(datetime_15, tempC) %>%
  # and need to remove the strange missingness delineator
  filter(!tempC == "\\N") %>%
  mutate(tempC = as.numeric(tempC)) %>%
  rename(tempC_HBWatER = tempC)

# Joined dates span April 2020 through August 2022
temp_both <- inner_join(temp_fs6, temp_hbwater6, by = c("TIMESTAMP_15" = "datetime_15")) %>%
  mutate(diff = tempC_FS - tempC_HBWatER)

# Plot stream temperature in W5 as a function of W6.
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
lm_temp <- lm(tempC_HBWatER ~ tempC_FS, data = temp_both_ed)

# Examine model output.
summary(lm_temp)
# y = 1.0781841x - 0.3041381
# Multiple R^2 = 0.9788
# p < 2.2e-16

# Examine model residuals.
plot(lm_temp)

# Trying the same model with log-transformed values.
lm_temp2 <- lm(log(tempC_HBWatER) ~ log(tempC_FS), data = temp_both_ed)
summary(lm_temp2)
plot(lm_temp2)
# Residuals look worse, and there's no need to overcomplicate
# things here, so using the original formula.

#### Correction to FS W6 data ####

# Trim FS data to the dates that it needs to be corrected for.
# Working from the original dataset so that I'm not averaging
# twice (from 5min to 15min to daily).
temp_fs6_corrected <- temp_fs %>%
  select(TIMESTAMP, Streamtemp_W6) %>%
  drop_na(Streamtemp_W6) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%m/%d/%Y %H:%M")) %>%
  filter(TIMESTAMP > as.POSIXct("12/31/2017 23:59", format = "%m/%d/%Y %H:%M")) %>%
  filter(TIMESTAMP < as.POSIXct("04/23/2020 14:01", format = "%m/%d/%Y %H:%M")) %>%
  mutate(Temp_corr = 1.0781841*Streamtemp_W6 - 0.3041381)

# Assemble full W6 record.
temp_fs6_corrected$Source <- "FS"
temp_hbwater6$Source <- "HBWatER"

temp_fs6_corrected_trim <- temp_fs6_corrected %>%
  rename(datetime = TIMESTAMP,
         TempC = Temp_corr) %>%
  select(datetime, TempC, Source)

temp_hbwater6_trim <- temp_hbwater6 %>%
  rename(datetime = datetime_15,
         TempC = tempC_HBWatER)

temp_fullrecord_w6 <- rbind(temp_fs6_corrected_trim,
                            temp_hbwater6_trim)

#### Offset Calculation W6/W5 ####

# Trim down historical dataset to include only watersheds 5 & 6.
chem_trim <- chem %>%
  filter(site %in% c("W5", "W6")) %>%
  select(site, date, temp)

# Average out instances of duplicates
chem_wide <- chem_trim %>%
  group_by(site, date) %>%
  summarize(temp = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = "site",
              values_from = "temp")

# First, plot stream temperature in W5 as a function of W6.
ggplot(chem_wide, 
       aes(x = W6,
           y = W5)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(x = "W6 Stream Temperature",
       y = "W5 Stream Temperature") +
  theme_bw()

# Seems to be nearly a 1:1 relationship, but let's check to see where 
# the greatest deviations are.
chem_wide <- chem_wide %>%
  mutate(diff = W5- W6)
# Lots of positive deviations in the 80s, following the removal (168 >1).
# Fewer negative deviations at various times (<100 >-1).
# But none as large as the ones we saw with the FS instrument, which
# leads me to believe even more that that was an instrumental issue.

# Fit a linear model to determine the offset to be applied
# in order to generate the long-term record for W5 using the
# W6 data.
lm_temp3 <- lm(W5 ~ W6, data = chem_wide)

# Examine model output.
summary(lm_temp3)
# y = 1.000574x + 0.044111
# Multiple R^2 = 0.9847
# p < 2.2e-16

# Examine model residuals.
plot(lm_temp)

# Trying the same model with log-transformed values.
lm_temp4 <- lm(log(W5) ~ log(W6), data = chem_wide)
summary(lm_temp4)
plot(lm_temp4)
# Residuals look weirder, and there's no need to overcomplicate
# things here, so using the original formula.

#### Correction to generate W5 data ####

# Using the full W6 record generated above.
temp_fullrecord <- temp_fullrecord_w6 %>%
  rename(TempC_W6 = TempC,
         Source_W6 = Source) %>%
  mutate(TempC_W5 = 1.000574*TempC_W6 + 0.044111)

# Aggregate daily data and export that as well.
temp_fullrecord_daily <- temp_fullrecord %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(daily_TempC_W5 = mean(TempC_W5),
            daily_TempC_W6 = mean(TempC_W6)) %>%
  ungroup()

#### Gap Fill ####

# Check to be sure no days are missing.
temp_fullrecord_daily <- temp_fullrecord_daily %>%
  mutate(diff_date = (interval(date, lag(date))) %/% days(1))
# Ok, there are a few instances, so I'll chose to interpolate.

high_missingness <- temp_fullrecord_daily %>%
  filter(diff_date < -6)

# Based on the above missingness, the periods I should interpret
# with care are spring 2021 and spring 2024, which displayed
# higher missingness in those years.

# First, need to create a full record of dates to join by.
all_dates <- seq(as.Date("2018-01-01"), as.Date("2024-12-31"), 
                 by = "day")

df <- as.data.frame(all_dates) %>%
  rename(date = all_dates)

temp_fullrecord_daily <- left_join(df, temp_fullrecord_daily)

# Using a linear interpolation.
temp_fullrecord_daily <- temp_fullrecord_daily %>%
  mutate(daily_TempC_W5_interp = na.approx(daily_TempC_W5),
         daily_TempC_W6_interp = na.approx(daily_TempC_W6))

#### Export ####

#saveRDS(temp_fullrecord, "data_working/stream_temp_W5_W6_2018_2025.rds")
#saveRDS(temp_fullrecord_daily, "data_working/stream_temp_daily_W5_W6_2018_2025.rds")

# End of script.
