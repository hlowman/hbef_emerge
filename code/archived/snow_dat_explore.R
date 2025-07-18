### Snow Data Exploration
### April 17, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF snow data.

# Data was downloaded on April 17, 2024 from 
# https://doi.org/10.6073/pasta/5e1c5cb3aae2391cda9ca3ab4487046c

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(patchwork)

# Load data.
dat <- read_csv("data_raw/HBEF_snowcourse_1956-2023.csv")
# snow_depth is in mm

# "Maximum snow depth, snow water content, frost occurrence, and frost depth data are recorded at points along a transect known as a snow course, which includes 10 points spaced at 2-m intervals within a designated 0.25 ha area. Data from one course are averaged for each collection date."

# "During the period from 1967 to 1984, the network was reduced to 2 snow courses on the north-facing slope (STA17 and STA19) and two snow courses on the south-facing slope (STA2 and STA9), along with a snow course at the headquarters location (STAHQ). Ongoing data collection occurs at these five snow courses."

#### Tidy ####

# Trim only to ongoing collection sites and years of interest.
dat_trim <- dat %>%
  mutate(date = ymd(Date)) %>%
  filter(Site %in% c("STA2", "STA9", "STA17", "STA19", "STAHQ")) %>%
  mutate(slope = case_when(Site %in% c("STA2", "STA9") ~ "South",
                           Site %in% c("STA17", "STA19") ~ "North",
                           TRUE ~ "HQ")) %>%
  filter(date > ymd("2017-01-01")) %>%
  # and remove missing values
  filter(snow_depth > 0)

#### Plot ####

(fig_snow <- ggplot(dat_trim, aes(x = date, 
                                 y = snow_depth,
                                 color = Site)) +
  geom_point() +
  labs(x = "Date", y = "Snow Depth (mm)") +
  facet_grid(slope~.) +
  scale_color_manual(values = cal_palette("sbchannel")) +
  theme_bw() +
  theme(legend.position = "none"))

# Export figure.
# ggsave(plot = fig_snow,
#        filename = "figures/snow_weekly_041724.jpg",
#        width = 20,
#        height = 10,
#        units = "cm")

# Ok, so dates of first collection on the south-facing slopes, at 
# least match almost to the day each year, so this might be a good
# indicator of start of winter.
# Ranging from November 19 in 2018 to December 19 in 2022.

# Also, know this is not as accurate a measure of the end of winter
# but last dates at STA2 range from March 23 in 2020 to 
# April 16 in 2018.

# So winter is going to span max November to April.

# End of script.
