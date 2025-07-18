### Weir Data Exploration
### May 1, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF weir data.

# Data was downloaded on May 1, 2024 from 
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hbr&identifier=68.

# Additional data was provided by Amey Bailey.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(viridis)
library(patchwork)

# Load data.
dat <- read_csv("data_raw/sdmskgha.csv")

dat_w1 <- read_csv("data_raw/sediment_weir_1.csv")
dat_w5 <- read_csv("data_raw/sediment_weir_5.csv")
dat_w6 <- read_csv("data_raw/sediment_weir_6.csv")

#### Plot ####

# Transform data long-ways.
dat_long <- dat %>%
  pivot_longer(cols = `WS-1`:`WS-8`) %>%
  # and remove NAs
  filter(value > 0)

(weir_plot <- ggplot(dat_long, aes(x = YEAR, y = value)) +
    geom_point(aes(color = name)) +
    geom_line(aes(color = name)) +
    labs(x = "Year", y = "Soil Material Lost (Kg/Ha)") +
    theme_bw() +
    scale_color_viridis(discrete = TRUE))

# 1975 is when collection in watersheds other than 2 and 6 started.
# W8 has been discontinued.
# As of 1999, looks like only W1, W5, and W6 are monitored.
# I wonder, have they changed the methods? Because 1970 was a banner
# year for W2, but no other site has come close since.

# Export figure.
# ggsave(plot = weir_plot,
#        filename = "figures/weir_sed_050124.jpg",
#        width = 20,
#        height = 10,
#        units = "cm")

# Don't immediately notice any long-term trends.

# High years - 1987, 1996
# Low years - 1983, 1988, 1992, 1995, 1997

dat_w1 <- dat_w1 %>%
  drop_na(`start date`) %>%
  mutate(Collection_Date = mdy(`end date`)) %>%
  mutate(Year = year(Collection_Date))
# huh it's struggling with dates prior to 68 for some
# reason, but I'm going to leave that for now

dat_w1_recent <- dat_w1 %>%
  filter(Year > 2016 & Year < 2023) %>%
  mutate(watershed = "W1")

# Replace text in organic column with NA.
dat_w1_recent[5,3] <- NA

# And summarize the two collections in 2017.
dat_w1_trim <- dat_w1_recent %>%
  mutate(organic = as.numeric(organic),
         mixed = as.numeric(mixed),
         mineral = as.numeric(mineral),
         total = as.numeric(total)) %>%
  group_by(watershed, Year) %>%
  summarize(sum_organic = sum(organic),
            sum_mixed = sum(mixed),
            sum_mineral = sum(mineral),
            sum_total = sum(total)) %>%
  ungroup()

dat_w5 <- dat_w5 %>%
  drop_na(`end date`) %>%
  mutate(Collection_Date = mdy(`end date`)) %>%
  mutate(Year = year(Collection_Date))
# warning - two years don't have dates, only years
# but otherwise the above still works

dat_w5_recent <- dat_w5 %>%
  filter(Year > 2016 & Year < 2023) %>%
  mutate(watershed = "W5")

# Replace text in organic column with NA.
dat_w5_recent[5,3] <- NA

# And summarize the two collections in 2017.
dat_w5_trim <- dat_w5_recent %>%
  mutate(organic = as.numeric(organic),
         mixed = as.numeric(mixed),
         mineral = as.numeric(mineral),
         total = as.numeric(total)) %>%
  group_by(watershed, Year) %>%
  summarize(sum_organic = sum(organic),
            sum_mixed = sum(mixed),
            sum_mineral = sum(mineral),
            sum_total = sum(total)) %>%
  ungroup()

dat_w6 <- dat_w6 %>%
  drop_na(`end date`) %>%
  mutate(Collection_Date = mdy(`end date`)) %>%
  mutate(Year = year(Collection_Date))
# warning - two years don't have dates, only years
# but otherwise the above still works

dat_w6_recent <- dat_w6 %>%
  filter(Year > 2016 & Year < 2023) %>%
  mutate(watershed = "W6")

# Replace text in organic column with NA.
dat_w6_recent[5,3] <- NA

# And summarize the two collections in 2017.
dat_w6_trim <- dat_w6_recent %>%
  mutate(organic = as.numeric(organic),
         mixed = as.numeric(mixed),
         mineral = as.numeric(mineral),
         total = as.numeric(total)) %>%
  group_by(watershed, Year) %>%
  summarize(sum_organic = sum(organic),
            sum_mixed = sum(mixed),
            sum_mineral = sum(mineral),
            sum_total = sum(total)) %>%
  ungroup()

# And join the above together.
dat_weirs <- full_join(dat_w1_trim, dat_w5_trim)
dat_weirs <- full_join(dat_weirs, dat_w6_trim)

# And make in long format for easier stacked bar plotting.
dat_weirs_long <- dat_weirs %>%
  select(-sum_total) %>%
  pivot_longer(cols = sum_organic:sum_mineral,
               names_to = "Fraction")

# Also going to calculate long-term averages.
dat_w1_avg <- dat_w1 %>%
  drop_na(total) %>%
  filter(total > 0)

mean(dat_w1_avg$total) # 43.53673

dat_w5_avg <- dat_w5 %>%
  drop_na(total) %>%
  filter(total > 0)

mean(dat_w5_avg$total) # 34.35862

dat_w6_avg <- dat_w6 %>%
  drop_na(total) %>%
  filter(total > 0)

mean(dat_w6_avg$total) # 14.41096

mean_dat <- as.data.frame(cbind(c("W1", "W5", "W6"), 
                                c(43.53673, 34.35862, 14.41096))) %>%
  mutate(V2 = as.numeric(V2)) %>%
  rename(watershed = V1,
         mean_total = V2)

(weir_barplot <- ggplot(dat_weirs_long, 
                        aes(x = Year, 
                            y = value,
                            fill = Fraction)) +
    geom_bar(position="stack", stat="identity") +
    geom_hline(data = mean_dat, aes(yintercept = mean_total),
               color = "#3F320D") +
    labs(x = "Year", y = "Dried Material (Kg/Ha)",
         caption = "Weir Pond Sediment Material Collection Data") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "top") +
    scale_fill_manual(labels = c("Mineral", "Mixed", "Organic"),
                      values = c("#DED4C8", "#AD6F4F", "#AEC96F")) +
    facet_grid(watershed~.))

# Export figure.
# ggsave(plot = weir_barplot,
#        filename = "figures/weir_recent_053124.jpg",
#        width = 15,
#        height = 15,
#        units = "cm")
    
# End of script.
  