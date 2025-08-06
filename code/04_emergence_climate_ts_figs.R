### Time Series Figures of Emergence Alongside Conditions
### August 6, 2025
### Heili Lowman

#### README ####

# The following script will create figures to showcase the
# most/least productive years alongside concurrent stream
# conditions.

"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D"

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)
library(lubridate)
library(scales)

# Load data.
insect_dat <- readRDS("data_working/aquatic_counts_complete_yrs_071825.rds")
stream_dat <- readRDS("data_working/stream_climate_qchem.rds")

#### Tidy ####

# Trim only to black flies and summarize by week.
# Summarize by order.
dat_dipt <- insect_dat %>%
  filter(Order == "dipteran") %>%
  group_by(watershed, year, Date) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

# Join with stream data.
dat_all <- left_join(stream_dat, dat_dipt,
                     by = join_by("WS" == "watershed",
                                  "DATE" == "Date"))

#### Plot ####

##### Fig 1 #####

# First figure will compare W5 & W6 in 2019
# to demonstrate variation among watersheds in a given year

# I will initially make a revised dataset that I can use
# to plot the previous year and the current year's data
# simultaneously. - NB: Chose not to go this route because
# the figure gets way too messy.

# dat_2019 <- dat_all %>%
#   filter(DATE < "2020-01-01" &
#            DATE > "2018-12-31") %>%
#   mutate(group = "present")
# 
# dat_2018 <- dat_all %>%
#   filter(DATE < "2019-01-01" &
#            DATE > "2017-12-31") %>%
#   mutate(DATE_plus1 = DATE + years(1)) %>%
#   select(-DATE) %>%
#   rename(DATE = DATE_plus1) %>%
#   mutate(group = "minus1yr")
# 
# dat_manip <- full_join(dat_2019, dat_2018)

(fig_compare1a <- ggplot(dat_all %>%
                          filter(WS == "5") %>%
                          filter(DATE < "2020-01-01" &
                                DATE > "2018-12-31") %>%
                          drop_na(total_count), 
                         aes(x = DATE, y = total_count)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%b-%Y"),
               limits = as.Date(c('2019-01-01','2020-01-01'))) +
  ylim(0, 4000) +
  labs(y = "Weekly Emergence",
       title = "Watershed 5") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 20),
        legend.position = "none"))

(fig_compare1b <- ggplot(dat_all %>%
                           filter(WS == "6") %>%
                           filter(DATE < "2020-01-01" &
                                    DATE > "2018-12-31") %>%
                           drop_na(total_count), 
                         aes(x = DATE, y = total_count)) +
    geom_line(linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 4000) +
    labs(title = "Watershed 6") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1c <- ggplot(dat_all %>%
                           filter(WS == "5") %>%
                           filter(DATE < "2020-01-01" &
                                    DATE > "2018-12-31"),
                         aes(x = DATE, y = Streamflow)) +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 80) +
    labs(y = "Discharge (mm/day)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1d <- ggplot(dat_all %>%
                           filter(WS == "6") %>%
                           filter(DATE < "2020-01-01" &
                                    DATE > "2018-12-31"), 
                         aes(x = DATE, y = Streamflow)) +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 80) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1e <- ggplot(dat_all %>%
                           filter(WS == "5") %>%
                           filter(DATE < "2020-01-01" &
                                    DATE > "2018-12-31") %>%
                           drop_na(temp), 
                         aes(x = DATE, y = temp)) +
    geom_line(color = "#D95F02", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 20) +
    labs(y = "Temperature (°C)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1f <- ggplot(dat_all %>%
                           filter(WS == "6") %>%
                           filter(DATE < "2020-01-01" &
                                    DATE > "2018-12-31") %>%
                           drop_na(temp), 
                         aes(x = DATE, y = temp)) +
    geom_line(color = "#D95F02", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 20) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1g <- ggplot(dat_all %>%
                           filter(WS == "5") %>%
                           filter(DATE < "2020-01-01" &
                                    DATE > "2018-12-31") %>%
                           drop_na(chla_T), 
                         aes(x = DATE, y = chla_T)) +
    geom_line(color = "#66A61E", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 10) +
    labs(x = "Date", y = "Chlorophyll a (mg/L)") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1h <- ggplot(dat_all %>%
                           filter(WS == "6") %>%
                           filter(DATE < "2020-01-01" &
                                    DATE > "2018-12-31") %>%
                           drop_na(chla_T), 
                         aes(x = DATE, y = chla_T)) +
    geom_line(color = "#66A61E", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 10) +
    labs(x = "Date") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

# Pulling the first figure together.
(fig_compare1 <- (fig_compare1a + fig_compare1b) /
    (fig_compare1c + fig_compare1d) /
    (fig_compare1e + fig_compare1f) /
    (fig_compare1g + fig_compare1h))

# Export figure.
# ggsave(plot = fig_compare1,
#        filename = "figures/w5_w6_2019_080625.jpg",
#        width = 30,
#        height = 30,
#        units = "cm")

##### Fig 2 #####

# Second figure will demonstrate variation within a watershed across years.
# This will instead present 2020-2022 for Watershed 5 alone.

# In order to have the lines plot appropriately, I will add a column to
# group by year.
dat_all <- dat_all %>%
  mutate(Year = year(DATE)) %>%
  mutate(month = month(DATE)) %>%
  mutate(period = case_when(month < 10 ~ "early",
                            TRUE ~ "late"))

(fig_compare2a <- ggplot(dat_all %>%
                           filter(WS == "5") %>%
                           filter(DATE < "2023-01-01" &
                                    DATE > "2019-12-31") %>%
                           drop_na(total_count), 
                         aes(x = DATE, y = total_count,
                             color = period,
                             group = Year)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("black", "gray70")) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Weekly Emergence") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare2b <- ggplot(dat_all %>%
                           filter(WS == "5") %>%
                           filter(DATE < "2023-01-01" &
                                    DATE > "2019-12-31"), 
                         aes(x = DATE, y = Streamflow)) +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Q (mm/day)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare2c <- ggplot(dat_all %>%
                           filter(WS == "5") %>%
                           filter(DATE < "2023-01-01" &
                                    DATE > "2019-12-31") %>%
                           drop_na(temp), 
                         aes(x = DATE, y = temp)) +
    geom_line(color = "#D95F02", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Temperature (°C)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare2d <- ggplot(dat_all %>%
                           filter(WS == "5") %>%
                           filter(DATE < "2023-01-01" &
                                    DATE > "2019-12-31") %>%
                           drop_na(chla_T), 
                         aes(x = DATE, y = chla_T,
                             group = Year)) +
    geom_line(color = "#66A61E", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(x = "Date", y = "chl. a (mg/L)") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Pulling the first figure together.
(fig_compare2 <- (fig_compare2a) /
    (fig_compare2b) /
    (fig_compare2c) /
    (fig_compare2d))

# Export figure.
# ggsave(plot = fig_compare2,
#        filename = "figures/w5_20_21_22_080625.jpg",
#        width = 45,
#        height = 27,
#        units = "cm")

#### Temp at Peak Emergence ####

# Slice entire dataset to include only peaks by year and site.
dat_early_peaks <- dat_all %>%
  filter(month < 10) %>%
  group_by(WS, Year) %>%
  slice_max(total_count, na_rm = TRUE) %>%
  ungroup() %>%
  mutate(keep = case_when(duplicate == "Dup" ~ "No",
                          WS == 1 & Year %in% c(2018,2019) |
                            WS == 2 & Year %in% c(2018,2019) |
                            WS == 3 & Year %in% c(2018,2019) |
                            WS == 4 & Year %in% c(2018,2019) |
                            WS == 5 & Year %in% c(2018,2019, 2020,
                                                  2021, 2022, 2023, 2024) |
                            WS == 6 & Year %in% c(2018,2019, 2020,
                                                  2021, 2022, 2023, 2024) |
                            WS == 9 & Year %in% c(2018,2019) ~ "Yes",
                          TRUE ~ NA)) %>%
  filter(keep == "Yes") %>%
  drop_na(keep)

mean(dat_early_peaks$temp, na.rm = TRUE) # 10.84
sd(dat_early_peaks$temp, na.rm = TRUE) # 1.52

# End of script.
