### Discharge Data Exploration
### April 17, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF discharge data.

# Data was downloaded on April 17, 2024 from 
# https://doi.org/10.6073/pasta/67df3a028faf70827b43f2a525190d9c

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(patchwork)

# Load data.
dat <- read_csv("data_raw/HBEF_DailyStreamflow_1956-2023.csv")
# Streamflow is in mm/day.

#### Tidy ####

# Format date column.
dat$date <- ymd(dat$DATE)

# And create dataset for only years of interest.
dat_post17 <- dat %>%
  filter(date > ymd("2017-01-01")) %>%
  # And label low-flows
  mutate(low = factor(case_when(Streamflow < 0.5 ~ "YES",
                         TRUE ~ "NO"),
                      levels = c("YES", "NO")))

# And a specifically low-flow dataset.
dat_post17_low <- dat_post17 %>%
  filter(Streamflow < 0.5)

#### Plot ####

# Initial plots
hist(dat$Streamflow)
hist(dat_post17$Streamflow)
hist(dat_post17_low$Streamflow)

# Quick plot of only W6 discharge with low flows colored
(fig_w6 <- ggplot(dat_post17 %>% filter(WS == 6) %>%
                    filter(date > ymd("2021-01-01")) %>%
                    filter(date < ymd("2022-01-01")),
            aes(x = date, y = Streamflow)) +
  geom_point(aes(color = low)) +
  scale_color_manual(values = c("#e05959","#045CB4")) +
  theme_bw() +
  theme(legend.position = "none"))

# Now create a plot of all watersheds.
(fig_wall <- ggplot(dat_post17 %>%
                      filter(WS %in% c(1,2,3,4,5,6,9)) %>%
                      filter(date < ymd("2023-12-31")),
                    aes(x = date, y = Streamflow)) +
    geom_point(alpha = 0.3,
               aes(color = low)) +
    scale_color_manual(values = c("#e05959","#045CB4")) +
    #scale_y_log10() +
    theme_bw() +
    facet_grid(WS~.) +
    theme(legend.position = "none"))

# Export figure.
# ggsave(plot = fig_wall,
#        filename = "figures/discharge_daily_041724.jpg",
#        width = 30,
#        height = 30,
#        units = "cm")

# Ok, so discharge is looking fairly similar across sites.
# Low flow time periods are most common in summer, except in 2021
# and 2023.

# So, let's create a chart of no flow days.
dat_post17 <- dat_post17 %>%
  mutate(Year = year(date),
         Month = month(date)) %>%
  # create a new season column for grouping
  # Nov - Mar belongs to winter (of the previous year)
  # Apr - Oct belongs to summer (of the same year)
  mutate(Season = case_when(
    Month %in% c(11,12) ~ paste("Winter",Year),
    Month %in% c(1,2,3) ~ paste("Winter",Year-1),
    TRUE ~ paste("Summer",Year)))

# And aggregate by watershed and season
dat_lowflow_days <- dat_post17 %>%
  group_by(WS, Season, low) %>%
  summarize(n_days = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = low, values_from = n_days)

# Use only "yes" values to create a more condensed table
dat_lowflow_only <- dat_lowflow_days %>%
  select(WS, Season, YES) %>%
  pivot_wider(names_from = Season, values_from = YES)

# Ok, and let's try and visualize this in a meaningful way.
(fig_seasonal_lows <- ggplot(dat_lowflow_days %>%
  filter(WS %in% c(1,2,3,4,5,6,9)) %>%
    pivot_longer(cols = c(YES, NO)) %>%
    mutate(name = factor(name, levels = c("YES", "NO"))) %>%
    mutate(season = factor(as.character(Season),
                           levels = c("Summer 2017",
                                      "Winter 2017",
                                      "Summer 2018",
                                      "Winter 2018",
                                      "Summer 2019",
                                      "Winter 2019",
                                      "Summer 2020",
                                      "Winter 2020",
                                      "Summer 2021",
                                      "Winter 2021",
                                      "Summer 2022",
                                      "Winter 2022",
                                      "Summer 2023",
                                      "Winter 2023",
                                      "Summer 2024",
                                      "Winter 2024"))),
  aes(x = season, y = value, fill = name)) +
  geom_bar(stat="identity", 
           color="black", 
           position=position_dodge())+
  scale_color_manual(values = c("#e05959","#045CB4")) +
  theme_bw() +
  facet_grid(WS~.))

# Ahhh this is a bit of a mess.

# End of script.
