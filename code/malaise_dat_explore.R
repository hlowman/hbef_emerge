### Malaise Trap Data Exploration
### March 25, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the malaise trap insect data (1972-2017).

# Data was provided by Nick Rodenhouse. Note, all samples are from Watershed 6.
# For additional information regarding sampling collection and processing,
# please see metadata documents compiled by N. Rodenhouse. Advice regarding
# data aggregation has instructed the following:
# - Do not use counts from 1995.
# - Include only "Main" plot samples. 
# - Exclude sites "Y5" and "Y7".

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)

# Load data.
dat <- read_csv("data_raw/Malaise_72_17_diptera_aquatics_other_final_1_20_21.csv")

#### Tidy ####

dat_main <- dat %>%
  filter(Year != 1995) %>%
  filter(Plot == "Main") %>%
  # Also excluding "EZ4" and "EZ8" because there are so few
  # samples at these sites.
  filter(Location %in% c("EZ2", "EZ6")) %>%
  # Filter out May and August dates per N. Rodenhouse's suggestion
  filter(Month %in% c(6,7)) %>%
  drop_na(`Date collected`) %>%
  mutate(Date = dmy(`Date collected`))

#### Plot ####

# Summarize by date first.
dat_main_daily <- dat_main %>%
  group_by(Plot, Location, Year, Month, Week, Date) %>%
  summarize(sum_Total_individ = sum(`Total individuals`, na.rm = TRUE)) %>%
  ungroup()

# Plot all plots separately.
(fig1 <- ggplot(dat_main_daily, aes(x = Date,
                                    y = sum_Total_individ,
                                    group = Year)) +
    geom_line(alpha = 0.6) +
    labs(x = "Date",
         y = "Total Individuals Counted") +
    facet_grid(Location ~ .) +
    theme_bw())

# Was there a large change in 1991?
dat_main_weekly <- dat_main_daily %>%
  group_by(Plot, Location, Year, Month, Week) %>%
  summarize(sum_Total_individ_w = sum(sum_Total_individ, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(decade = factor(case_when(Year < 1980 ~ "70s",
                                   Year >= 1980 & Year < 1990 ~ "80s",
                                   Year >= 1990 & Year < 2000 ~ "90s",
                                   Year >= 2000 & Year < 2010 ~ "00s",
                                   Year >= 2010 ~ "10s"),
                         levels = c("70s", "80s", "90s",
                                    "00s", "10s"))) %>%
  mutate(week = factor(Week))

dat_main_weekly_summ <- dat_main_weekly %>%
  group_by(Plot, Location, Week, decade) %>%
  summarize(mean_Total_individ_w = mean(sum_Total_individ_w, na.rm = TRUE),
            sd_Total_individ_w = sd(sum_Total_individ_w, na.rm = TRUE)) %>%
  ungroup()

# Let's visualize the data another way.
(fig2 <- ggplot(dat_main_weekly, aes(x = week,
                                    y = sum_Total_individ_w,
                                    color = decade)) +
    geom_boxplot() +
    scale_color_manual(values = cal_palette("lupinus")) +
    scale_y_log10() +
    labs(x = "Week of Year",
         y = "Total Individuals Counted") +
    facet_grid(Location ~ .) +
    theme_bw())

(fig3 <- ggplot(dat_main_weekly_summ, aes(x = Week,
                                     y = mean_Total_individ_w,
                                     color = decade)) +
    geom_line() +
    geom_pointrange(aes(ymin = mean_Total_individ_w - sd_Total_individ_w,
                        ymax = mean_Total_individ_w + sd_Total_individ_w)) +
    scale_color_manual(values = cal_palette("lupinus")) +
    scale_y_log10() +
    labs(x = "Week of Year",
         y = "Total Individuals Counted") +
    facet_grid(Location ~ .) +
    theme_bw())

# Export figure.
# ggsave(plot = fig3,
#        filename = "figures/malaise_emerge_032524.jpg",
#        width = 35,
#        height = 8,
#        units = "cm")

# End of script.
