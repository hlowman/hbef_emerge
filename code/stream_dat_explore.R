### Stream Data Exploration
### March 21, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF stream data.

# Data was downloaded from https://hbwater.org/restricted_QAQC/
# on March 21, 2024.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(calecopal)
library(patchwork)

# Load data.
dat <- read_csv("data_raw/HBEFdata_Current_2024-03-21.csv")

# And load tidied aquatic invert data.
dat_inv <- readRDS("data_working/aquatic_taxa_counts_032824.rds")

#### Tidy/Examine ####

# Add a properly formatted date column.
dat_8 <- dat %>%
  mutate(Date = mdy(date)) %>%
  # and filter only for watersheds of interest
  filter(site %in% c("W1", "W2", "W3", "W4", "W5", "W6", "W9", "HBK")) %>%
  # as well as dates of interest
  filter(Date > mdy(as.character("1/1/17")))

# First, just curious about the spread of data.
hist(dat$pH)
hist(dat$spCond)
hist(dat$temp)
hist(dat$TDN)
hist(dat$DOC)

#### Plot ####

# Overall figure
(fig1 <- ggplot(dat_8, aes(x = Date, 
                         y = temp,
                         color = site)) +
   geom_line() +
   labs(y = "Temperature (C)",
        x = "Date",
        caption = "Streamwater Temperature by Watershed") +
   scale_color_manual(values = cal_palette("sbchannel",
                                           n = 15,
                                           type = "continuous")) +
   facet_grid(site ~ .) +
   theme_bw())

# Add a column to delineate seasons (approx.) by which we'll aggregate.
dat_8 <- dat_8 %>%
  mutate(month = month(Date)) %>%
  mutate(season = factor(case_when(month %in% c(1,2,3,11,12) ~ "Winter",
                                   month %in% c(4,5) ~ "Spring",
                                   month %in% c(6,7,8) ~ "Summer",
                                   month %in% c(9,10) ~ "Fall"),
                         levels = c("Summer", "Fall",
                                    "Winter", "Spring")))

# Note, water year in this region starts June 1. (?)

# Now, to generate some aggregate statistics.
dat_summ <- dat_8 %>%
  mutate(WY = factor(waterYr)) %>%
  group_by(site, WY, season) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE),
            CV_temp = sd(temp, na.rm = TRUE)/mean(temp, na.rm = TRUE),
            mean_ght = mean(gageHt, na.rm = TRUE),
            CV_ght = sd(gageHt, na.rm = TRUE)/mean(gageHt, na.rm = TRUE)) %>%
  ungroup()

# And to plot
(fig2 <- ggplot(dat_summ, aes(x = season, 
                              y = CV_temp,
                              color = WY)) +
    geom_jitter(size = 2, width = 0.15, alpha = 0.8) +
    labs(y = "CV of Temperature (C)",
         x = "Season",
         caption = "Streamwater Temperature Variation by Watershed") +
    scale_color_manual(values = cal_palette("lake",
                                            n = 8,
                                            type = "continuous")) +
    facet_grid(site ~ .) +
    theme_bw())

# Appears variation in temperature is greatest in Winter followed by Spring,
# whereas Summer/Fall temperatures are fairly consistent across years.

# Export figure.
# ggsave(plot = fig2,
#        filename = "figures/temp_cv_032124.jpg",
#        width = 15,
#        height = 30,
#        units = "cm")

# And plot flow
(fig3 <- ggplot(dat_summ, aes(x = season, 
                              y = CV_ght,
                              color = WY)) +
    geom_jitter(size = 2, width = 0.15) +
    labs(y = "CV of Gage Height (ft)",
         x = "Season",
         caption = "Streamflow Variation by Watershed") +
    scale_color_manual(values = cal_palette("casj",
                                            n = 8,
                                            type = "continuous")) +
    facet_grid(site ~ .) +
    theme_bw())

# Appears flow is consistent across years in Winter/Spring, but
# more variable in Summer/Fall across years.

# Export figure.
# ggsave(plot = fig3,
#        filename = "figures/gageht_cv_032124.jpg",
#        width = 15,
#        height = 30,
#        units = "cm")

# And out of curiosity, let's zoom in on Watershed 6 once more.
(fig4 <- ggplot(dat_8 %>% 
                  filter(site == "W6") %>%
                  mutate(Month = factor(month)),
                aes(x = Month, y = temp, fill = Month)) +
    geom_boxplot() +
    labs(y = "Temperature (C)",
         x = "Month",
         caption = "Streamwater Temperature by Month in W6 (2017-2024)") +
    scale_fill_manual(values = cal_palette("lake",
                                            n = 12,
                                            type = "continuous")) +
    theme_bw() +
    theme(legend.position = "none"))

(fig5 <- ggplot(dat_8 %>% 
                  filter(site == "W6") %>%
                  mutate(Month = factor(month)),
                aes(x = Month, y = gageHt, fill = Month)) +
    geom_boxplot() +
    labs(y = "Gage Height (ft)",
         x = "Month",
         caption = "Streamflow by Month in W6 (2017-2024)") +
    scale_fill_manual(values = cal_palette("casj",
                                           n = 12,
                                           type = "continuous")) +
    theme_bw() +
    theme(legend.position = "none"))

fig45 <- fig4 / fig5

# Export figure.
# ggsave(plot = fig45,
#        filename = "figures/temp_gageht_W6_032124.jpg",
#        width = 15,
#        height = 15,
#        units = "cm")

# Emily asked for a different kind of figure, so let's make that here.
# Let's first create a separate dataset and append means.
dat_w6 <- dat_8 %>%
  filter(site == "W6") %>%
  group_by(month) %>%
  mutate(mean_monthly_temp = mean(temp, na.rm = TRUE),
         mean_monthly_gageHt = mean(gageHt, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Month = factor(month))

(fig6 <- ggplot(dat_w6, aes(x = month, y = temp, color = Month)) +
    geom_point(size = 3, shape = 1, fill = NA) +
    scale_color_manual(values = cal_palette("lake",
                                            n = 12,
                                            type = "continuous")) +
    geom_point(aes(y = mean_monthly_temp), size = 3, shape = 15, color = "black") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
    labs(y = "Temperature (C)",
         x = "Month",
         caption = "Streamwater Temperature by Month in W6 (2017-2024)") +
    theme_bw() +
    theme(legend.position = "none"))

# Export figure.
# ggsave(plot = fig6,
#        filename = "figures/temp_W6_pts_032724.jpg",
#        width = 15,
#        height = 8,
#        units = "cm")

# And let's do the same for the gage height data.
(fig7 <- ggplot(dat_w6, aes(x = month, y = gageHt, color = Month)) +
    geom_point(size = 3, shape = 1, fill = NA) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
    scale_color_manual(values = cal_palette("casj",
                                           n = 12,
                                           type = "continuous")) +
    geom_point(aes(y = mean_monthly_gageHt), size = 3, shape = 15, color = "black") +
    labs(y = "Gage Height (ft)",
         x = "Month",
         caption = "Streamflow by Month in W6 (2017-2024)") +
    theme_bw() +
    theme(legend.position = "none"))

# Export figure.
# ggsave(plot = fig7,
#        filename = "figures/gageht_W6_pts_032724.jpg",
#        width = 15,
#        height = 8,
#        units = "cm")

# Combine with invertebrate data to compare conditions through time.
# First, create a new dataset with which to delineate peaks.
dat_peak_inv <- dat_inv %>%
  group_by(watershed, year) %>%
  slice(which.max(total_count)) %>%
  ungroup()

(fig8a <- ggplot(dat_inv %>%
                   filter(watershed == 6), aes(x = Date,
                                               y = total_count)) +
    geom_point(color = "#6B6D9F", alpha = 0.8) +
    geom_point(data = dat_peak_inv %>% filter(watershed == 6),
               aes(x = Date, y = total_count),
               color = "#F28705", shape = 8) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Weekly Emergence") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(fig8b <- ggplot(dat_w6, aes(x = Date, y = temp)) +
    geom_line(color = "#3793EC") +
    # add lines delimiting dates of peak emergence
    geom_vline(xintercept = as.Date("2018-05-14"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2019-05-20"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2020-05-28"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2021-05-17"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2022-05-16"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2023-05-30"), 
               color = "#F28705", alpha = 0.5) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Temperature (C)") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(fig8c <- ggplot(dat_w6, aes(x = Date, y = gageHt)) +
    geom_line(color = "#8197A4") +
    # add lines delimiting dates of peak emergence
    geom_vline(xintercept = as.Date("2018-05-14"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2019-05-20"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2020-05-28"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2021-05-17"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2022-05-16"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2023-05-30"), 
               color = "#F28705", alpha = 0.5) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Gage Height (ft)",
         x = "Date") +
    theme_bw())

(fig8 <- fig8a / fig8b / fig8c)

fig8 + plot_annotation(caption = 'Aquatic insect emergence, streamwater temperature, and streamflow for Watershed 6. Peak emergence dates are denoted in orange.')

# Export figure.
# ggsave(plot = fig8,
#        filename = "figures/emerge_temp_gageht_W6_032824.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

# So, it seems while peak emergence dates remain relatively stable
# in W6, there may be an affect of floods on magnitude and temp.
# is almost always at 10 degree Celsius.

# Ok, let's also make this same figure for W5 (with which we can
# generate some hypotheses).
dat_w5 <- dat_8 %>%
  filter(site == "W5")

(fig9a <- ggplot(dat_inv %>%
                   filter(watershed == 5), aes(x = Date,
                                               y = total_count)) +
    geom_point(color = "#6B6D9F", alpha = 0.8) +
    geom_point(data = dat_peak_inv %>% 
                 filter(watershed == 5) %>%
                 # removing the last peak because this full year
                 # has not been counted yet
                 filter(Date < as.Date("2021-01-01")),
               aes(x = Date, y = total_count),
               color = "#F28705", shape = 8) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Weekly Emergence") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(fig9b <- ggplot(dat_w5, aes(x = Date, y = temp)) +
    geom_line(color = "#3793EC") +
    # add lines delimiting dates of peak emergence
    geom_vline(xintercept = as.Date("2018-06-04"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2019-05-28"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2020-05-26"), 
               color = "#F28705", alpha = 0.5) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Temperature (C)") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(fig9c <- ggplot(dat_w5, aes(x = Date, y = gageHt)) +
    geom_line(color = "#8197A4") +
    # add lines delimiting dates of peak emergence
    geom_vline(xintercept = as.Date("2018-06-04"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2019-05-28"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2020-05-26"), 
               color = "#F28705", alpha = 0.5) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Gage Height (ft)",
         x = "Date") +
    theme_bw())

(fig9 <- fig9a / fig9b / fig9c)

fig9 + plot_annotation(caption = 'Aquatic insect emergence, streamwater temperature, and streamflow for Watershed 5. Peak emergence dates are denoted in orange. Note, only 2018-2020 have been counted in full.')

# Export figure.
# ggsave(plot = fig9,
#        filename = "figures/emerge_temp_gageht_W5_032824.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

# And the same figure for W1 (since peak emergence was so late here).
dat_w1 <- dat_8 %>%
  filter(site == "W1")

(fig10a <- ggplot(dat_inv %>%
                   filter(watershed == 1), aes(x = Date,
                                               y = total_count)) +
    geom_point(color = "#6B6D9F", alpha = 0.8) +
    geom_point(data = dat_peak_inv %>% 
                 filter(watershed == 1),
               aes(x = Date, y = total_count),
               color = "#F28705", shape = 8) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Weekly Emergence") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(fig10b <- ggplot(dat_w1, aes(x = Date, y = temp)) +
    geom_line(color = "#3793EC") +
    # add lines delimiting dates of peak emergence
    geom_vline(xintercept = as.Date("2018-06-11"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2019-05-28"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2020-05-26"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2021-11-01"), 
               color = "#F28705", alpha = 0.5) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Temperature (C)") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(fig10c <- ggplot(dat_w1, aes(x = Date, y = gageHt)) +
    geom_line(color = "#8197A4") +
    # add lines delimiting dates of peak emergence
    geom_vline(xintercept = as.Date("2018-06-11"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2019-05-28"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2020-05-26"), 
               color = "#F28705", alpha = 0.5) +
    geom_vline(xintercept = as.Date("2021-11-01"), 
               color = "#F28705", alpha = 0.5) +
    xlim(as.Date("2017-06-01"), 
         as.Date("2023-12-31")) +
    labs(y = "Gage Height (ft)",
         x = "Date") +
    theme_bw())

(fig10 <- fig10a / fig10b / fig10c)

fig10 + plot_annotation(caption = 'Aquatic insect emergence, streamwater temperature, and streamflow for Watershed 1. Peak emergence dates are denoted in orange.')

# Export figure.
# ggsave(plot = fig10,
#        filename = "figures/emerge_temp_gageht_W1_040224.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

# End of script.
