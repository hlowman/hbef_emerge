### SI Figures
### September 16, 2025
### Heili Lowman

#### README ####

# The following script will create supplementary figures
# as part of the insect emergence manuscript.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)

# Load datasets.
q_dat <- read_csv("data_raw/HBEF_DailyStreamflow_1956-2024.csv") # Streamflow is in mm/day.
q_stats <- readRDS("data_working/low_high_flow_days_cvQ.rds") # With low and high flow counts populated.
ppt_dat <- read_csv("data_raw/dailyWatershedPrecip1956-2025.csv") # Precipitation is in mm.
temp_dat <- read_csv("data_raw/HBEF_air_temp_daily.csv") # Daily air temperature in Celsius.
chem_dat <- read_csv("data_raw/HubbardBrook_weekly_stream_chemistry_1963-2024.csv") # Weekly chem data.
insect_dat <- readRDS("data_working/aquatic_counts_complete_yrs_121925.rds") # Insect count data.
stream_dat <- readRDS("data_working/stream_climate_qchem.rds") # Summarized chemistry data.

weir1_dat <- read_csv("data_raw/sediment_weir_1.csv")
weir5_dat <- read_csv("data_raw/sediment_weir_5.csv")
weir6_dat <- read_csv("data_raw/sediment_weir_6.csv")

#### Tidy ####

# Edits to flow and chemistry data for context plot below.
# Will only present data at one site so as not to bias outputs.
q_dat <- q_stats %>%
  filter(WS == 6)

q_dat_present <- q_dat %>%
  mutate(keep = case_when(WS == 6 & water_year %in% c(2017, 2018, 2019, 2020,
                                                2021, 2022, 2023, 2024) ~ "Yes",
                          TRUE ~ NA)) %>%
  filter(keep == "Yes")

temp_trim <- temp_dat %>%
  filter(STA == "STA1") %>%
  mutate(month = month(date),
         year = year(date),
         water_year = case_when(month %in% c(1,2,3,4,5) ~ year-1,
                                month %in% c(6,7,8,9,10,11,12) ~ year)) %>%
  group_by(water_year) %>%
  summarize(mean_air_temp = mean(AVE, na.rm = TRUE)) %>%
  ungroup()

temp_dat_present <- temp_trim %>%
  mutate(keep = case_when(water_year %in% c(2017, 2018, 2019, 2020,
                                            2021, 2022, 2023, 2024) ~ "Yes",
                          TRUE ~ NA)) %>%
  filter(keep == "Yes")

chem_dat <- chem_dat %>%
  filter(site %in% c("W1", "W2", "W3",
                     "W4", "W5", "W6",
                     "W9"))

chem_dat_present <- chem_dat %>% 
  mutate(year = year(date)) %>%
  mutate(keep = case_when(site == "W1" & year %in% c(2018,2019,2020) |
                          site == "W2" & year %in% c(2018,2019,2020) |
                          site == "W3" & year %in% c(2018,2019,2020) |
                          site == "W4" & year %in% c(2018,2019,2020) |
                          site == "W5" & year %in% c(2018,2019, 2020,
                                                  2021, 2022, 2023, 2024) |
                          site == "W6" & year %in% c(2018,2019, 2020,
                                                  2021, 2022, 2023, 2024) |
                          site == "W9" & year %in% c(2018,2019,2020) ~ "Yes",
                          TRUE ~ NA)) %>%
  filter(keep == "Yes")

# Filtering/joining insect and temperature data.
# Trim only to black flies and summarize by week.
# Summarize by order.
dat_dipt <- insect_dat %>%
  filter(Order == "dipteran") %>%
  group_by(watershed, year, Date) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = month(Date))

# The stream data doesn't always perfectly align with the
# insect collections, so using the previous measurement of
# temperature here.
stream_dat_filled <- stream_dat %>%
  fill(temp)

# Join with stream data.
dat_all <- full_join(dat_dipt, stream_dat_filled,
                     by = join_by("watershed" == "WS",
                                  "Date" == "DATE"))

# Also, need to calculate flow duration curves by year.
flow_dat <- stream_dat %>%
  select(WS, DATE, Streamflow) %>%
  mutate(Year = year(DATE))

curves5 <- data.frame()

for(i in c(2017:2024)){
  yearly_curve <- flow_dat %>%
    filter(WS == 5) %>%
    filter(Year == i) %>%
    arrange(desc(Streamflow)) %>%
    mutate(rank = row_number(),
           exceedance_perc = (rank / (n() + 1)) * 100)
  
  curves5 <- rbind(curves5, yearly_curve)
}

# Trimming weir data to the dates of interest.
weir1_dat_recent <- weir1_dat %>%
  mutate(year = year(mdy(`end date`))) %>%
  filter(year > 2017 & year < 2023) %>%
  mutate(watershed = "W1")

# Replace text in organic column with NA.
weir1_dat_recent[3,3] <- NA

weir5_dat_recent <- weir5_dat %>%
  mutate(year = year(mdy(`end date`))) %>%
  filter(year > 2017 & year < 2023) %>%
  mutate(watershed = "W5")

# Replace text in organic column with NA.
weir5_dat_recent[3,3] <- NA

weir6_dat_recent <- weir6_dat %>%
  mutate(year = year(mdy(`end date`))) %>%
  filter(year > 2017 & year < 2023) %>%
  mutate(watershed = "W6")

# Replace text in organic column with NA.
weir6_dat_recent[3,3] <- NA

# And join the above together.
dat_weirs <- full_join(weir1_dat_recent, weir5_dat_recent)
dat_weirs <- full_join(dat_weirs, weir6_dat_recent)

# And make in long format for easier stacked bar plotting.
dat_weirs_long <- dat_weirs %>%
  mutate(organic = as.numeric(organic)) %>%
  select(`start date`:total, year, watershed) %>%
  pivot_longer(cols = organic:mineral,
               names_to = "Fraction")

#### Figures ####

##### Historical #####

# Historical context figures for temperature, flow, and nutrients.
# And filtering to overlap a similar historical period (>1964) as
# some of the other analyses.

# Discharge panels
(fig_q_low <- ggplot(q_dat, 
                     aes(x = low_flow_days)) +
   geom_histogram(color = "black", fill = "white",
                  bins = 20) +
   geom_histogram(data = q_dat_present,
                  color = "black", fill = "gray40",
                  bins = 20) +
   scale_x_log10() +
   labs(x = "Low Flow Days",
        y = "Water Years") +
   theme_bw())

(fig_q_high <- ggplot(q_dat, 
                      aes(x = high_flow_days)) +
    geom_histogram(color = "black", fill = "white",
                   bins = 10) +
    geom_histogram(data = q_dat_present,
                   color = "black", fill = "gray40",
                   bins = 10) +
    scale_x_continuous(breaks = c(0,2,4,6,8)) +
    labs(x = "High Flow Days",
         y = "Water Years") +
    theme_bw())

(fig_cvq <- ggplot(q_dat, 
                   aes(x = cv_q)) +
    geom_histogram(color = "black", fill = "white",
                   bins = 20) +
    geom_histogram(data = q_dat_present,
                   color = "black", fill = "gray40",
                   bins = 20) +
    labs(x = expression(CV[Q]),
         y = "Water Years") +
    theme_bw())

# Air temperature panel
(fig_air_temp <- ggplot(temp_trim %>%
                          filter(water_year > 1955), 
                        aes(x = mean_air_temp)) +
    geom_histogram(color = "black", fill = "white",
                   bins = 20) +
    geom_histogram(data = temp_dat_present,
                   color = "black", fill = "gray40",
                   bins = 20) +
    #scale_x_log10() +
    labs(x = "Mean Annual Air Temperature (°C)",
         y = "Water Years") +
    theme_bw())

# Combine into a single figure.
(fig_historic <- (fig_q_low + fig_q_high) / (fig_cvq + fig_air_temp) +
    plot_annotation(tag_levels = "A"))

# And export.
# ggsave(plot = fig_historic,
#        filename = "figures/conditions_historic_v_present_111825.jpg",
#        width = 20,
#        height = 18,
#        units = "cm")

##### TS W5 vs. W6 #####

# First figure will compare W5 & W6 in 2019
# to demonstrate variation among watersheds in a given year

# I will initially make a revised dataset that I can use
# to plot the previous year and the current year's data
# simultaneously. - NB: Chose not to go this route because
# the figure gets way too messy.

(fig_compare1a <- ggplot(dat_all %>%
                           filter(watershed == "5") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(total_count), 
                         aes(x = Date, y = total_count)) +
   geom_line(linewidth = 1) +
   scale_x_date(date_breaks = "3 month", 
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
                           filter(watershed == "6") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(total_count), 
                         aes(x = Date, y = total_count)) +
    geom_line(linewidth = 1) +
    scale_x_date(date_breaks = "3 month",
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
                           filter(watershed == "5") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31"),
                         aes(x = Date, y = Streamflow)) +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 80) +
    labs(y = "Discharge (mm/day)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1d <- ggplot(dat_all %>%
                           filter(watershed == "6") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31"), 
                         aes(x = Date, y = Streamflow)) +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
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
                           filter(watershed == "5") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(temp), 
                         aes(x = Date, y = temp)) +
    geom_line(color = "#D95F02", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 20) +
    labs(y = "Temperature (°C)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1f <- ggplot(dat_all %>%
                           filter(watershed == "6") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(temp), 
                         aes(x = Date, y = temp)) +
    geom_line(color = "#D95F02", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
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
                           filter(watershed == "5") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(chla_T), 
                         aes(x = Date, y = chla_T)) +
    geom_line(color = "#66A61E", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 #labels = date_format("%b"),
                 limits = as.Date(c('2019-01-01','2020-01-01'))) +
    ylim(0, 10) +
    labs(x = "Date", y = "Chlorophyll a (mg/L)") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

(fig_compare1h <- ggplot(dat_all %>%
                           filter(watershed == "6") %>%
                           filter(Date < "2020-01-01" &
                                    Date > "2018-12-31") %>%
                           drop_na(chla_T), 
                         aes(x = Date, y = chla_T)) +
    geom_line(color = "#66A61E", linewidth = 1) +
    scale_x_date(date_breaks = "3 month", 
                 #labels = date_format("%b"),
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
#        filename = "figures/w5_w6_2019_121925.jpg",
#        width = 30,
#        height = 30,
#        units = "cm")

##### TS Flow Duration #####

# Second figure will demonstrate variation within a watershed across years.
# This will instead present 2020-2022 for Watershed 5 alone.

# In order to have the lines plot appropriately, I will add a column to
# group by year.
dat_all <- dat_all %>%
  mutate(Year = year(Date)) %>%
  mutate(month = month(Date))

(fig_compare2a <- ggplot(dat_all %>%
                           filter(watershed == "5") %>%
                           filter(Date < "2023-01-01" &
                                    Date > "2019-12-31") %>%
                           drop_na(total_count), 
                         aes(x = Date, y = total_count,
                             group = Year)) +
    geom_line(linewidth = 1) +
    scale_x_date(date_breaks = "6 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Weekly Aq. Diptera\nEmergence") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          text = element_text(size = 20),
          legend.position = "none"))

(fig_compare2b <- ggplot(dat_all %>%
                           filter(watershed == "5") %>%
                           filter(Date < "2023-01-01" &
                                    Date > "2019-12-31"), 
                         aes(x = Date, y = Streamflow)) +
    geom_hline(aes(yintercept = 0.0440),
               color = "#7570B3", linewidth = 0.75) + # 10%tile historical flow
    # geom_hline(aes(yintercept = 1.128),
    #            color = "#375377", linewidth = 0.75) + # median historical flow
    geom_hline(aes(yintercept = 22.82592),
               color = "#8DA0CB", linewidth = 0.75) + # 99%tile historical flow
    annotate("text", x = ymd("2022-09-01"), y = 10, size = 8,
             label = "10th", color = "#7570B3") +
    # annotate("text", x = ymd("2022-09-01"), y = 4, 
    #          label = "50th", color = "#375377") +
    annotate("text", x = ymd("2022-09-01"), y = 32, size = 8,
             label = "99th", color = "#8DA0CB") +
    geom_line(color = "#3793EC", linewidth = 1) +
    scale_x_date(date_breaks = "6 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Discharge\n(mm/day)") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Quickly make a separate dataset with which to calculate cumulative low flow days.
dat_w5 <- dat_all %>%
  filter(watershed == "5") %>%
  mutate(year = year(Date),
         low_flow_days = ifelse(Streamflow <= 0.0440, 1, 0)) %>%
  arrange(Date) %>%
  group_by(year) %>%
  mutate(sum_low_flow_days = cumsum(low_flow_days)) %>%
  ungroup()

# Figure for cumulative plots of low flow days.
(fig_compare2c <- ggplot(dat_w5 %>%
                           filter(Date < "2023-01-01" &
                                    Date > "2019-12-31"), 
                         aes(x = Date, y = sum_low_flow_days,
                             group = year)) +
    geom_line(color = "#7570B3", linewidth = 1) +
    scale_x_date(date_breaks = "6 month", 
                 labels = date_format("%b-%Y"),
                 limits = as.Date(c('2020-01-01','2023-01-01'))) +
    labs(y = "Cumulative\nLow Flow Days",
         x = "Date") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# Figure with flow duration curves by year.
(fig_compare_fd <- ggplot(curves5 %>%
                            filter(DATE < "2023-01-01" &
                                     DATE > "2019-12-31"), 
                          aes(x = exceedance_perc, 
                              y = Streamflow)) +
    geom_line(color = "#7570B3", linewidth = 1) +
    #scale_y_log10(breaks=c(.01,.1,1,10,100),labels=c(.01,.1,1,10,100)) +
    labs(x = "Percent Time Flow Exceeded",
         y = "Discharge\n(mm/day)") +
    facet_grid(.~Year) +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Pulling the first figure together.
(fig_compare2 <- (fig_compare2a) /
    (fig_compare2b) /
    (fig_compare_fd) ) +
  plot_annotation(tag_levels = "A")

# Export figure.
# ggsave(plot = fig_compare2,
#        filename = "figures/w5_20_21_22_121925.jpg",
#        width = 35,
#        height = 25,
#        units = "cm")

##### Temp vs. Emergence #####

# Including only W5 & W6.
(fig_about_peak <- ggplot(dat_all %>% 
                            mutate(keep = case_when(duplicate == "Dup" ~ "No",
                                                    watershed == 5 & year %in% 
                                                      c(2018,2019, 2020,
                                                        2021, 2022, 2023, 2024) |
                                                    watershed == 6 & year %in% 
                                                      c(2018,2019, 2020,
                                                        2021, 2022, 2023, 2024) ~ "Yes",
                                                    TRUE ~ NA)) %>%
                            filter(keep == "Yes"),
                          aes(x = temp, y = total_count,
                              shape = watershed)) +
   geom_point(size = 5, stroke = 2, alpha = 0.5) +
   scale_shape_manual(values = c(16, 21)) +
   labs(x = "Daytime Temperature (°C)",
        y = "Weekly Aquatic Diptera Emergence",
        shape = "Watershed") +
   theme_bw() +
   theme(text = element_text(size = 20)))

# Export figure.
# ggsave(plot = fig_about_peak,
#        filename = "figures/temp_about_peak_W5W6_121925.jpg",
#        width = 24,
#        height = 14,
#        units = "cm")

##### Weir Ponds #####

# Figure of the weir basin contents.
(weir_barplot <- ggplot(dat_weirs_long, 
                        aes(x = year, 
                            y = value,
                            fill = Fraction)) +
    geom_bar(position="stack", stat="identity") +
    labs(x = "Year", y = "Dried Material (kg/ha)") +
    theme_bw() +
    # theme(text = element_text(size = 20),
    #       legend.position = "top") +
    scale_fill_manual(labels = c("Mineral", "Mixed", "Organic"),
                      values = c("#DED4C8", "#AD6F4F", "#AEC96F")) +
    facet_grid(watershed~.))

# Export figure.
# ggsave(plot = weir_barplot,
#        filename = "figures/weir_recent_091625.jpg",
#        width = 20,
#        height = 15,
#        units = "cm")

# End of script.
