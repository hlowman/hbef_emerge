### Discharge Analysis (revised)
### July 1, 2025
### Heili Lowman

#### README ####

# The following script will estimate discharge metrics
# leading up to peak emergence each year.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load discharge data.
q_dat <- read_csv("data_raw/HBEF_DailyStreamflow_1956-2023.csv")
# Streamflow is in mm/day.

# Load emergence indices.
emerge_indices <- readRDS("data_working/emerge_indices_all_070125.rds")
emerge_cf <- readRDS("data_working/emerge_indices_cf_070125.rds")
emerge_sf <- readRDS("data_working/emerge_indices_sf_070125.rds")

#### Q metrics ####

# Rather than over-engineering the seasons, I am (for now) simply
# putting a date threshold on things.

# Calculate mean Q for each year from March 1 to May 31.
q_means <- q_dat %>%
  mutate(Year = year(DATE)) %>%
  mutate(Month = month(DATE)) %>%
  filter(Year > 2017 & Month > 2 & Month < 5) %>%
  group_by(WS, Year) %>%
  summarize(meanQ_mmd = mean(Streamflow)) %>%
  ungroup() %>%
  mutate(WS = as.character(WS))

# And join with emergence datasets.
emergence_q <- left_join(emerge_indices, q_means,
                         by = c("watershed" = "WS", "Year"))
emergence_cf_q <- left_join(emerge_cf, q_means,
                            by = c("watershed" = "WS", "Year"))
emergence_sf_q <- left_join(emerge_sf, q_means,
                            by = c("watershed" = "WS", "Year"))

#### Plot ####

(fig1_discharge <- ggplot(emergence_q,
                           aes(x = meanQ_mmd,
                               y = annual_count,
                               fill = factor(Year))) +
   geom_point(size = 6, shape = 21, alpha = 0.75) +
   scale_fill_manual(values = c("white", "#C6B6E9",
                                "#AA91DE","#8D6DD3", "#7148C8",
                                "#5524BD","#3900B3")) +
   labs(x = "Mean Discharge March 1 - April 30",
        y = "Annual Total Emergence",
        fill = "Year") +
   theme_bw() +
   theme(text = element_text(size = 14),
         legend.position = "none"))

(fig2_discharge <- ggplot(emergence_cf_q,
                          aes(x = meanQ_mmd,
                              y = annual_count,
                              fill = factor(Year))) +
    geom_point(size = 6, shape = 21, alpha = 0.75) +
    scale_fill_manual(values = c("white", "#C6B6E9",
                                 "#AA91DE","#8D6DD3", "#7148C8",
                                 "#5524BD","#3900B3")) +
    labs(x = "Mean Discharge March 1 - April 30",
         y = "Annual Caddisfly Emergence",
         fill = "Year") +
    theme_bw() +
    theme(text = element_text(size = 14),
          legend.position = "none"))

(fig3_discharge <- ggplot(emergence_sf_q,
                          aes(x = meanQ_mmd,
                              y = annual_count,
                              fill = factor(Year))) +
    geom_point(size = 6, shape = 21, alpha = 0.75) +
    scale_fill_manual(values = c("white", "#C6B6E9",
                                 "#AA91DE","#8D6DD3", "#7148C8",
                                 "#5524BD","#3900B3")) +
    labs(x = "Mean Discharge March 1 - April 30",
         y = "Annual Stonefly Emergence",
         fill = "Year") +
    theme_bw() +
    theme(text = element_text(size = 14)))

(fig_discharge <- fig1_discharge + fig2_discharge + fig3_discharge +
    plot_annotation(tag_levels = "a"))

# ggsave(plot = fig_discharge,
#        filename = "figures/annual_emerge_discharge_070125.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

# End of script.


# End of script.