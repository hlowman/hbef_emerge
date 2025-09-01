### Covariate Figures of Current/Past Emergence
### August 7, 2025
### Heili Lowman

#### README ####

# The following script will create figures to showcase the
# current and previous years' total emergence, with 
# coloration to explore relationships with flow conditions.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)

# Load data.
dat <- readRDS("data_working/sum_annual_dipt_emerge_081425.rds")
flow_dat <- readRDS("data_working/low_high_flow_days_cvQ.rds")
april_flow_dat <- readRDS("data_working/april_flows.rds")

#### Tidy ####

# Calculate the previous year's emergence values.
dat_wide <- dat %>%
  pivot_wider(names_from = period, values_from = sum_total_count) %>%
  group_by(watershed) %>%
  mutate(prev_early = lag(early),
         prev_late = lag(late)) %>%
  ungroup()

# And remove HBK because it does not have the flow records necessary.
dat_wide_ed <- dat_wide %>%
  filter(!watershed == "HBK")

# Need to make watershed numerical.
dat_wide_ed$watershed <- as.numeric(dat_wide_ed$watershed)

# Join with discharge percentile data.
all_dat <- left_join(dat_wide_ed, flow_dat,
                        by = c("watershed" = "WS", "year" = "water_year")) 

all_dat <- left_join(all_dat, april_flow_dat,
                     by = c("watershed" = "WS", "year" = "water_year")) %>%
  group_by(watershed) %>%
  mutate(prev_low_flow_perc = lag(low_flow_perc),
         prev_high_flow_perc = lag(high_flow_perc),
         prev_cv_q_perc = lag(cv_q_perc),
         prev_sum_apr_q_perc = lag(sum_apr_q_perc),
         prev_max_apr_q_perc = lag(max_apr_q_perc)) %>%
  mutate(low_color = case_when(prev_low_flow_perc >= 0.9 ~ "above90perc",
                               TRUE ~ "below90perc"),
         high_color = case_when(prev_high_flow_perc >= 0.9 ~ "above90perc",
                                TRUE ~ "below90perc"),
         var_color = case_when(prev_cv_q_perc >= 0.9 ~ "above90perc",
                               TRUE ~ "below90perc"),
         aprsum_color = case_when(prev_sum_apr_q_perc >= 0.9 ~ "above90perc",
                               TRUE ~ "below90perc"),
         aprmax_color = case_when(prev_max_apr_q_perc >= 0.9 ~ "above90perc",
                                  TRUE ~ "below90perc")) %>%
  ungroup()

# Trim down only to sites with sufficient data.
all_dat_trim <- all_dat %>%
  mutate(keep = case_when(watershed %in% c(1,2,3,4,9) & year %in% c(2019, 2020) |
                            watershed %in% c(5,6) & year %in% c(2019, 2020, 
                                                                2021, 2022,
                                                                2023, 2024) ~ "Yes",
                          TRUE ~ "No")) %>%
  filter(keep == "Yes")

#### Plot ####

# Plot emergence relationship versus low flows
(fig_low <- ggplot(all_dat_trim,
                   aes(x = prev_early, 
                       y = early,
                       fill = prev_low_flow_perc)) +
   geom_point(size = 7, shape = 21) +
   labs(y = "Total Aq. Diptera Emergence",
        x = "Prior Year Total Aq. Diptera Emergence",
        shape = "Watershed",
        fill = "Prior Year Low Flow %tile") +
   scale_fill_gradient(low = "white", high = "#E7298A",
                       breaks = c(0.1, 0.5, 0.9)) +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "bottom"))

# Plot emergence relationship versus high flows
(fig_high <- ggplot(all_dat_trim,
                    aes(x = prev_early, 
                        y = early,
                        fill = prev_high_flow_perc)) +
    geom_point(size = 7, shape = 21) +
    labs(y = "Total Aq. Diptera Emergence",
         x = "Prior Year Total Aq. Diptera Emergence",
         shape = "Watershed",
         fill = "Prior Year High Flow %tile") +
    scale_fill_gradient(low = "white", high = "#E7298A",
                        breaks = c(0.1, 0.5, 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "bottom"))

# Plot emergence relationship versus flow variability
(fig_cv <- ggplot(all_dat_trim,
                  aes(x = prev_early, 
                      y = early,
                      fill = prev_cv_q_perc)) +
    geom_point(size = 7, shape = 21) +
    scale_shape_manual(values = c(21, 22)) +
    labs(y = "Total Aq. Diptera Emergence",
         x = "Prior Year Total Aq. Diptera Emergence",
         shape = "Watershed",
         fill = "Prior Year Flow Variability %tile") +
    scale_fill_gradient(low = "white", high = "#E7298A",
                        breaks = c(0.1, 0.5, 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "bottom"))

# Plot emergence relationship versus snowmelt flows
(fig_apr <- ggplot(all_dat_trim,
                  aes(x = prev_early, 
                      y = early,
                      fill = prev_max_apr_q_perc)) +
    geom_point(size = 7, shape = 21) +
    scale_shape_manual(values = c(21, 22)) +
    labs(y = "Total Aq. Diptera Emergence",
         x = "Prior Year Total Aq. Diptera Emergence",
         shape = "Watershed",
         fill = "Prior Year Max. April Flow %tile") +
    scale_fill_gradient(low = "white", high = "#E7298A",
                        breaks = c(0.1, 0.5, 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "bottom"))

(fig_flows <- fig_low + fig_high + fig_cv + fig_apr +
    plot_annotation(tag_levels = "A"))

# Export figure.
ggsave(plot = fig_flows,
       filename = "figures/sum_emerge_flows_090125.jpg",
       width = 40,
       height = 35,
       units = "cm")

# End of script.
