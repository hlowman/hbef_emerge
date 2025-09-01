### Manuscript Figures
### July 8, 2025
### Heili Lowman

#### README ####

# The following script will create additional figures drafted for the
# black fly emergence manuscript.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(rcartocolor)

# Load necessary data.
dipt_dat <- readRDS("data_working/sum_emerge_dipt_070825.csv")
q_dat <- readRDS("data_working/low_high_flow_days_cvQ.rds")

#### Tidy ####

# Calculate the previous year's emergence values.
dipt_dat_wide <- dipt_dat %>%
  pivot_wider(names_from = period, values_from = sum_total_count) %>%
  group_by(watershed) %>%
  mutate(prev_early = lag(early),
         prev_late = lag(late)) %>%
  ungroup()

# Need to make watershed numerical.
dipt_dat_wide$watershed <- as.numeric(dipt_dat_wide$watershed)

# Join with discharge percentile data.
dipt_q_dat <- left_join(dipt_dat_wide, q_dat,
                        by = c("watershed" = "WS", "year" = "water_year")) %>%
  group_by(watershed) %>%
  mutate(prev_low_flow_perc = lag(low_flow_perc),
         prev_high_flow_perc = lag(high_flow_perc)) %>%
  mutate(low_color = case_when(prev_low_flow_perc >= 0.9 ~ "above90perc",
                            TRUE ~ "below90perc"),
         high_color = case_when(prev_high_flow_perc >= 0.9 ~ "above90perc",
                            TRUE ~ "below90perc"),
         var_color = case_when(cv_q_perc >= 0.9 ~ "above90perc",
                                TRUE ~ "below90perc"))

#### Plot ####

# Plot emergence relationship versus low flows
(fig_low <- ggplot(dipt_q_dat,
                   aes(x = prev_early, 
                       y = early,
                       fill = prev_low_flow_perc)) +
   geom_point(size = 7, 
              aes(shape = factor(watershed))) +
   scale_shape_manual(values = c(21, 22),
                      guide = "none") +
   labs(y = "Total Simuliidae Emergence",
        x = "Previous Year Total Simuliidae Emergence",
        shape = "Watershed",
        fill = "Previous Year\nLow Flow %tile") +
   scale_fill_gradient(low = "white", high = "firebrick3") +
   theme_bw() +
   theme(text = element_text(size = 20)))

# Plot emergence relationship versus high flows
(fig_high <- ggplot(dipt_q_dat,
                   aes(x = prev_early, 
                       y = early,
                       fill = prev_high_flow_perc)) +
    geom_point(size = 7, 
               aes(shape = factor(watershed))) +
    scale_shape_manual(values = c(21, 22),
                       guide = "none") +
    labs(y = "Total Simuliidae Emergence",
         x = "Previous Year Total Simuliidae Emergence",
         shape = "Watershed",
         fill = "Previous Year\nHigh Flow %tile") +
    scale_fill_gradient(low = "white", high = "firebrick3") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Plot emergence relationship versus flow variability
(fig_cv <- ggplot(dipt_q_dat,
                    aes(x = prev_early, 
                        y = early,
                        fill = cv_q_perc)) +
    geom_point(size = 7, 
               aes(shape = factor(watershed))) +
    scale_shape_manual(values = c(21, 22)) +
    labs(y = "Total Simuliidae Emergence",
         x = "Previous Year Total Simuliidae Emergence",
         shape = "Watershed",
         fill = "Previous Year\nFlow Variability %tile") +
    scale_fill_gradient(low = "white", high = "firebrick3") +
    theme_bw() +
    theme(text = element_text(size = 20)))

(fig_flows <- fig_low + fig_high +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_flows,
#        filename = "figures/sum_emerge_flows_070825.jpg",
#        width = 45,
#        height = 15,
#        units = "cm")

# End of script.
