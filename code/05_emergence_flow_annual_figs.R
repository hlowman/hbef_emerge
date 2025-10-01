### Covariate Figures of Current/Past Emergence
### August 7, 2025
### Heili Lowman

#### README ####

# The following script will create figures to showcase the
# current and previous years' total emergence as well as 
# relationships with flow conditions.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(patchwork)
library(RColorBrewer)
library(nlme)

# Load data.
dat <- readRDS("data_working/sum_annual_dipt_emerge_091225.rds") # from 01 script
flow_dat <- readRDS("data_working/low_high_flow_days_cvQ.rds")
april_flow_dat <- readRDS("data_working/april_flows.rds")

#### Tidy ####

# Calculate the previous year's emergence values.
dat_wide <- dat %>%
  group_by(watershed) %>%
  mutate(prev_emerge = lag(sum_total_count)) %>%
  ungroup()

# And remove HBK because it does not have the flow records necessary.
dat_wide_ed <- dat_wide %>%
  filter(!watershed == "HBK")

# Need to make watershed numerical.
dat_wide_ed$watershed <- as.numeric(dat_wide_ed$watershed)

# Trim flow data to years of interest.
flow_dat_trim <- flow_dat %>%
  filter(water_year > 2016)

# Join with discharge percentile data.
all_dat <- full_join(dat_wide_ed, flow_dat_trim,
                        by = c("watershed" = "WS", "year" = "water_year")) 

all_dat <- left_join(all_dat, april_flow_dat,
                     by = c("watershed" = "WS", "year" = "water_year")) %>%
  arrange(watershed, year) %>% # make sure years are in order
  group_by(watershed) %>%
  mutate(prev_low_flow_days = lag(low_flow_days),
         prev_low_flow_perc = lag(low_flow_perc),
         prev_high_flow_days = lag(high_flow_days),
         prev_high_flow_perc = lag(high_flow_perc),
         prev_cv_q = lag(cv_q),
         prev_cv_q_perc = lag(cv_q_perc),
         prev_sum_apr_q_perc = lag(sum_apr_q_perc),
         prev_max_apr_q = lag(max_apr_q),
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

# Trim down only to W5 & W6 sites.
all_dat_trim <- all_dat %>%
  mutate(keep = case_when(watershed %in% c(5,6) & year %in% c(2018, 2019, 2020, 
                                                              2021, 2022, 2023, 
                                                              2024) ~ "Yes",
                          TRUE ~ "No")) %>%
  filter(keep == "Yes")

#### Plot ####

##### Emergence AR(1) #####

(fig_emerge <- ggplot(all_dat_trim,
                       aes(x = prev_emerge, 
                           y = sum_total_count,
                           color = factor(year),
                           shape = factor(watershed))) +
   geom_point(size = 7) +
   labs(y = "Total Aq. Diptera Emergence",
        x = "Prior Year Aq. Diptera Emergence",
        shape = "Watershed",
        color = "Year") +
   scale_color_brewer(palette = "Dark2") +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "right"))

# ggsave(plot = fig_emerge,
#        filename = "figures/sum_emerge_100125.jpg",
#        width = 20,
#        height = 17,
#        units = "cm")

# And linear regression to examine effect of prior yr's emergence.

# Simple linear regression
emerge.lm1 <- lm(sum_total_count ~ prev_emerge, data = all_dat_trim)
summary(emerge.lm1)
plot(emerge.lm1)
# Re-fit with gls to compare better with lmem() function
emerge.lm2 <- gls(sum_total_count ~ prev_emerge, data = all_dat_trim %>%
                    drop_na(prev_emerge))
# Fit multi-level model to account for site-level variation
emerge.lm3 <- lme(sum_total_count ~ prev_emerge, 
                  random = ~1|watershed,
                  data = all_dat_trim %>%
                    drop_na(prev_emerge) %>%
                    mutate(watershed = factor(watershed)))
# Compare the two model structures
AIC(emerge.lm2, emerge.lm3) # essentially identical
# Examine residuals
plot(emerge.lm3)
qqnorm(emerge.lm3)
# Summary of multi-level model 
summary(emerge.lm3)

##### Low flows #####

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

(fig_low_alt <- ggplot(all_dat_trim,
                   aes(x = prev_low_flow_days, 
                       y = sum_total_count,
                       color = factor(year),
                       shape = factor(watershed))) +
    geom_point(size = 7) +
    labs(y = "Total Aq. Diptera Emergence",
         x = "Prior Year Low Flow Days",
         shape = "Watershed",
         fill = "Year") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

##### High flows #####

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

(fig_high_alt <- ggplot(all_dat_trim,
                       aes(x = prev_high_flow_days, 
                           y = sum_total_count,
                           color = factor(year),
                           shape = factor(watershed))) +
    geom_point(size = 7) +
    labs(y = "Total Aq. Diptera Emergence",
         x = "Prior Year High Flow Days",
         shape = "Watershed",
         fill = "Year") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

##### Variability #####

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

(fig_cv_alt <- ggplot(all_dat_trim,
                        aes(x = prev_cv_q, 
                            y = sum_total_count,
                            color = factor(year),
                            shape = factor(watershed))) +
    geom_point(size = 7) +
    labs(y = "Total Aq. Diptera Emergence",
         x = "Prior Year Coeff. of Variation in Discharge",
         shape = "Watershed",
         fill = "Year") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none"))

##### Snowmelt #####

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

(fig_apr_alt <- ggplot(all_dat_trim,
                      aes(x = prev_max_apr_q, 
                          y = sum_total_count,
                          color = factor(year),
                          shape = factor(watershed))) +
    geom_point(size = 7) +
    labs(y = "Total Aq. Diptera Emergence",
         x = "Prior Year Max. April Discharge",
         shape = "Watershed",
         color = "Year") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "right"))

(fig_flows <- fig_low + fig_high + fig_cv + fig_apr +
    plot_annotation(tag_levels = "A"))

(fig_flows_alt <- fig_low_alt + fig_high_alt + fig_cv_alt + fig_apr_alt +
    plot_annotation(tag_levels = "A"))

# Export figures.
# ggsave(plot = fig_flows,
#        filename = "figures/sum_emerge_flows_090125.jpg",
#        width = 40,
#        height = 35,
#        units = "cm")

ggsave(plot = fig_flows_alt,
       filename = "figures/sum_emerge_flows_100125.jpg",
       width = 40,
       height = 35,
       units = "cm")

# End of script.
