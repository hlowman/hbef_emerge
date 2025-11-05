### Covariate Figures of Current/Past Emergence
### August 7, 2025
### Heili Lowman

#### README ####

# The following script will create figures to showcase the
# current and previous years' total emergence as well as 
# relationships with flow conditions and chemistry.

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
chem_dat <- readRDS("data_working/nuts_chla_pH.rds")

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

# And make corresponding site column for chem dataset.
chem_dat <- chem_dat %>%
  mutate(watershed = case_when(site == "W1" ~ 1,
                               site == "W2" ~ 2,
                               site == "W3" ~ 3,
                               site == "W4" ~ 4,
                               site == "W5" ~ 5,
                               site == "W6" ~ 6,
                               site == "W9" ~ 7,
                               site == "HBK" ~ 0)) %>%
  # and removing HB to match dataset above
  filter(watershed > 0)

# Join with discharge data.
all_dat <- full_join(dat_wide_ed, flow_dat_trim,
                        by = c("watershed" = "WS", "year" = "water_year")) 

all_dat <- left_join(all_dat, april_flow_dat,
                     by = c("watershed" = "WS", "year" = "water_year")) 

# Join with chemistry data.
all_dat <- full_join(all_dat, chem_dat,
                     by = c("watershed", "year" = "water_year")) 

# And format for lagged columns.
all_dat <- all_dat %>%
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
         prev_max_apr_q_perc = lag(max_apr_q_perc),
         prev_low_pH_days = lag(low_pH_days),
         prev_no3 = lag(mean_NO3),
         prev_po4 = lag(mean_PO4),
         prev_chla = lag(mean_chla)) %>%
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
                   aes(x = prev_emerge, 
                       y = sum_total_count,
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
    geom_point(size = 10, alpha = 0.75) +
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

##### pH #####

# Plot emergence relationship versus pH deviations
(fig_pH <- ggplot(all_dat_trim,
                       aes(x = prev_low_pH_days, 
                           y = sum_total_count,
                           color = factor(year),
                           shape = factor(watershed))) +
   geom_point(size = 10, alpha = 0.75) +
   labs(y = "Total Aq. Diptera Emergence",
        x = "Prior Year Low pH Days",
        shape = "Watershed",
        color = "Year") +
   scale_color_brewer(palette = "Dark2") +
   theme_bw() +
   theme(text = element_text(size = 20)))

##### Algae #####

# Plot emergence relationship versus chlorophyll a concentrations
(fig_chla <- ggplot(all_dat_trim,
                  aes(x = prev_chla, 
                      y = sum_total_count,
                      color = factor(year),
                      shape = factor(watershed))) +
   geom_point(size = 7) +
   labs(y = "Total Aq. Diptera Emergence",
        x = "Prior Year Mean Chlorophyll a",
        shape = "Watershed",
        color = "Year") +
   scale_color_brewer(palette = "Dark2") +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "right"))

##### Nitrate #####

# Plot emergence relationship versus nitrate concentrations
(fig_NO3 <- ggplot(all_dat_trim,
                    aes(x = prev_no3, 
                        y = sum_total_count,
                        color = factor(year),
                        shape = factor(watershed))) +
   geom_point(size = 7) +
   labs(y = "Total Aq. Diptera Emergence",
        x = "Prior Year Mean Nitrate",
        shape = "Watershed",
        color = "Year") +
   scale_color_brewer(palette = "Dark2") +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "none"))

##### Phosphate #####

# Plot emergence relationship versus nitrate concentrations
(fig_PO4 <- ggplot(all_dat_trim,
                   aes(x = prev_po4, 
                       y = sum_total_count,
                       color = factor(year),
                       shape = factor(watershed))) +
   geom_point(size = 7) +
   labs(y = "Total Aq. Diptera Emergence",
        x = "Prior Year Mean Phosphate",
        shape = "Watershed",
        color = "Year") +
   scale_color_brewer(palette = "Dark2") +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "none"))

(fig_chem <- fig_NO3 + fig_PO4 + fig_pH + fig_chla +
    plot_annotation(tag_levels = "A"))

# Export figures.
# ggsave(plot = fig_chem,
#        filename = "figures/sum_emerge_chem_100125.jpg",
#        width = 40,
#        height = 35,
#        units = "cm")

(fig_disc <- fig_low_alt + fig_pH +
    plot_annotation(tag_levels = "A"))

# Export figures.
# ggsave(plot = fig_disc,
#        filename = "figures/sum_emerge_low_pH_110525.jpg",
#        width = 40,
#        height = 17,
#        units = "cm")

#### Statistics ####

## 1 ## Annual emergence vs. low flow days the previous year
hist(all_dat_trim$prev_low_flow_days) # explored a log transformation
# and addition of small value to 0s so as not to create Infs/NaNs
# but residuals from lmem appear better with raw values
hist(all_dat_trim$sum_total_count) # appears roughly normally distributed

# Simple linear regression
annual.lm1 <- lm(sum_total_count ~ prev_low_flow_days, data = all_dat_trim)
summary(annual.lm1)
plot(annual.lm1) # again, looks alright considering low SS
# Re-fit with gls to compare better with lmem() function
annual.lm2 <- gls(sum_total_count ~ prev_low_flow_days, data = all_dat_trim)
# Fit multi-level model to account for site-level variation
annual.lm3 <- lme(sum_total_count ~ prev_low_flow_days,
                random = ~1|watershed,
                data = all_dat_trim %>%
                  mutate(watershed = factor(watershed)))
# Compare the two model structures
AIC(annual.lm2, annual.lm3) # identical
# Examine residuals
plot(annual.lm3)
qqnorm(annual.lm3)
# Summary of multi-level model 
summary(annual.lm3)

## 2 ## Annual emergence vs. low pH days the previous year
hist(all_dat_trim$prev_low_pH_days) # needs a log transformation
hist(log(all_dat_trim$prev_low_pH_days)) # alright
hist(all_dat_trim$sum_total_count) # appears roughly normally distributed

# Simple linear regression
annual2.lm1 <- lm(sum_total_count ~ log(prev_low_pH_days), data = all_dat_trim)
summary(annual2.lm1)
plot(annual2.lm1) # again, looks alright considering low SS
# Re-fit with gls to compare better with lmem() function
annual2.lm2 <- gls(sum_total_count ~ log(prev_low_pH_days), data = all_dat_trim)
# Fit multi-level model to account for site-level variation
annual2.lm3 <- lme(sum_total_count ~ log(prev_low_pH_days),
                  random = ~1|watershed,
                  data = all_dat_trim %>%
                    mutate(watershed = factor(watershed)))
# Compare the two model structures
AIC(annual2.lm2, annual2.lm3) # also identical
# Examine residuals
plot(annual2.lm3)
qqnorm(annual2.lm3)
# Summary of multi-level model 
summary(annual2.lm3)

# End of script.
