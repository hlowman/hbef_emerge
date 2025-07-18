### Black Fly Data Summary
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data summarizing
# and visualization of the HBEF aquatic insect emergence data,
# focusing on only aquatic black flies.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)
library(lubridate)

# Load data.
dat <- readRDS("data_working/aquatic_counts_complete_yrs_071825.rds")

#### Tidy ####

# Summarize by order.
dat_order <- dat %>%
  group_by(watershed, year, Date, Order) %>%
  # note, this will sum across all traps collected in a given week
  summarize(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

# Trim data down to only black flies in watersheds 5 & 6.
dat_dipt <- dat_order %>%
  filter(Order == "dipteran") %>%
  filter(watershed %in% c(5,6)) %>%
  mutate(month = month(Date)) %>%
  # a quick eyeball suggests all peaks happen post October 1 (!)
  mutate(period = case_when(month < 10 ~ "early",
                            TRUE ~ "late"))

# Calculate total emergence
dat_dipt_sum <- dat_dipt %>%
  group_by(watershed, year, period) %>%
  summarize(sum_total_count = sum(replace_na(total_count, 0))) %>%
  ungroup()

# Summary statistics of annual emergence.
dat_dipt_stat <- dat_dipt_sum %>%
  group_by(watershed, period) %>%
  summarize(mean_total = mean(sum_total_count),
            sd_total = sd(sum_total_count)) %>%
  ungroup()

#### Plot ####

(fig1_dipt <- ggplot(dat_dipt, aes(x = Date, 
                                      y = total_count,
                                      color = period,
                                      group = year)) +
   geom_line(linewidth = 1) +
   scale_color_manual(values = c("black", "gray70")) +
   scale_x_continuous(
     breaks = seq.Date(as.Date("2018-01-01"), 
                       as.Date("2024-12-31"), 
                       by = "1 year"),
     labels = ~ format(.x, "%Y")) +
   labs(y = "Weekly Count of Simuliidae") +
   facet_grid(watershed~., 
              labeller = labeller(
                watershed = c('5'="Watershed 5",
                              '6'="Watershed 6"))) +
   theme_bw() +
   theme(text = element_text(size = 20),
         legend.position = "none"))

# Export figure.
# ggsave(plot = fig1_dipt,
#        filename = "figures/emerge_aq_dipt_070725.jpg",
#        width = 45,
#        height = 15,
#        units = "cm")

(fig2_dipt <- ggplot(dat_dipt_sum %>%
                       filter(period == "early"), 
                     aes(x = watershed, 
                         y = sum_total_count)) +
    geom_boxplot(linewidth = 0.75, width = 0.4) +
    geom_jitter(size = 7, shape = 21, 
                width = 0.2, alpha = 0.8,
                aes(fill = factor(year))) +
    labs(y = "Annual Count of Simuliidae",
         x = "Watershed",
         fill = "Year") +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 20)))

# Add to time series above.
(fig_dipt <- fig1_dipt + fig2_dipt +
    plot_layout(widths = c(5, 1)) +
    plot_annotation(tag_levels = "A"))

# Export figure.
# ggsave(plot = fig_dipt,
#        filename = "figures/emerge_dipt_071825.jpg",
#        width = 45,
#        height = 15,
#        units = "cm")

# End of script.
