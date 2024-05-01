### Weir Data Exploration
### May 1, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF weir data.

# Data was downloaded on May 1, 2024 from 
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hbr&identifier=68.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(viridis)
library(patchwork)

# Load data.
dat <- read_csv("data_raw/sdmskgha.csv")

#### Plot ####

# Transform data long-ways.
dat_long <- dat %>%
  pivot_longer(cols = `WS-1`:`WS-8`) %>%
  # and remove NAs
  filter(value > 0)

(weir_plot <- ggplot(dat_long, aes(x = YEAR, y = value)) +
    geom_point(aes(color = name)) +
    geom_line(aes(color = name)) +
    labs(x = "Year", y = "Soil Material Lost (Kg/Ha)") +
    theme_bw() +
    scale_color_viridis(discrete = TRUE))

# 1975 is when collection in watersheds other than 2 and 6 started.
# W8 has been discontinued.
# As of 1999, looks like only W1, W5, and W6 are monitored.
# I wonder, have they changed the methods? Because 1970 was a banner
# year for W2, but no other site has come close since.

# Export figure.
# ggsave(plot = weir_plot,
#        filename = "figures/weir_sed_050124.jpg",
#        width = 20,
#        height = 10,
#        units = "cm")

# Don't immediately notice any long-term trends.

# High years - 1987, 1996
# Low years - 1983, 1988, 1992, 1995, 1997
    
# End of script.
  